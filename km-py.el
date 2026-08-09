;;; km-py.el --- Misc utils for Python -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-py
;; Version: 0.2.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (project "0.11.1") (python "0.28") (eglot "1.17") (pyvenv "1.21"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; km-py.el is a utility library that enhances the Python development
;; experience in Emacs by integrating with several Python environment
;; management systems such as Poetry, Pipenv, virtualenv, and pip. It provides
;; automatic configuration of Python LSP servers, virtual environment activation,
;; and convenient shell-interaction functions, aiming to offer a seamless
;; workflow for Python projects within Emacs.

;; The library auto-detects project types, configures Python paths, and
;; ensures that proper virtual environments are activated. It also auto-generates
;; Pyright configuration files based on Poetry settings when needed.

;; Features include:
;; - Fresh-process execution that automatically chooses between direct file and
;;   package-aware `python -m' semantics.
;; - Project-session and buffer-local environment-variable and `PYTHONPATH'
;;   configuration without modifying Emacs's global process environment.
;; - Customization options for specifying LSP server arguments and Python
;;   shell commands to be advised with auto-start functionality.
;; - Commands for setting up the Python environment and integrating with
;;   the Emacs Eglot LSP client.
;; - Utility functions for working with Poetry, such as checking for the
;;   existence of executables in the virtual environment and generating Pyright
;;   configurations from the Poetry setup.
;; - The ability to automatically start a Python shell when executing
;;   certain shell-interactive commands.
;; - Hooks and autoload cookies are provided for easy activation and
;;   deactivation of the library's functionality.

;; This library is indispensable for Python developers who use Emacs as
;; their primary editor and desire a more integrated and automated Python
;; development setup.

;;; Usage:

;; After installing or loading the library into your Emacs setup, you can enable
;; the Python environment setup by calling `km-py-setup-enable'. To disable the
;; setup and all advised commands, use `km-py-setup-disable'.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'project)
(require 'python)
(require 'seq)
(require 'subr-x)
(require 'eglot)
(require 'pyvenv)

(defgroup km-py nil
  "Utilities for working with Python projects."
  :group 'python
  :prefix "km-py-")

(defgroup km-pyright nil
  "Pyright integration for `km-py'."
  :group 'km-py
  :prefix "km-py-")

(defcustom km-py-run-interpreter nil
  "Python interpreter used by `km-py-run' commands.

When nil, prefer a Python executable in the nearest virtual environment, then
`python-shell-interpreter', `python3', and `python'.  A relative filename is
resolved from the Python project root."
  :group 'km-py
  :type '(choice (const :tag "Detect automatically" nil)
                 (file :tag "Python interpreter")))

(defcustom km-py-run-environment nil
  "Environment variable overrides for Python processes.

Each element is a cons cell (NAME . VALUE).  The variables are applied only to
processes started by `km-py-run' commands or to new inferior Python shells; the
global Emacs `process-environment' is not modified.  This option may be set in
directory-local variables."
  :group 'km-py
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Value")))

(defcustom km-py-run-pythonpath nil
  "Additional import roots for Python processes.

Relative directories are resolved from the current Python project root.  These
entries are prepended to inherited `PYTHONPATH' entries and may also be set in
directory-local variables."
  :group 'km-py
  :type '(repeat (directory :tag "Import root")))

(defcustom km-py-run-save-buffer 'ask
  "Whether `km-py-run' commands save a modified source buffer.

When `ask', ask before saving.  When non-nil, save without asking.  When nil,
run the last saved version of the file."
  :group 'km-py
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Always save" t)
                 (const :tag "Run last saved version" nil)))

(defcustom km-py-run-redact-environment-regexp
  (regexp-opt '("PASSWORD" "PASSWD" "SECRET" "TOKEN" "API_KEY"
                "ACCESS_KEY" "PRIVATE_KEY") 'words)
  "Regexp matching environment names whose values should not be displayed.

This affects `km-py-run-describe-context' only.  It does not alter the value
passed to the Python process."
  :group 'km-py
  :type 'regexp)

(cl-defstruct (km-py-run-context
               (:constructor km-py-run-context-create))
  "Fully resolved settings for one Python invocation."
  root
  cwd
  interpreter
  mode
  target
  file
  arguments
  command
  environment
  environment-overrides
  pythonpath
  import-root)

(defvar km-py--project-run-settings (make-hash-table :test #'equal)
  "Session-local run settings indexed by canonical project root.")

(defvar km-py--last-run-contexts (make-hash-table :test #'equal)
  "Last successful run context indexed by canonical project root.")

(defvar km-py-run-arguments-history nil
  "Minibuffer history for Python program arguments.")

(defvar km-py-run-environment-name-history nil
  "Minibuffer history for environment variable names.")

(defvar km-py-run-environment-value-history nil
  "Minibuffer history for environment variable values.")

(defvar-local km-py--run-context nil
  "Run context associated with a `km-py' output buffer.")

(defvar-local km-py--shell-base-process-environment nil
  "Original `python-shell-process-environment' before km-py overrides.")

(defvar-local km-py--shell-base-extra-pythonpaths nil
  "Original `python-shell-extra-pythonpaths' before km-py additions.")

(defvar-local km-py--shell-context-captured nil
  "Non-nil after original inferior-shell settings have been captured.")

(defvar-local km-py--shell-context-signature nil
  "Signature of the environment desired for the current Python shell.")

(defvar-local km-py--shell-context-warning-signature nil
  "Last stale Python shell context signature reported to the user.")

(defcustom km-py-lsp-server-args '((poetry . ("poetry"
                                              "run"
                                              "pyright-langserver"
                                              "--stdio"))
                                   (pipenv . ("pipenv"
                                              "run"
                                              "pyright-langserver"
                                              "--stdio"))
                                   (pip . ("pyright-langserver"
                                           "--stdio"))
                                   (conda . ("conda" "run" "pyright-langserver"
                                             "--stdio"))
                                   (virtualenv . ("pyright-langserver"
                                                  "--stdio"))
                                   (setuptools . ("pyright-langserver"
                                                  "--stdio")))
  "List of arguments for Python LSP server based on environment.

A list of arguments to pass to the Python LSP server when starting it. The list
is an association list where each element specifies the command to run the LSP
server for a different Python environment management system.

Each element of the association list is a cons cell `(KEY . VALUE)`, where KEY
is a symbol representing the environment management system, and VALUE is a list
of strings representing the command and its arguments.

Supported environment management systems and their corresponding symbols are:

- `poetry`: For projects using Poetry.
- `pipenv`: For projects using Pipenv.
- `pip`: For projects using pip with a global Python installation.
- `virtualenv`: For projects using a virtual environment created with
  virtualenv.

The default values are set to run the `pyright-langserver` with the `--stdio`
argument, which is necessary for communication between the LSP client and
server. The commands are prefixed with the environment management system's run
command, if applicable (e.g., `poetry run`).

To customize for a specific environment, modify the list by associating the
desired symbol with a new list"
  :group 'km-pyright
  :type '(alist
          :key-type
          (radio :tag "Project type"
           (const poetry)
           (const pipenv)
           (const pip)
           (const conda)
           (const virtualenv)
           (const setuptools))
          :value-type
          (repeat
           (string :tag "Server arguments"))))

(defcustom km-py-project-markers-files '("Pipfile"
                                         "pyproject.toml"
                                         "requirements.txt"
                                         "setup.py"
                                         "setup.cfg"
                                         "environment.yml")
  "List of filenames used to identify Python project directories.

A list of filenames used to identify Python projects.

The default filenames are \"Pipfile\", \"pyproject.toml\",
\"requirements.txt\", \"setup.py\", \"setup.cfg\", and \"environment.yml\".

Each element in the list should be a string representing a filename
that is commonly found in the root directory of a Python project."
  :group 'km-py
  :type '(repeat string))

(defcustom km-py-commands-to-advice '(python-shell-send-string
                                      python-shell-send-statement
                                      python-shell-send-region
                                      python-shell-send-defun
                                      python-shell-send-buffer
                                      python-shell-send-file
                                      python-shell-switch-to-shell
                                      python-eldoc-at-point
                                      python-describe-at-point)
  "List of commands that should be adviced to ensure a Python shell is running.

A list of Python shell commands that will be advised with to start a Python
shell if not already running before executing command.

Each element in the list should be a function that corresponds to a command used
to interact with the Python shell. Custom functions can also be added to the
list by selecting the \"Custom function\" tag and specifying the function name.

To apply the advice, use the `km-py--advice-shell-commands' function. To remove
the advice, use the `km-py--unadvice-shell-commands' function."
  :group 'km-py
  :type '(repeat
          (choice
           (function-item python-shell-send-string)
           (function-item python-shell-send-statement)
           (function-item python-shell-send-region)
           (function-item python-shell-send-defun)
           (function-item python-shell-send-buffer)
           (function-item python-shell-send-file)
           (function-item python-shell-switch-to-shell)
           (function-item python-eldoc-at-point)
           (function-item python-describe-at-point)
           (function :tag "Custom function"))))

(defcustom km-py-commands-to-auto-show-shell-buffer '(python-shell-send-statement
                                                      python-shell-send-region
                                                      python-shell-send-defun
                                                      python-shell-send-buffer
                                                      python-shell-send-file)
  "List of Python shell commands to auto-display buffer.

A list of Python shell commands that trigger the automatic display of the Python
shell buffer when executed. The default commands are `python-shell-send-string',
`python-shell-send-statement', `python-shell-send-region',
`python-shell-send-defun', `python-shell-send-buffer', `python-shell-send-file',
and `python-describe-at-point'.

Each element in the list should be a function that, when called, is intended to
interact with the Python shell. Custom functions can also be added to the list
by selecting the \"Custom function\" option and specifying the function name."
  :group 'km-py
  :type '(repeat
          (choice
           (function-item python-shell-send-statement)
           (function-item python-shell-send-region)
           (function-item python-shell-send-defun)
           (function-item python-shell-send-buffer)
           (function-item python-shell-send-file)
           (function-item python-describe-at-point)
           (function :tag "Custom function"))))

(defcustom km-py-venv-names '(".env" "env" ".venv" "venv")
  "List of virtual environment directory names to search for.

A list of directory names that are considered potential Python virtual
environments.

Each element in the list is a string that represents a directory name to be
checked when searching for a Python virtual environment in the current path. The
search function looks for these directories at the current path and upwards,
stopping at the root directory or when a matching virtual environment is found."
  :group 'km-py
  :type '(repeat
          (string :tag "Venv directory name")))

(defun km-py--environment-alist-p (value)
  "Return non-nil when VALUE is a valid environment override alist."
  (and (listp value)
       (seq-every-p
        (lambda (entry)
          (and (consp entry)
               (stringp (car entry))
               (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" (car entry))
               (stringp (cdr entry))
               (not (string-match-p "\0" (cdr entry)))))
        value)))

(defun km-py--string-list-p (value)
  "Return non-nil when VALUE is a list containing only strings."
  (and (listp value) (seq-every-p #'stringp value)))

(put 'km-py-run-environment 'safe-local-variable
     #'km-py--environment-alist-p)
(put 'km-py-run-pythonpath 'safe-local-variable #'km-py--string-list-p)
(put 'km-py-run-interpreter 'safe-local-variable
     (lambda (value) (or (null value) (stringp value))))

(defun km-py--canonical-root (root)
  "Return a canonical directory name for ROOT."
  (when root
    (file-name-as-directory
     (if (file-exists-p root)
         (file-truename root)
       (expand-file-name root)))))

(defun km-py--project-setting (root property)
  "Return session setting PROPERTY for project ROOT."
  (plist-get (gethash (km-py--canonical-root root)
                      km-py--project-run-settings)
             property))

(defun km-py--set-project-setting (root property value)
  "Set session setting PROPERTY to VALUE for project ROOT."
  (let* ((key (km-py--canonical-root root))
         (settings (copy-sequence (gethash key km-py--project-run-settings))))
    (puthash key (plist-put settings property value)
             km-py--project-run-settings)
    value))

(defun km-py--alist-put (alist key value)
  "Return a copy of ALIST with KEY associated with VALUE."
  (let ((result (copy-tree alist)))
    (if-let* ((entry (assoc-string key result)))
        (setcdr entry value)
      (push (cons key value) result))
    result))

(defun km-py--alist-delete (alist key)
  "Return a copy of ALIST without entries whose key equals KEY."
  (seq-remove (lambda (entry) (string= key (car entry))) alist))

(defun km-py--merge-environment-alists (&rest alists)
  "Merge ALISTS from weakest to strongest precedence."
  (let (result)
    (dolist (alist alists result)
      (dolist (entry alist)
        (setq result (km-py--alist-put result (car entry) (cdr entry)))))))

(defun km-py--effective-run-environment (root)
  "Return effective environment overrides for project ROOT."
  (km-py--merge-environment-alists
   (default-value 'km-py-run-environment)
   (km-py--project-setting root :environment)
   (when (local-variable-p 'km-py-run-environment)
     km-py-run-environment)))

(defun km-py--delete-duplicates-by (items key-function)
  "Return ITEMS without duplicate values produced by KEY-FUNCTION."
  (let (keys result)
    (dolist (item items (nreverse result))
      (let ((key (funcall key-function item)))
        (unless (member key keys)
          (push key keys)
          (push item result))))))

(defun km-py--effective-run-pythonpath (root)
  "Return configured Python import roots for project ROOT.

Paths from buffer-local settings take precedence over project-session and
global settings.  Relative paths are resolved from ROOT."
  (let* ((local (when (local-variable-p 'km-py-run-pythonpath)
                  km-py-run-pythonpath))
         (paths (append local
                        (km-py--project-setting root :pythonpath)
                        (default-value 'km-py-run-pythonpath))))
    (km-py--delete-duplicates-by
     (mapcar (lambda (path)
               (let ((expanded
                      (directory-file-name
                       (expand-file-name path (or root default-directory)))))
                 (if (file-exists-p expanded)
                     (directory-file-name (file-truename expanded))
                   expanded)))
             paths)
     (lambda (path)
       (if (file-exists-p path) (file-truename path) path)))))

(defun km-py--environment-remove (name environment)
  "Return ENVIRONMENT without a variable named NAME."
  (let ((prefix (concat name "=")))
    (seq-remove (lambda (entry) (string-prefix-p prefix entry)) environment)))

(defun km-py--environment-set (name value environment)
  "Return ENVIRONMENT with NAME set to VALUE."
  (cons (concat name "=" value)
        (km-py--environment-remove name environment)))

(defun km-py--environment-get (name environment)
  "Return the value of NAME in ENVIRONMENT, or nil when absent."
  (let* ((prefix (concat name "="))
         (entry (seq-find (lambda (item) (string-prefix-p prefix item))
                          environment)))
    (when entry (substring entry (length prefix)))))

(defun km-py--apply-environment-overrides (environment overrides)
  "Apply OVERRIDES alist to a copy of ENVIRONMENT."
  (let ((result (copy-sequence environment)))
    (dolist (entry overrides result)
      (setq result (km-py--environment-set (car entry) (cdr entry) result)))))

(defun km-py--interpreter-venv (interpreter)
  "Return the virtual environment containing INTERPRETER, or nil."
  (let* ((bin-directory (file-name-directory interpreter))
         (candidate (and bin-directory
                         (file-name-directory
                          (directory-file-name bin-directory)))))
    (when (and candidate
               (file-exists-p (expand-file-name "pyvenv.cfg" candidate)))
      (directory-file-name candidate))))

(defun km-py--apply-interpreter-environment (environment interpreter)
  "Return ENVIRONMENT adjusted for INTERPRETER's virtual environment."
  (if-let* ((venv (km-py--interpreter-venv interpreter)))
      (let* ((bin (directory-file-name (file-name-directory interpreter)))
             (path (km-py--split-path-list
                    (km-py--environment-get "PATH" environment)))
             (path (km-py--delete-duplicates-by
                    (cons bin path)
                    (lambda (item)
                      (if (file-exists-p item) (file-truename item) item))))
             (result (km-py--environment-set "VIRTUAL_ENV" venv environment)))
        (km-py--environment-set "PATH" (km-py--join-path-list path) result))
    environment))

(defun km-py--path-separator-string ()
  "Return variable `path-separator' as a string."
  (if (characterp path-separator)
      (char-to-string path-separator)
    path-separator))

(defun km-py--split-path-list (value)
  "Split environment path VALUE using the platform path separator."
  (if (or (null value) (string-empty-p value))
      nil
    (split-string value (regexp-quote (km-py--path-separator-string)) t)))

(defun km-py--join-path-list (paths)
  "Join PATHS using the platform path separator."
  (mapconcat #'identity paths (km-py--path-separator-string)))

(defun km-py--python-identifier-p (value)
  "Return non-nil when VALUE can be a Python module-name component."
  (and (stringp value)
       (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" value)))

(defun km-py--package-directory-chain-p (import-root directories)
  "Return non-nil when DIRECTORIES below IMPORT-ROOT form a package chain."
  (and directories
       (let ((directory import-root)
             (valid t))
         (dolist (component directories valid)
           (setq directory (expand-file-name component directory))
           (unless (file-exists-p (expand-file-name "__init__.py" directory))
             (setq valid nil))))))

(defun km-py--module-candidates (root)
  "Return possible import roots for project ROOT.

Each result is a cons cell whose car is a directory and whose cdr says whether
the directory was explicitly configured, which permits namespace packages."
  (let* ((configured (km-py--effective-run-pythonpath root))
         (src (expand-file-name "src" root))
         (candidates
          (append (mapcar (lambda (path) (cons path t)) configured)
                  (when (file-directory-p src) (list (cons src nil)))
                  (list (cons (directory-file-name root) nil)))))
    (sort (km-py--delete-duplicates-by
           candidates
           (lambda (candidate)
             (let ((path (car candidate)))
               (if (file-exists-p path) (file-truename path) path))))
          (lambda (left right) (> (length (car left)) (length (car right)))))))

(defun km-py--module-info (file root)
  "Return module information for FILE in project ROOT, or nil.

The result is a plist with `:module', `:import-root', and `:kind'."
  (let* ((root (km-py--canonical-root root))
         (expanded-file (if (file-exists-p file)
                            (file-truename file)
                          (expand-file-name file)))
        result)
    (catch 'done
      (dolist (candidate (km-py--module-candidates root))
        (let ((import-root (file-name-as-directory (car candidate)))
              (explicit (cdr candidate)))
          (when (file-in-directory-p expanded-file import-root)
            (let* ((relative (file-relative-name expanded-file import-root))
                   (without-extension (file-name-sans-extension relative))
                   (components (split-string without-extension "/" t))
                   (filename (car (last components)))
                   (directories (butlast components))
                   (package-chain
                    (km-py--package-directory-chain-p import-root directories)))
              (when (and (string= (file-name-extension expanded-file) "py")
                         (seq-every-p #'km-py--python-identifier-p components)
                         (or package-chain
                             (and explicit directories)))
                (let ((kind 'module)
                      (module-components components))
                  (when (string= filename "__main__")
                    (setq kind 'package
                          module-components directories))
                  (when (and module-components
                             (seq-every-p #'km-py--python-identifier-p
                                          module-components))
                    (setq result
                          (list :module (string-join module-components ".")
                                :import-root (directory-file-name import-root)
                                :kind kind))
                    (throw 'done result))))))))
      result)))

(defun km-py--venv-python (venv)
  "Return the Python executable in VENV, or nil."
  (seq-find #'file-executable-p
            (mapcar (lambda (relative) (expand-file-name relative venv))
                    '("bin/python" "Scripts/python.exe" "Scripts/python"))))

(defun km-py--resolve-executable (program root)
  "Resolve PROGRAM as an executable, relative to ROOT when appropriate."
  (when (and program (not (string-empty-p program)))
    (cond ((file-name-absolute-p program)
           (and (file-executable-p program) program))
          ((string-match-p "[/\\\\]" program)
           (let ((expanded (expand-file-name program root)))
             (and (file-executable-p expanded) expanded)))
          ((executable-find program)))))

(defun km-py--run-interpreter (root)
  "Return the Python interpreter for project ROOT, or signal an error."
  (let* ((default-directory root)
         (venv (km-py-find-venv-path))
         (interpreter
          (if km-py-run-interpreter
              (or (km-py--resolve-executable km-py-run-interpreter root)
                  (user-error "km-py: Configured interpreter is not executable: %s"
                              km-py-run-interpreter))
            (or (and venv (km-py--venv-python venv))
                (km-py--resolve-executable python-shell-interpreter root)
                (executable-find "python3")
                (executable-find "python")))))
    (or interpreter
        (user-error "km-py: Cannot find a Python interpreter for %s" root))))

(defun km-py--context-pythonpath (root import-root environment)
  "Return merged Python paths for ROOT, IMPORT-ROOT, and ENVIRONMENT."
  (let ((src (expand-file-name "src" root)))
    (km-py--delete-duplicates-by
     (delq nil
           (append (km-py--effective-run-pythonpath root)
                   (list import-root)
                   (when (file-directory-p src)
                     (list (directory-file-name src)))
                   (list (directory-file-name root))
                   (km-py--split-path-list
                    (km-py--environment-get "PYTHONPATH" environment))))
     (lambda (path)
       (cond ((string-empty-p path) path)
             ((file-exists-p path) (file-truename path))
             (t path))))))

(defun km-py--command-string (arguments)
  "Return shell command string for argv list ARGUMENTS."
  (mapconcat #'shell-quote-argument arguments " "))

(defun km-py-resolve-run-context (&optional file mode arguments)
  "Resolve a Python run context for FILE, MODE, and ARGUMENTS.

MODE is one of `auto', `file', or `module'.  In `auto' mode, run package files
with Python's `-m' option and other files by filename.  ARGUMENTS is a list of
strings passed to the program after the target."
  (let* ((file (or file buffer-file-name
                   (user-error "km-py: Current buffer does not visit a file")))
         (expanded-file (if (file-exists-p file)
                            (file-truename file)
                          (expand-file-name file)))
         (root (km-py--canonical-root
                (or (km-py-project-root)
                    (file-name-directory expanded-file))))
         (requested-mode (or mode 'auto))
         (module-info (km-py--module-info expanded-file root))
         (resolved-mode
          (pcase requested-mode
            ('auto (if module-info 'module 'file))
            ((or 'file 'module) requested-mode)
            (_ (user-error "km-py: Unsupported run mode %S" requested-mode))))
         (_ (when (and (eq resolved-mode 'module) (null module-info))
              (user-error "km-py: Cannot derive a module name for %s"
                          expanded-file)))
         (interpreter (km-py--run-interpreter root))
         (overrides (km-py--effective-run-environment root))
         (environment
          (km-py--apply-environment-overrides
           (km-py--apply-interpreter-environment process-environment interpreter)
           overrides))
         (import-root (plist-get module-info :import-root))
         (pythonpath (km-py--context-pythonpath root import-root environment))
         (environment (if pythonpath
                          (km-py--environment-set
                           "PYTHONPATH" (km-py--join-path-list pythonpath)
                           environment)
                        environment))
         (target (if (eq resolved-mode 'module)
                     (plist-get module-info :module)
                   expanded-file))
         (arguments (or arguments nil))
         (command (append (list interpreter)
                          (when (eq resolved-mode 'module) (list "-m"))
                          (list target)
                          arguments)))
    (km-py-run-context-create
     :root root
     :cwd root
     :interpreter interpreter
     :mode resolved-mode
     :target target
     :file expanded-file
     :arguments arguments
     :command command
     :environment environment
     :environment-overrides overrides
     :pythonpath pythonpath
     :import-root import-root)))

(defun km-py-get-project-type (project-directory)
  "Determine Python project type based on files in PROJECT-DIRECTORY.

Argument PROJECT-DIRECTORY is a string specifying the path to the project
directory."
  (let ((default-directory (if (and project-directory
                                    (file-exists-p project-directory))
                               (expand-file-name
                                (file-name-as-directory
                                 project-directory))
                             default-directory)))
    (cond ((file-exists-p "Pipfile") 'pipenv)
          ((file-exists-p "environment.yml") 'conda)
          ((file-exists-p "pyproject.toml")
           (with-temp-buffer
             (insert-file-contents "pyproject.toml")
             (if (re-search-forward "\\[tool\\.poetry\\]" nil t)
                 'poetry
               'virtualenv)))
          ((or (file-exists-p "setup.py")
               (file-exists-p "setup.cfg"))
           'setuptools)
          ((file-exists-p "requirements.txt") 'pip)
          (t 'virtualenv))))

(defun km-py-pipenv-setup ()
  "Configure Python environment with Pipenv in Emacs."
  (require 'flymake)
  (when-let* ((proj (km-py-project-root)))
    (let ((venv-path (string-trim (shell-command-to-string "pipenv --venv"))))
      (when (and venv-path (file-directory-p venv-path))
        (pyvenv-activate venv-path)
        (setq-local python-shell-interpreter (concat venv-path "/bin/python")
                    python-shell-interpreter-args "-i"
                    python-interpreter (concat venv-path "/bin/python"))
        (when (executable-find "ruff")
          (setq-local python-flymake-command
                      (list "env" "-u"
                            "VIRTUAL_ENV"
                            "pipenv"
                            "run"
                            "ruff"
                            "--config"
                            (expand-file-name "pyproject.toml" proj)
                            "--quiet" "--stdin-filename=stdin" "-"))
          (when (bound-and-true-p flymake-mode)
            (flymake-mode -1))
          (add-hook 'flymake-diagnostic-functions #'python-flymake nil t)
          (flymake-mode 1))))))


(defvar-local km-py--poetry-project-root nil
  "Path to the current poetry project root.")

(defun km-py--poetry-find-project-root ()
  "Return the poetry project root if any."
  (or km-py--poetry-project-root
      (when-let* ((root (locate-dominating-file default-directory
                                                "pyproject.toml"))
                  (pyproject-contents
                   (with-temp-buffer
                     (insert-file-contents-literally (concat (file-name-as-directory
                                                              root)
                                                             "pyproject.toml"))
                     (buffer-string)))
                  (_ (string-match "^\\[tool\\.poetry\\]" pyproject-contents)))
        (setq km-py--poetry-project-root root))))


(defun km-py--poetry-get-virtualenv ()
  "Retrieve the virtual environment path using Poetry."
  (when-let* ((poetry (executable-find "poetry")))
    (let ((start)
          (end))
      (with-temp-buffer
        (when (zerop (call-process
                      "env"
                      nil
                      t
                      nil
                      "-u"
                      "VIRTUAL_ENV"
                      poetry
                      "env" "info"))
          (when (re-search-backward "^Virtualenv$" nil t 1)
            (catch 'done
              (while
                  (when (zerop (forward-line 1))
                    (not (looking-at "^\n")))
                (when (looking-at "^Path:")
                  (setq start (re-search-forward "Path:[\s\t]+" nil t 1))
                  (setq end (line-end-position))
                  (throw 'done (buffer-substring-no-properties start end)))))))))))


(defun km-py-poetry-setup ()
  "Configure Python environment with Poetry in Emacs."
  (require 'flymake)
  (when-let* ((proj (km-py--poetry-find-project-root)))
    (when-let* ((venv (km-py--poetry-get-virtualenv))
                (poetry-python-path (km-py-poetry-which-python)))
      (pyvenv-activate venv)
      (setq-local python-shell-interpreter poetry-python-path
                  python-shell-interpreter-args "-i"
                  python-interpreter poetry-python-path)
      (when (km-py-poetry-check-exec "ruff")
        (setq-local python-flymake-command
                    (list "env" "-u"
                          "VIRTUAL_ENV"
                          (executable-find "poetry")
                          "run"
                          "ruff"
                          "--config"
                          (expand-file-name "pyproject.toml" proj)
                          "--quiet" "--stdin-filename=stdin" "-"))
        (when (bound-and-true-p flymake-mode)
          (flymake-mode -1))
        (add-hook 'flymake-diagnostic-functions #'python-flymake nil t)
        (flymake-mode 1)))))

(defun km-py--poetry-write-pyright-config (&optional force)
  "Create or update Pyright config from Poetry environment.

Optional argument FORCE is a boolean indicating whether to overwrite an existing
Pyright configuration file. If nil, the file is not overwritten."
  (when-let* ((proj
               (km-py-project-root))
              (pyright-config (expand-file-name "pyrightconfig.json" proj))
              (config (km-py-poetry-get-pyright-config))
              (config-str
               (when (or force (not (file-exists-p pyright-config)))
                 (with-temp-buffer (insert
                                    (json-encode config))
                                   (json-pretty-print-buffer)
                                   (buffer-string)))))
    (write-region
     config-str
     nil pyright-config nil 0)))

;;;###autoload
(defun km-py-poetry-write-pyright-config (&optional force)
  "Create/update Pyright config from Poetry environment.

Optional argument FORCE is a boolean indicating whether to overwrite an existing
Pyright configuration file. If nil, the file is not overwritten."
  (interactive "P")
  (km-py--poetry-write-pyright-config force))

(defun km-py-eglot-update-or-insert-mode (symb value)
  "Update or add SYMB and VALUE to `eglot-server-programs'.

Argument SYMB is a symbol representing the mode to update or insert in
`eglot-server-programs'.

Argument VALUE is the new value to associate with SYMB in
`eglot-server-programs'."
  (let ((cell))
    (cond ((setq cell (assq symb eglot-server-programs))
           (setcdr cell value))
          ((when (listp symb)
             (setq cell (seq-find (lambda (it)
                                    (let ((name (car it)))
                                      (seq-find (lambda (s)
                                                  (if (listp name)
                                                      (memq s name)
                                                    (eq symb name)))
                                                symb)))
                                  eglot-server-programs)))
           (setcar cell symb)
           (setcdr cell value))
          ((setq cell (seq-find (lambda (it)
                                  (let ((name (car it)))
                                    (if (listp name)
                                        (memq symb
                                              name)
                                      (eq symb name))))
                                eglot-server-programs))
           (setcdr cell value))
          (t (message "km-py: `%s' not in `eglot-server-programs'" symb)))))

(defun km-py-poetry-check-exec (program)
  "Check if PROGRAM exists in Poetry's virtualenv bin directory.

Argument PROGRAM is the name of the executable to check within the Poetry
virtual environment."
  (when-let* ((venv (km-py--poetry-get-virtualenv)))
    (let ((file (expand-file-name (concat "bin/" program) venv)))
      (when (file-exists-p file)
        file))))

(defun km-py-poetry-which-python ()
  "Find the Python executable managed by Poetry."
  (let ((poetry (executable-find "poetry")))
    (with-temp-buffer
      (when (zerop (call-process
                    "env"
                    nil
                    t
                    nil
                    "-u"
                    "VIRTUAL_ENV"
                    poetry
                    "run"
                    "which"
                    "python"))
        (string-trim (buffer-string))))))

(defun km-py--poetry-call (&rest args)
  "Execute Poetry command with ARGS and return trimmed output string.

Remaining arguments ARGS are strings passed as command arguments to the
\"poetry\" executable."
  (let ((poetry (executable-find "poetry")))
    (with-temp-buffer
      (when (zerop (apply #'call-process
                          "env"
                          nil
                          t
                          nil
                          "-u"
                          "VIRTUAL_ENV"
                          poetry
                          args))
        (string-trim (buffer-string))))))



(defun km-py-find-venv-path ()
  "Find the nearest virtual environment directory from the current path."
  (let ((found)
        (directory default-directory))
    (while (and
            (not found)
            (not (string= "/" directory)))
      (setq found (when-let* ((name (seq-find
                                     (lambda (venv-name)
                                       (let ((venv-path
                                              (expand-file-name
                                               venv-name
                                               directory))
                                             (cands '("bin/activate"
                                                      "Scripts/activate"
                                                      "bin/activate.csh"
                                                      "bin/activate.fish")))
                                         (and (file-directory-p venv-path)
                                              (seq-find
                                               (lambda (file)
                                                 (file-exists-p
                                                  (expand-file-name
                                                   file
                                                   venv-path)))
                                               cands))))
                                     km-py-venv-names)))
                    (file-name-as-directory (expand-file-name name directory))))
      (setq directory (expand-file-name "../" directory)))
    found))

(defun km-py-setup ()
  "Configure Python environment based on project type."
  (let* ((curr-project-root (km-py-project-root))
         (type (km-py-get-project-type curr-project-root))
         (venv-path (km-py-find-venv-path)))
    (pcase type
      ((guard venv-path)
       (when-let* ((interpreter (km-py--venv-python venv-path)))
         (setq-local python-shell-interpreter interpreter
                     python-interpreter interpreter)))
      ('poetry
       ;; (km-py-poetry-setup)
       ))
    (make-local-variable 'eglot-stay-out-of)
    (add-to-list 'eglot-stay-out-of 'flymake-diagnostic-functions)
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (when-let* ((server-args (cdr (assq type km-py-lsp-server-args))))
      (setq-local eglot-server-programs (copy-tree eglot-server-programs))
      (km-py-eglot-update-or-insert-mode '(python-mode python-ts-mode)
                                         server-args))
    (eglot-ensure)))

(defun km-py-poetry-get-pyright-config ()
  "Generate Pyright config from Poetry environment."
  (when-let* ((python-path (km-py-poetry-which-python))
              (proj-name (file-name-parent-directory
                          (file-name-parent-directory python-path)))
              (venvPath (directory-file-name
                         (file-name-parent-directory proj-name)))
              (venv (file-name-nondirectory (directory-file-name
                                             proj-name))))
    `((pythonPath . ,python-path)
      (venvPath . ,venvPath)
      (venv . ,venv))))

(defun km-py-find-project-root (&optional directory)
  "Locate the root DIRECTORY with `km-py-project-markers-files'.

Optional argument DIRECTORY is the directory from which to start searching for
the project root. If not provided, `default-directory' is used."
  (unless directory (setq directory default-directory))
  (if-let* ((found (seq-find
                    (lambda (it)
                      (file-exists-p (expand-file-name it directory)))
                    km-py-project-markers-files)))
      (file-name-as-directory directory)
    (let ((parent (expand-file-name ".." directory)))
      (unless (or (string= parent directory)
                  (string= directory "")
                  (string= directory "/"))
        (km-py-find-project-root parent)))))

(defun km-py--project-root ()
  "Return the root directory of the current project."
  (when-let* ((project (ignore-errors (project-current))))
    (if (fboundp 'project-root)
        (project-root project)
      (with-no-warnings
        (car (project-roots project))))))

(defun km-py--find-python-project-markers ()
  "Return a list of Python project markers found in the current directory."
  (let ((fn (apply-partially #'locate-dominating-file default-directory)))
    (delete-dups (delq nil (mapcar fn km-py-project-markers-files)))))

(defun km-py-project-root ()
  "Find and return the root directory of the current Python project."
  (or
   (km-py-find-project-root)
   (km-py--project-root)))

(defun km-py--current-project-root-or-error ()
  "Return the current Python project root, or signal a user error."
  (or (km-py-project-root)
      (and buffer-file-name (file-name-directory buffer-file-name))
      (user-error "km-py: Cannot determine the current Python project root")))

(defun km-py--read-run-arguments (root)
  "Read Python program arguments for project ROOT."
  (let* ((previous (km-py--project-setting root :arguments))
         (initial (when previous (km-py--command-string previous)))
         (input (read-string "Program arguments: " initial
                             'km-py-run-arguments-history)))
    (if (string-empty-p input) nil (split-string-and-unquote input))))

(defun km-py--maybe-save-run-buffer ()
  "Save the current source buffer according to `km-py-run-save-buffer'."
  (when (and buffer-file-name (buffer-modified-p))
    (pcase km-py-run-save-buffer
      ('ask (when (y-or-n-p "Save buffer before running? ") (save-buffer)))
      ((pred identity) (save-buffer))))
  (when (and buffer-file-name (not (file-exists-p buffer-file-name)))
    (user-error "km-py: Save %s before running it" (buffer-name))))

(defun km-py--run-buffer-name (context)
  "Return a compilation buffer name for CONTEXT."
  (let* ((root (km-py-run-context-root context))
         (project-name
          (file-name-nondirectory (directory-file-name root)))
         (target (km-py-run-context-target context)))
    (format "*km-py:%s:%s*" project-name
            (if (eq (km-py-run-context-mode context) 'module)
                target
              (file-name-nondirectory target)))))

(defun km-py--start-run-context (context)
  "Start a compilation process described by CONTEXT."
  (let* ((default-directory (km-py-run-context-cwd context))
         (process-environment (km-py-run-context-environment context))
         (command (km-py--command-string
                   (km-py-run-context-command context)))
         (buffer-name (km-py--run-buffer-name context))
         (buffer
          (compilation-start command 'compilation-mode
                             (lambda (_mode) buffer-name))))
    (with-current-buffer buffer
      (setq-local km-py--run-context context))
    (puthash (km-py--canonical-root (km-py-run-context-root context))
             context km-py--last-run-contexts)
    buffer))

(defun km-py--run-current-file (mode arguments)
  "Run the current file with MODE and program ARGUMENTS."
  (km-py--maybe-save-run-buffer)
  (let ((context (km-py-resolve-run-context buffer-file-name mode arguments)))
    (when (string-suffix-p ".__init__" (km-py-run-context-target context))
      (message "km-py: Running __init__.py as a module may initialize its package twice"))
    (km-py--start-run-context context)))

;;;###autoload
(defun km-py-run-dwim (&optional edit)
  "Run the current Python file using automatically selected semantics.

Package files run with `python -m'; standalone files run by filename.  Reuse
program arguments last entered for the current project.  With prefix argument
EDIT, call `km-py-run' to edit the mode and arguments first."
  (interactive "P")
  (if edit
      (call-interactively #'km-py-run)
    (let* ((root (km-py--current-project-root-or-error))
           (arguments (km-py--project-setting root :arguments)))
      (km-py--run-current-file 'auto arguments))))

;;;###autoload
(defun km-py-run ()
  "Prompt for execution mode and arguments, then run the current Python file."
  (interactive)
  (let* ((root (km-py--current-project-root-or-error))
         (mode (intern
                (completing-read "Execution mode: "
                                 '("auto" "module" "file") nil t nil nil
                                 "auto")))
         (arguments (km-py--read-run-arguments root)))
    (km-py--set-project-setting root :arguments arguments)
    (km-py--run-current-file mode arguments)))

;;;###autoload
(defun km-py-run-file (&optional edit-arguments)
  "Run the current Python buffer directly by filename.

With prefix argument EDIT-ARGUMENTS, prompt for program arguments."
  (interactive "P")
  (let* ((root (km-py--current-project-root-or-error))
         (arguments (if edit-arguments
                        (km-py--read-run-arguments root)
                      (km-py--project-setting root :arguments))))
    (when edit-arguments
      (km-py--set-project-setting root :arguments arguments))
    (km-py--run-current-file 'file arguments)))

;;;###autoload
(defun km-py-run-module (&optional edit-arguments)
  "Run the current Python buffer as an importable module.

With prefix argument EDIT-ARGUMENTS, prompt for program arguments."
  (interactive "P")
  (let* ((root (km-py--current-project-root-or-error))
         (arguments (if edit-arguments
                        (km-py--read-run-arguments root)
                      (km-py--project-setting root :arguments))))
    (when edit-arguments
      (km-py--set-project-setting root :arguments arguments))
    (km-py--run-current-file 'module arguments)))

;;;###autoload
(defun km-py-run-repeat ()
  "Repeat the last `km-py-run' invocation for the current project."
  (interactive)
  (let* ((root (km-py--canonical-root
                (km-py--current-project-root-or-error)))
         (context (gethash root km-py--last-run-contexts)))
    (unless context
      (user-error "km-py: No previous run for %s" root))
    (km-py--maybe-save-run-buffer)
    (km-py--start-run-context context)))

(defun km-py--environment-names ()
  "Return environment variable names available for completion."
  (delete-dups
   (append (mapcar #'car
                   (km-py--effective-run-environment
                    (km-py--current-project-root-or-error)))
           (mapcar (lambda (entry)
                     (car (split-string entry "=")))
                   process-environment))))

;;;###autoload
(defun km-py-run-set-env (name value &optional buffer-local)
  "Set environment variable NAME to VALUE for Python runs.

By default, remember the setting for the current project and Emacs session.
With prefix argument BUFFER-LOCAL, set it only in the current buffer."
  (interactive
   (let* ((name (completing-read "Environment variable: "
                                 (km-py--environment-names) nil nil nil
                                 'km-py-run-environment-name-history))
          (root (km-py--current-project-root-or-error))
          (current (cdr (assoc-string
                         name (km-py--effective-run-environment root)))))
     (list name
           (read-string (format "%s value: " name) current
                        'km-py-run-environment-value-history)
           current-prefix-arg)))
  (unless (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" name)
    (user-error "km-py: Invalid environment variable name %S" name))
  (let ((root (km-py--current-project-root-or-error)))
    (if buffer-local
        (setq-local km-py-run-environment
                    (km-py--alist-put
                     (when (local-variable-p 'km-py-run-environment)
                       km-py-run-environment)
                     name value))
      (km-py--set-project-setting
       root :environment
       (km-py--alist-put (km-py--project-setting root :environment)
                         name value)))
    (message "km-py: Set %s for %s scope" name
             (if buffer-local "buffer" "project-session"))))

;;;###autoload
(defun km-py-run-unset-env (name &optional buffer-local)
  "Remove the configured environment override NAME.

By default, remove the current project-session override.  With prefix argument
BUFFER-LOCAL, remove the current buffer-local override instead."
  (interactive
   (list (completing-read
          "Remove environment override: "
          (mapcar #'car
                  (km-py--effective-run-environment
                   (km-py--current-project-root-or-error)))
          nil t nil 'km-py-run-environment-name-history)
         current-prefix-arg))
  (let ((root (km-py--current-project-root-or-error)))
    (if buffer-local
        (setq-local km-py-run-environment
                    (km-py--alist-delete
                     (when (local-variable-p 'km-py-run-environment)
                       km-py-run-environment)
                     name))
      (km-py--set-project-setting
       root :environment
       (km-py--alist-delete (km-py--project-setting root :environment) name)))
    (message "km-py: Removed %s from %s scope" name
             (if buffer-local "buffer" "project-session"))))

;;;###autoload
(defun km-py-run-add-pythonpath (directory &optional buffer-local)
  "Add DIRECTORY to `PYTHONPATH' for Python runs.

By default, remember it for the current project and Emacs session.  With prefix
argument BUFFER-LOCAL, add it only in the current buffer."
  (interactive
   (let ((root (km-py--current-project-root-or-error)))
     (list (read-directory-name "Add Python import root: " root nil t)
           current-prefix-arg)))
  (let* ((root (km-py--current-project-root-or-error))
         (path (directory-file-name (expand-file-name directory root))))
    (unless (file-directory-p path)
      (user-error "km-py: Not a directory: %s" path))
    (if buffer-local
        (setq-local km-py-run-pythonpath
                    (cons path
                          (delete path
                                  (when (local-variable-p 'km-py-run-pythonpath)
                                    km-py-run-pythonpath))))
      (km-py--set-project-setting
       root :pythonpath
       (cons path (delete path (km-py--project-setting root :pythonpath)))))
    (message "km-py: Added %s to %s PYTHONPATH" path
             (if buffer-local "buffer" "project-session"))))

;;;###autoload
(defun km-py-run-clear-project-settings ()
  "Clear all session-local run settings for the current Python project."
  (interactive)
  (let ((root (km-py--canonical-root
               (km-py--current-project-root-or-error))))
    (remhash root km-py--project-run-settings)
    (remhash root km-py--last-run-contexts)
    (message "km-py: Cleared project-session settings for %s" root)))

(defun km-py--display-environment-value (name value)
  "Return display representation of environment NAME and VALUE."
  (let ((case-fold-search t))
    (if (string-match-p km-py-run-redact-environment-regexp name)
        "<redacted>"
      (prin1-to-string value))))

;;;###autoload
(defun km-py-run-describe-context (&optional mode)
  "Display the resolved run context for the current buffer.

Optional MODE defaults to `auto'.  Sensitive environment values are redacted
according to `km-py-run-redact-environment-regexp'."
  (interactive)
  (let* ((context (km-py-resolve-run-context buffer-file-name
                                             (or mode 'auto)
                                             nil))
         (buffer (get-buffer-create "*km-py run context*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Project root:  %s\n" (km-py-run-context-root context))
                (format "Working dir:   %s\n" (km-py-run-context-cwd context))
                (format "Interpreter:   %s\n" (km-py-run-context-interpreter context))
                (format "Mode:          %s\n" (km-py-run-context-mode context))
                (format "Target:        %s\n" (km-py-run-context-target context))
                (format "Command:       %s\n"
                        (km-py--command-string
                         (km-py-run-context-command context)))
                "\nPYTHONPATH:\n")
        (dolist (path (km-py-run-context-pythonpath context))
          (insert (format "  %s\n" path)))
        (insert "\nEnvironment overrides:\n")
        (if-let* ((overrides
                   (km-py-run-context-environment-overrides context)))
            (dolist (entry overrides)
              (insert (format "  %s=%s\n" (car entry)
                              (km-py--display-environment-value
                               (car entry) (cdr entry)))))
          (insert "  <none>\n"))
        (special-mode)))
    (pop-to-buffer buffer)))


(defun km-py--shell-environment-strings (base overrides)
  "Return shell environment strings from BASE with OVERRIDES applied."
  (let ((result (copy-sequence base)))
    (dolist (entry overrides result)
      (setq result (km-py--environment-set (car entry) (cdr entry) result)))))

(defun km-py--shell-context-signature (root interpreter overrides paths)
  "Return a comparable shell signature for ROOT, INTERPRETER, OVERRIDES, PATHS."
  (list (km-py--canonical-root root) interpreter
        (sort (copy-tree overrides)
              (lambda (left right) (string< (car left) (car right))))
        paths))

(defun km-py-apply-shell-context ()
  "Apply current project run settings to subsequently created Python shells.

This function configures buffer-local Python shell variables.  It does not
modify a shell that is already running and does not mutate Emacs's global
process environment."
  (interactive)
  (let* ((root (km-py--canonical-root
                (km-py--current-project-root-or-error)))
         (interpreter (km-py--run-interpreter root))
         (overrides (km-py--effective-run-environment root))
         (environment
          (km-py--apply-environment-overrides
           (km-py--apply-interpreter-environment process-environment interpreter)
           overrides))
         (paths (km-py--context-pythonpath root nil environment))
         (shell-overrides
          (km-py--merge-environment-alists
           (when (km-py--interpreter-venv interpreter)
             (list (cons "VIRTUAL_ENV"
                         (km-py--environment-get "VIRTUAL_ENV" environment))
                   (cons "PATH" (km-py--environment-get "PATH" environment))))
           overrides)))
    (unless km-py--shell-context-captured
      (setq-local km-py--shell-base-extra-pythonpaths
                  (copy-sequence python-shell-extra-pythonpaths))
      (when (boundp 'python-shell-process-environment)
        (setq-local km-py--shell-base-process-environment
                    (copy-sequence python-shell-process-environment)))
      (setq-local km-py--shell-context-captured t))
    (setq-local python-shell-interpreter interpreter
                python-interpreter interpreter
                python-shell-extra-pythonpaths
                (km-py--delete-duplicates-by
                 (append paths km-py--shell-base-extra-pythonpaths)
                 (lambda (path)
                   (if (file-exists-p path) (file-truename path) path))))
    (when (boundp 'python-shell-process-environment)
      (setq-local python-shell-process-environment
                  (km-py--shell-environment-strings
                   km-py--shell-base-process-environment shell-overrides)))
    (setq-local km-py--shell-context-signature
                (km-py--shell-context-signature
                 root interpreter overrides python-shell-extra-pythonpaths))))

(defun km-py--mark-shell-context (process)
  "Record the current source buffer's context signature on PROCESS."
  (when (processp process)
    (process-put process 'km-py-shell-context-signature
                 km-py--shell-context-signature)))

(defun km-py--warn-stale-shell-context (process)
  "Warn once when PROCESS does not match the desired shell context."
  (let ((running (process-get process 'km-py-shell-context-signature)))
    (when (and running
               (not (equal running km-py--shell-context-signature))
               (not (equal km-py--shell-context-warning-signature
                           km-py--shell-context-signature)))
      (setq-local km-py--shell-context-warning-signature
                  km-py--shell-context-signature)
      (message "km-py: Python settings changed; restart the shell to apply them"))))

(defun km-py-run-shell (&rest _)
  "Start a project-configured Python shell if one is not already running."
  (km-py-apply-shell-context)
  (if-let* ((process (python-shell-get-process)))
      (km-py--warn-stale-shell-context process)
    (let ((current-prefix-arg '(4)))
      (when-let* ((process (call-interactively #'run-python)))
        (km-py--mark-shell-context process)))))

(defun km-py--advice-shell-commands ()
  "Add `km-py-run-shell' advice to Python shell commands.

Iterate over each command in a predefined list of Python shell commands.
Add a specified function as a before advice to each command in the list."
  (dolist (cmd km-py-commands-to-advice)
    (advice-add cmd
                :before #'km-py-run-shell)))

(defun km-py--unadvice-shell-commands ()
  "Remove `km-py-run-shell' advice from Python shell commands."
  (dolist (cmd km-py-commands-to-advice)
    (advice-remove cmd #'km-py-run-shell)))

(defun km-py--advice-shell-commands-to-pop-buffer ()
  "Allow auto displaying shell buffer after specific commands."
  (dolist (cmd km-py-commands-to-auto-show-shell-buffer)
    (advice-add cmd
                :after #'km-py--advice-show-shell-buffer)))

(defun km-py--advice-show-shell-buffer (&rest _)
  "Display Python shell buffer if not already visible."
  (when-let* ((buff (python-shell-get-buffer))
              (wnd (or (get-buffer-window buff)
                       (with-selected-window
                           (let ((wind (selected-window)))
                             (or
                              (window-right wind)
                              (window-left wind)
                              (progn (split-window-right) wind)))
                         (pop-to-buffer-same-window buff)
                         (selected-window)))))
    (with-selected-window wnd
      (goto-char (point-max))
      (set-window-point (selected-window)
                        (point-max)))))

(defun km-py--unadvice-shell-commands-to-pop-buffer ()
  "Remove advice from Python shell commands."
  (dolist (cmd km-py-commands-to-auto-show-shell-buffer)
    (advice-remove cmd #'km-py--advice-show-shell-buffer)))


(defun km-py-setup-python-path ()
  "Apply project import roots to `python-shell-extra-pythonpaths'."
  (km-py-apply-shell-context))

(defun km-py--run-in-buffer (buffer timer-sym fn &rest args)
  "Run a function FN in a BUFFER and cancel timer TIMER-SYM.

Argument TIMER-SYM is a symbol that represents a timer.
Argument BUFFER is the buffer in which the function/macro will be executed.
Argument FN is the function or macro that will be executed.
Argument ARGS is a list of additional arguments that will be passed to the FN."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (km-py--cancel-timer timer-sym)
      (let ((wnd (get-buffer-window buffer)))
        (if wnd
            (with-selected-window wnd
              (apply fn args))
          (apply fn args))))))

(defun km-py--cancel-timer (timer-sym)
  "Cancel a timer if it exists and set the value of TIMER-SYM to nil.

Argument TIMER-SYM is a symbol that represents the timer to be canceled."
  (when-let* ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value)
      (set timer-sym nil))))

(defun km-py--debounce (timer-sym delay fn &rest args)
  "Debounce execution FN with ARGS for DELAY.
TIMER-SYM is a symbol to use as a timer."
  (km-py--cancel-timer timer-sym)
  (set timer-sym (apply #'run-with-timer delay nil
                        #'km-py--run-in-buffer
                        (current-buffer)
                        timer-sym
                        fn
                        args)))

(defvar-local km-py--shell-timer nil)

(defvar km-py-send-file-code "__file__ = %s\n"
  "String template for updating `__file__' before evaluating Python code.")

(defun km-py--send-buffer (proc)
  "Send the current buffer's content to a live Python process PROC.

Argument PROC is a process object representing the Python subprocess."
  (when (process-live-p proc)
    (let ((setup-code
           (when buffer-file-name
             (format km-py-send-file-code (prin1-to-string buffer-file-name)))))
      (when setup-code
        (python-shell-send-string-no-output setup-code proc))
      (python-shell-send-buffer t))))


(defun km-py--run-python-send-buffer (&optional proc-wnd)
  "Run a Python process and send the current buffer's content to it.

Optional argument PROC-WND is a window object that, if live, will be
used to display the Python process buffer."
  (km-py-apply-shell-context)
  (when-let* ((wnd (selected-window))
              (new-proc (with-selected-window wnd
                          (run-python (python-shell-calculate-command)
                                      python-shell-dedicated
                                      (not proc-wnd))))
              (buff (process-buffer new-proc)))
    (km-py--mark-shell-context new-proc)
    (when (window-live-p proc-wnd)
      (with-selected-window proc-wnd
        (pop-to-buffer-same-window buff)))
    (km-py--debounce 'km-py--shell-timer
                     1
                     #'km-py--send-buffer
                     new-proc)))

;;;###autoload
(defun km-py-shell-send-buffer (&optional restart)
  "Send the current buffer's content to a Python process, optionally restarting.

Optional argument RESTART is a prefix argument that, when non-nil,
indicates that the Python process should be restarted before sending
the buffer."
  (interactive "P")
  (km-py-setup-python-path)
  (let* ((proc (python-shell-get-process))
         (proc-buff
          (when (process-live-p proc)
            (process-buffer proc))))
    (cond ((and proc-buff restart)
           (let ((proc-wnd
                  (get-buffer-window proc-buff)))
             (kill-buffer proc-buff)
             (km-py--run-python-send-buffer proc-wnd)))
          (proc (km-py--send-buffer proc))
          (t (km-py--run-python-send-buffer)))))

;;;###autoload
(defun km-py-shell-send-buffer-new (&optional no-restart)
  "Execute the current buffer's content by default in a new Python process.

Optional argument NO-RESTART is a prefix argument that, when non-nil,
indicates that the Python process should not be restarted before sending
the buffer."
  (interactive "P")
  (km-py-setup-python-path)
  (km-py-shell-send-buffer (not no-restart)))


;;;###autoload
(defun km-py-advice-shell-commands ()
  "Allow autostart a Python shell before some functions.

This functions is listed in `km-py-commands-to-advice' and will be adviced to
automically start a Python shell if not already running before executing
command."
  (interactive)
  (km-py--advice-shell-commands))

;;;###autoload
(defun km-py-unadvice-shell-commands ()
  "Disable autostart a Python shell before some running `km-py-commands-to-advice'."
  (interactive)
  (km-py--unadvice-shell-commands))

;;;###autoload
(defun km-py-setup-enable ()
  "Activate Python environment setup on Python mode hook."
  (interactive)
  (add-hook 'python-base-mode-hook #'km-py-setup)
  (km-py--advice-shell-commands)
  (km-py--advice-shell-commands-to-pop-buffer))

;;;###autoload
(defun km-py-setup-disable ()
  "Disable Python setup and unadvise shell commands."
  (interactive)
  (remove-hook 'python-base-mode-hook #'km-py-setup)
  (km-py--unadvice-shell-commands)
  (km-py--unadvice-shell-commands-to-pop-buffer))

;;;###autoload
(defun km-py-eglot-reconnect ()
  "Reconnect to the Eglot server and reopen the buffer at the same position."
  (interactive)
  (let ((file buffer-file-name)
        (buffer (current-buffer))
        (pos (point))
        (server (condition-case nil
                    (eglot--current-server-or-lose)
                  (error nil))))
    (when server
      (ignore-errors (eglot-reconnect server t)))
    (when (and file
               (or (not (buffer-modified-p buffer))
                   (and (yes-or-no-p "Save buffer?")
                        (progn (save-buffer) t))))
      (kill-buffer buffer)
      (find-file file)
      (goto-char pos))
    (when server
      (eglot-ensure))))

(defcustom km-py-indent-first-line-keywords '("class" "def" "if" "else" "elif"
                                              "for" "while" "try" "except"
                                              "finally" "async" "match" "case"
                                              "with"
                                              "@classmethod"
                                              "@staticmethod"
                                              "@property"
                                              "@lru_cache"
                                              "@wraps"
                                              "@cache"
                                              "@cached_property"
                                              "@contextmanager"
                                              "@abstractmethod")
  "List of Python keywords that initiate an indented code block.

This variable holds a list of Python keywords that, when appearing at the
beginning of a line, indicate that the subsequent lines should be indented
relative to that line. These keywords are associated with code structures
that introduce a new block in Python syntax expect the following lines to be
indented by 4 spaces.

This list is utilized by indentation functions to determine if the first line
of pasted or inserted code should be indented automatically, ensuring
consistent code formatting according to Python's indentation rules.

When pasting code that begins with one of these keywords, the indentation
functions can automatically adjust the first line's indentation to match the
expected indentation level.

If you customize this variable to include additional keywords, the indentation
functions will recognize those as initiation points for indented blocks as well."
  :group 'km-py
  :type '(repeat string))

(defcustom km-py-yank-auto-indent-first-line t
  "Whether to allow indenting the first line of current kill.

When set to t, the first line of the yanked text will be automatically
indented according to the indentation level of subsequent lines in the command
`km-py-yank'.

If set to nil, the first line will retain its original indentation."
  :group 'km-py
  :type 'boolean)

(defun km-py--dedent-text (str)
  "Remove leading indentation from the given string STR.

Argument STR is the string from which to trim leading indentation."
  (or (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (when (re-search-forward "[^\s\t\n\r\f]" nil t 1)
          (forward-char -1)
          (let* ((spaces (buffer-substring-no-properties
                          (line-beginning-position)
                          (point)))
                 (re
                  (unless (string-empty-p spaces)
                    (concat "^" spaces)))
                 (len (length spaces)))
            (when re
              (goto-char (point-min))
              (while (progn
                       (when (looking-at re)
                         (delete-region (point)
                                        (+ (point)
                                           len)))
                       (zerop (forward-line 1))))
              (buffer-string)))))
      str))

(defun km-py--ensure-first-line-indent (str)
  "Ensure the first line of STR is indented based on subsequent lines.

STR is the string whose first line's indentation will be adjusted to align with
the indentation level expected by its subsequent lines, enhancing code
consistency.

Argument STR is the string to ensure the first line is properly indented."
  (if (string-prefix-p " " str)
      str
    (let* ((lines (split-string str "\n" t))
           (first-line (pop lines))
           (last-line (car (last lines))))
      (let ((next-str (car lines))
            (next-indent
             (cond ((string-match-p
                     (concat "^" (regexp-opt
                                  km-py-indent-first-line-keywords
                                  'symbols))
                     first-line)
                    -4)
                   ((string-match-p "([\s]*$" first-line)
                    -4)
                   ((and last-line
                         (or
                          (and (string-match-p "{" first-line)
                               (not (string-match-p "}" first-line))
                               (string-match-p "}$"
                                               (string-trim-right last-line)))
                          (and (string-match-p "[[][\s]*$" first-line)
                               (string-match-p "\\]\\([\s]*\\)$" last-line))))
                    -4)
                   ((and (string-match-p "[[][\s]*$" first-line)
                         (when-let* ((last-line (car (last lines))))
                           (string-match-p "\\]\\([\s]*\\)$" last-line)))
                    -4)
                   (t 0))))
        (while (and next-str (string-match-p "^\s" next-str 0))
          (setq next-indent (1+ next-indent))
          (setq next-str (substring-no-properties next-str 1)))
        (if (> next-indent 0)
            (setq str (concat (make-string next-indent ?\s) str))
          str)))))


;;;###autoload
(defun km-py-yank (&optional arg)
  "Paste the current kill ring entry with adjusted indentation based on prefix.

The prefix argument ARG determines the behavior of the yank operation. See
`yank' command.

This command facilitates the insertion of previously killed (cut/copied) text
into a buffer, ensuring that the inserted text is correctly indented to match
the current point's indentation level.

The inserted text will have its leading indentation adjusted to match the
current line's indentation.

If the text to be inserted was indented, all lines will be reindented to match
the current line's leading spaces and tabs.

This command ensures that pasted content maintains logical structure by aligning
it with the surrounding code or text indentation.


For example, suppose the kill ring contains:

def example_function():
    print(\"Hello, World!\")

Running the command at a point with 4 spaces of indentation will insert:

    def example_function():
        print(\"Hello, World!\")."
  (interactive "*P")
  (when (and (region-active-p)
             (use-region-p))
    (delete-region (region-beginning)
                   (region-end)))
  (let ((prefix (buffer-substring-no-properties (line-beginning-position)
                                                (point))))
    (goto-char (line-beginning-position))
    (delete-region (point)
                   (line-end-position))
    (let* ((curr (current-kill
                  (cond ((listp arg) 0)
                        ((eq arg '-) -2)
                        (t (1- arg)))))
           (trimmed (km-py--dedent-text (if
                                            km-py-yank-auto-indent-first-line
                                            (km-py--ensure-first-line-indent
                                             curr)
                                          curr)))
           (empty-prefix (make-string (length prefix) ?\s)))
      (setq trimmed (seq-map-indexed
                     (lambda (line-str i)
                       (if (string-empty-p line-str)
                           line-str
                         (concat (if (> i 0) empty-prefix prefix)
                                 (string-trim-right
                                  line-str))))
                     (split-string trimmed "[\n\r\f]")))
      (insert (string-join trimmed "\n")))))


(defun km-py--double-quotes-to-single ()
  "Convert double quotes to single quotes in the current buffer."
  (save-excursion
    (goto-char (point))
    (with-undo-amalgamate
      (while (re-search-forward "[\"]" nil t 1)
        (cond ((looking-at "[\"][\"]")
               (forward-sexp 1)
               (when (looking-at "[\"]")
                 (forward-char 1)))
              ((looking-back "{\"" 0)
               (re-search-forward "\"}" nil t 1))
              (t
               (let* ((c-start (point))
                      (start (1- c-start))
                      (end (save-excursion
                             (forward-char -1)
                             (forward-sexp 1)
                             (point)))
                      (c-end (1- end))
                      (content (buffer-substring-no-properties c-start c-end)))
                 (delete-region start end)
                 (insert (concat "'" content "'")))))))))

;;;###autoload
(defun km-py-double-quotes-to-single ()
  "Convert double quotes to single quotes in the current buffer.
Convertation can be undone in a single step."
  (interactive)
  (km-py--double-quotes-to-single))

;;;###autoload
(defun km-py-copy-region-as-multi-line-string (beg end)
  "Copy selected region as a multi-line string with escaped newlines.

Argument BEG is the beginning position of the region to copy.

Argument END is the ending position of the region to copy."
  (interactive "r")
  (let* ((str (buffer-substring-no-properties beg end))
         (res (mapconcat
               (lambda (it)
                 (let ((item (prin1-to-string it)))
                   (with-temp-buffer
                     (erase-buffer)
                     (insert item)
                     (forward-char -1)
                     (insert "\\")
                     (insert "n")
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))
               (split-string str "\n")
               "\n")))
    (kill-new res)
    (message "Copied")
    res))

;;;###autoload
(defun km-py-remove-comments ()
  "Remove comments from Python code in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (with-undo-amalgamate
      (let ((case-fold-search t))
        (while (re-search-backward "#" nil t 1)
          (unless (or (save-excursion
                        (nth 3 (syntax-ppss (1+ (point)))))
                      (looking-at "# \\(noqa\\|type\\): [a-z0-9]+"))
            (let ((end (line-end-position))
                  (start))
              (skip-chars-backward "\s")
              (setq start (cond ((looking-back "\n" 1)
                                 (forward-char -1)
                                 (skip-chars-backward "\s")
                                 (point))
                                (t
                                 (point))))
              (delete-region start end))))))))

;;;###autoload
(defun km-py-guess-yas-snippet-symbol (&optional fallback-symbol items)
  "Return a matched symbol from ITEMS or FALLBACK-SYMBOL if none found.

Optional argument FALLBACK-SYMBOL is a string used when no match is found.

Optional argument ITEMS is a list of strings to search for a matching pattern.
If nil, the kill ring is used."
  (if-let* ((found (seq-find
                    (apply-partially #'string-match-p
                                     "^\\(self[\\.]\\)?[a-z0-9\\_]+$")
                    (or items kill-ring))))
      (substring-no-properties found)
    (format "%s" (or fallback-symbol ""))))


(provide 'km-py)
;;; km-py.el ends here
