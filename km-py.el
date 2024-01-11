;;; km-py.el --- Misc utils for Python -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-py
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (project "0.10.0") (python "0.28") (eglot "1.15") (pyvenv "1.21"))
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

(require 'project)
(require 'python)
(require 'eglot)
(require 'pyvenv)

(declare-function poetry-track-virtualenv "poetry")
(declare-function poetry-get-virtualenv "poetry")
(declare-function poetry-tracking-mode "poetry")
(declare-function poetry-find-project-root "poetry")


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
                                   (virtualenv . ("pyright-langserver"
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
           (const virtualenv))
          :value-type
          (repeat
           (string :tag "Server arguments"))))

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

(defcustom km-py-commands-to-auto-show-shell-buffer '(python-shell-send-string
                                                      python-shell-send-statement
                                                      python-shell-send-region
                                                      python-shell-send-defun
                                                      python-shell-send-buffer
                                                      python-shell-send-file
                                                      python-describe-at-point)
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
           (function-item python-shell-send-string)
           (function-item python-shell-send-statement)
           (function-item python-shell-send-region)
           (function-item python-shell-send-defun)
           (function-item python-shell-send-buffer)
           (function-item python-shell-send-file)
           (function-item python-describe-at-point)
           (function :tag "Custom function"))))

(defcustom km-py-venv-names '("env" ".venv" "venv")
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


(defun km-py-get-project-type (project-directory)
  "Determine Python project type based on files in PROJECT-DIRECTORY.

Argument PROJECT-DIRECTORY is a string specifying the path to the project
directory."
  (let ((default-directory (expand-file-name (file-name-as-directory
                                              project-directory))))
    (cond ((file-exists-p "Pipfile") 'pipenv)
          ((file-exists-p "environment.yml") 'conda)
          ((file-exists-p "pyproject.toml")
           (with-temp-buffer
             (insert-file-contents "pyproject.toml")
             (if (re-search-forward "\\[tool\\.poetry\\]" nil t) 'poetry
               'setuptools)))
          ((or (file-exists-p "setup.py")
               (file-exists-p "setup.cfg"))
           'setuptools)
          ((file-exists-p "requirements.txt") 'pip)
          (t 'virtualenv))))

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
  (km-py-poetry-write-pyright-config force))

(defun km-py-eglot-update-or-insert-mode (symb value)
  "Update or add SYMB and VALUE to `eglot-server-programs'.

Argument SYMB is a symbol representing the mode to update or insert in
`eglot-server-programs'.

Argument VALUE is the new value to associate with SYMB in
`eglot-server-programs'."
  (if-let ((cell (or (assq symb eglot-server-programs)
                     (seq-find (lambda (it)
                                 (let ((name (car it)))
                                   (if (listp name)
                                       (memq symb
                                             name)
                                     (eq symb name))))
                               eglot-server-programs))))
      (setcdr cell value)
    (add-to-list 'eglot-server-programs (cons symb value))))

(defun km-py-poetry-check-exec (program)
  "Check if PROGRAM exists in Poetry's virtualenv bin directory.

Argument PROGRAM is the name of the executable to check within the Poetry
virtual environment."
  (when-let ((venv (poetry-get-virtualenv)))
    (let ((file (concat venv "/bin/" program)))
      (when (file-exists-p file)
        file))))

(defun km-py-poetry-which-python ()
  "Find the Python executable managed by Poetry."
  (with-temp-buffer
    (when (zerop (call-process
                  "env"
                  nil
                  t
                  nil
                  "-u"
                  "VIRTUAL_ENV"
                  (executable-find "poetry")
                  "run"
                  "which"
                  "python"))
      (string-trim (buffer-string)))))

(defun km-py-poetry-setup ()
  "Configure Python environment with Poetry in Emacs."
  (require 'flymake)
  (when-let ((proj (poetry-find-project-root)))
    (unless (bound-and-true-p poetry-tracking-mode)
      (poetry-tracking-mode 1))
    (poetry-track-virtualenv)
    (when-let ((venv (poetry-get-virtualenv))
               (poetry-python-path (km-py-poetry-which-python)))
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

(defun km-py-find-venv-path ()
  "Find the nearest virtual environment directory from the current path."
  (let ((found)
        (directory default-directory))
    (while (and
            (not found)
            (not (string= "/" directory)))
      (setq found (when-let ((name (seq-find
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
  (when-let* ((curr-project-root (km-py-project-root))
              (type (km-py-get-project-type curr-project-root)))
    (pcase type
      ('poetry (km-py-poetry-setup))
      (_
       (when-let ((venv-path (km-py-find-venv-path)))
         (pyvenv-activate venv-path)
         (setq-local python-shell-interpreter
                     (or (executable-find "python3")
                         (executable-find "python")))
         (setq-local python-interpreter python-shell-interpreter))))
    (make-local-variable 'eglot-stay-out-of)
    (add-to-list 'eglot-stay-out-of 'flymake-diagnostic-functions)
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (when-let ((server-args (cdr (assq type km-py-lsp-server-args))))
      (km-py-eglot-update-or-insert-mode 'python-mode
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


(defun km-py-project-root ()
  "Find and return the root directory of the current Python project."
  (when-let ((project (ignore-errors (project-current))))
    (if (fboundp 'project-root)
        (project-root project)
      (with-no-warnings
        (car (project-roots project))))))


(defun km-py-run-shell (&rest _)
  "Start a Python shell if not already running."
  (unless (python-shell-get-process)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'run-python))))

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
  (when-let ((buff (python-shell-get-buffer)))
    (unless (get-buffer-window buff)
      (with-selected-window
          (let ((wind (selected-window)))
            (or
             (window-right wind)
             (window-left wind)
             (progn (split-window-sensibly) wind)))
        (pop-to-buffer-same-window buff)))))

(defun km-py--unadvice-shell-commands-to-pop-buffer ()
  "Remove advice from Python shell commands."
  (dolist (cmd km-py-commands-to-auto-show-shell-buffer)
    (advice-remove cmd #'km-py--advice-show-shell-buffer)))

;;;###autoload
(defun km-py-shell-send-buffer ()
  "Send buffer to Python shell and show process buffer."
  (interactive)
  (unless (python-shell-get-process)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'run-python)))
  (python-shell-send-buffer t)
  (when-let* ((proc (python-shell-get-process))
              (buff (and (process-live-p proc)
                         (process-buffer proc))))
    (unless (get-buffer-window buff)
      (with-selected-window
          (let ((wind (selected-window)))
            (or
             (window-right wind)
             (window-left wind)
             (progn (split-window-sensibly) wind)))
        (pop-to-buffer-same-window buff)))))

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



(provide 'km-py)
;;; km-py.el ends here