;;; km-py-tests.el --- Tests for km-py -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'km-py)

(defmacro km-py-test--with-project (binding &rest body)
  "Create a temporary Python project according to BINDING and run BODY."
  (declare (indent 1) (debug ((symbolp) body)))
  (let ((root (car binding)))
    `(let* ((,root (file-name-as-directory
                    (make-temp-file "km-py-test-" t)))
            (default-directory ,root)
            (km-py--project-run-settings (make-hash-table :test #'equal))
            (km-py--last-run-contexts (make-hash-table :test #'equal)))
       (unwind-protect
           (progn
             (km-py-test--write-file ,root "pyproject.toml"
                                     "[build-system]\nrequires = []\n")
             ,@body)
         (delete-directory ,root t)))))

(defun km-py-test--write-file (root relative contents)
  "Below ROOT, write CONTENTS to RELATIVE and return its filename."
  (let ((file (expand-file-name relative root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert contents))
    file))

(ert-deftest km-py-module-info-root-layout ()
  (km-py-test--with-project (root)
    (km-py-test--write-file root "sample/__init__.py" "")
    (km-py-test--write-file root "sample/tools/__init__.py" "")
    (let* ((file (km-py-test--write-file root "sample/tools/run.py" ""))
           (info (km-py--module-info file root)))
      (should (equal (plist-get info :module) "sample.tools.run"))
      (should (equal (plist-get info :import-root)
                     (directory-file-name (file-truename root))))
      (should (eq (plist-get info :kind) 'module)))))

(ert-deftest km-py-module-info-src-layout-and-main ()
  (km-py-test--with-project (root)
    (km-py-test--write-file root "src/sample/__init__.py" "")
    (let* ((file (km-py-test--write-file root "src/sample/__main__.py" ""))
           (info (km-py--module-info file root)))
      (should (equal (plist-get info :module) "sample"))
      (should (equal (plist-get info :import-root)
                     (directory-file-name
                      (file-truename (expand-file-name "src" root)))))
      (should (eq (plist-get info :kind) 'package)))))

(ert-deftest km-py-module-info-rejects-standalone-file ()
  (km-py-test--with-project (root)
    (let ((file (km-py-test--write-file root "script.py" "")))
      (should-not (km-py--module-info file root)))))

(ert-deftest km-py-module-info-allows-configured-namespace-package ()
  (km-py-test--with-project (root)
    (let* ((namespace-root (expand-file-name "namespaces" root))
           (file (km-py-test--write-file
                  root "namespaces/acme/widgets/main.py" ""))
           (_ (km-py--set-project-setting
               root :pythonpath (list namespace-root)))
           (info (km-py--module-info file root)))
      (should (equal (plist-get info :module) "acme.widgets.main")))))

(ert-deftest km-py-environment-precedence-and-isolation ()
  (km-py-test--with-project (root)
    (let ((original-default (default-value 'km-py-run-environment))
          (original-process '("FOO=inherited" "UNCHANGED=yes" "PATH=/bin")))
      (unwind-protect
          (progn
            (setq-default km-py-run-environment
                          '(("FOO" . "global") ("GLOBAL" . "yes")))
            (km-py--set-project-setting
             root :environment '(("FOO" . "project") ("PROJECT" . "yes")))
            (with-temp-buffer
              (setq default-directory root
                    buffer-file-name
                    (km-py-test--write-file root "script.py" ""))
              (setq-local km-py-run-environment
                          '(("FOO" . "buffer") ("BUFFER" . "yes")))
              (let* ((process-environment (copy-sequence original-process))
                     (context (km-py-resolve-run-context nil 'file))
                     (environment (km-py-run-context-environment context)))
                (should (equal (km-py--environment-get "FOO" environment)
                               "buffer"))
                (should (equal (km-py--environment-get "GLOBAL" environment)
                               "yes"))
                (should (equal (km-py--environment-get "PROJECT" environment)
                               "yes"))
                (should (equal (km-py--environment-get "BUFFER" environment)
                               "yes"))
                (should (equal (km-py--environment-get "UNCHANGED" environment)
                               "yes"))
                (should (equal process-environment original-process)))))
        (setq-default km-py-run-environment original-default)))))

(ert-deftest km-py-project-settings-do-not-leak ()
  (km-py-test--with-project (first)
    (km-py-test--with-project (second)
      (km-py--set-project-setting first :environment '(("VALUE" . "first")))
      (km-py--set-project-setting second :environment '(("VALUE" . "second")))
      (should (equal (km-py--project-setting first :environment)
                     '(("VALUE" . "first"))))
      (should (equal (km-py--project-setting second :environment)
                     '(("VALUE" . "second")))))))

(ert-deftest km-py-context-selects-file-and-module-modes ()
  (km-py-test--with-project (root)
    (let ((python (or (executable-find "python3") (executable-find "python"))))
      (km-py-test--write-file root "sample/__init__.py" "")
      (let ((module-file (km-py-test--write-file root "sample/main.py" ""))
            (script-file (km-py-test--write-file root "script.py" ""))
            (km-py-run-interpreter python))
        (with-temp-buffer
          (setq default-directory root buffer-file-name module-file)
          (let ((context (km-py-resolve-run-context)))
            (should (eq (km-py-run-context-mode context) 'module))
            (should (equal (km-py-run-context-command context)
                           (list python "-m" "sample.main")))))
        (with-temp-buffer
          (setq default-directory root buffer-file-name script-file)
          (let ((context (km-py-resolve-run-context)))
            (should (eq (km-py-run-context-mode context) 'file))
            (should (equal (km-py-run-context-command context)
                           (list python (file-truename script-file))))))))))

(ert-deftest km-py-pythonpath-merges-configured-auto-and-inherited-roots ()
  (km-py-test--with-project (root)
    (let* ((configured (expand-file-name "vendor" root))
           (inherited (expand-file-name "inherited" root))
           (process-environment
            (list (concat "PYTHONPATH=" inherited) "PATH=/bin"))
           (km-py-run-pythonpath '("vendor")))
      (make-directory configured)
      (make-directory inherited)
      (let ((paths (km-py--context-pythonpath root nil process-environment)))
        (should (equal (car paths) (file-truename configured)))
        (should (member (directory-file-name root) paths))
        (should (member inherited paths))
        (should (= (length paths) (length (delete-dups (copy-sequence paths)))))))))

(ert-deftest km-py-module-run-supports-relative-import-and-environment ()
  (km-py-test--with-project (root)
    (let* ((python (or (executable-find "python3") (executable-find "python")))
           (km-py-run-interpreter python)
           (process-environment (copy-sequence process-environment))
           (file (km-py-test--write-file
                  root "sample/main.py"
                  (concat "from .value import VALUE\n"
                          "import os\n"
                          "print(VALUE, os.environ['KM_PY_TEST'])\n"))))
      (km-py-test--write-file root "sample/__init__.py" "")
      (km-py-test--write-file root "sample/value.py" "VALUE = 'relative-ok'\n")
      (with-temp-buffer
        (setq default-directory root buffer-file-name file)
        (setq-local km-py-run-environment '(("KM_PY_TEST" . "env-ok")))
        (let* ((context (km-py-resolve-run-context))
               (default-directory (km-py-run-context-cwd context))
               (process-environment (km-py-run-context-environment context))
               (status (apply #'process-file
                              (car (km-py-run-context-command context))
                              nil t nil
                              (cdr (km-py-run-context-command context)))))
          (should (zerop status))
          (should (equal (string-trim (buffer-string))
                         "relative-ok env-ok")))))))

(ert-deftest km-py-shell-context-is-buffer-local ()
  (km-py-test--with-project (root)
    (let ((python (or (executable-find "python3") (executable-find "python")))
          (original-process (copy-sequence
                             (default-value 'process-environment)))
          (original-exec-path (copy-sequence (default-value 'exec-path))))
      (with-temp-buffer
        (setq default-directory root
              buffer-file-name
              (km-py-test--write-file root "script.py" "")
              km-py-run-interpreter python)
        (setq-local km-py-run-environment '(("KM_PY_SHELL" . "yes")))
        (km-py-apply-shell-context)
        (should (local-variable-p 'python-shell-extra-pythonpaths))
        (should (member (directory-file-name (file-truename root))
                        python-shell-extra-pythonpaths))
        (when (boundp 'python-shell-process-environment)
          (should (member "KM_PY_SHELL=yes"
                          python-shell-process-environment)))
        (should (local-variable-p 'process-environment))
        (should (equal (km-py--environment-get
                        "KM_PY_SHELL" process-environment)
                       "yes")))
      (should (equal (default-value 'process-environment) original-process))
      (should (equal (default-value 'exec-path) original-exec-path)))))

(ert-deftest km-py-process-context-exposes-venv-to-eglot ()
  (km-py-test--with-project (root)
    (let* ((venv (expand-file-name ".venv" root))
           (bin (expand-file-name "bin" venv))
           (python (km-py-test--write-file
                    root ".venv/bin/python" "#!/bin/sh\nexit 0\n"))
           (pyright (km-py-test--write-file
                     root ".venv/bin/pyright-langserver"
                     "#!/bin/sh\nexit 0\n"))
           (original-process (copy-sequence
                              (default-value 'process-environment)))
           (original-exec-path (copy-sequence (default-value 'exec-path))))
      (km-py-test--write-file root ".venv/bin/activate" "")
      (km-py-test--write-file root ".venv/pyvenv.cfg" "home = /usr/bin\n")
      (set-file-modes python #o755)
      (set-file-modes pyright #o755)
      (with-temp-buffer
        (setq default-directory root
              buffer-file-name
              (km-py-test--write-file root "sample.py" ""))
        (km-py-apply-shell-context)
        (should (equal python-shell-interpreter (file-truename python)))
        (should (equal (km-py--environment-get
                        "VIRTUAL_ENV" process-environment)
                       (directory-file-name (file-truename venv))))
        (should (equal (car exec-path) (file-truename bin)))
        (should (equal (executable-find "pyright-langserver")
                       (file-truename pyright)))
        (should (member (directory-file-name (file-truename root))
                        (km-py--split-path-list
                         (km-py--environment-get
                          "PYTHONPATH" process-environment)))))
      (should (equal (default-value 'process-environment) original-process))
      (should (equal (default-value 'exec-path) original-exec-path)))))

(ert-deftest km-py-process-context-preserves-global-npm-pyright ()
  (km-py-test--with-project (root)
    (let* ((venv (expand-file-name ".venv" root))
           (venv-bin (expand-file-name "bin" venv))
           (global-bin (expand-file-name "global-node-bin" root))
           (python (km-py-test--write-file
                    root ".venv/bin/python" "#!/bin/sh\nexit 0\n"))
           (global-pyright (km-py-test--write-file
                            root "global-node-bin/pyright-langserver"
                            "#!/bin/sh\nexit 0\n")))
      (km-py-test--write-file root ".venv/bin/activate" "")
      (km-py-test--write-file root ".venv/pyvenv.cfg" "home = /usr/bin\n")
      (set-file-modes python #o755)
      (set-file-modes global-pyright #o755)
      (with-temp-buffer
        (setq default-directory root
              buffer-file-name
              (km-py-test--write-file root "sample.py" ""))
        (setq-local process-environment
                    (km-py--environment-set
                     "PATH" global-bin (copy-sequence process-environment))
                    exec-path (list global-bin))
        (km-py-apply-shell-context)
        (should (equal (car exec-path) (file-truename venv-bin)))
        (should (member (file-truename global-bin) exec-path))
        (should (equal (executable-find "pyright-langserver")
                       (file-truename global-pyright)))))))

(provide 'km-py-tests)
;;; km-py-tests.el ends here
