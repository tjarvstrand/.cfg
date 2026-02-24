;; Python mode
(use-package python-mode)
(require 'python-mode)
(use-package python-pytest)


(add-to-list 'project-vc-extra-root-markers ".venv")
(add-to-list 'project-vc-extra-root-markers "pyproject.toml")

(add-to-list 'auto-mode-alist
             '("\\(/BUILD\\|/WORKSPACE\\|\\.bzl\\)$" . python-mode))

;; From https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "check"
            "--output-format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode))

(defun my-python-mode-hook ()
  (python-ts-mode)
  ;; Avoid custom python sexp navigation behaviour from python-nav-forward-sexp
  (setq forward-sexp-function nil)
  (setq-local
   eglot-server-programs
   `(
     ((python-mode python-ts-mode) . ("pylsp"))
     )
   )
  (eglot-ensure)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)
