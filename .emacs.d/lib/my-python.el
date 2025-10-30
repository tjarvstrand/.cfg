;; Python mode
(require 'python-mode)
;; (require 'flycheck)

(add-to-list 'project-vc-extra-root-markers ".venv")
(add-to-list 'project-vc-extra-root-markers "pyproject.toml")

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

;; ;; Use something adapted to your config to add `python-ruff' to `flycheck-checkers'
;; ;; This is an MVP example:
;; (setq python-mode-hook
;;       (list (defun my-python-hook ()
;;               )))

(defun my-python-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)
