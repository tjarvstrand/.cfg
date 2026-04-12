(use-package yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(defun my-yaml-mode-hook ()
  (display-line-numbers-mode 1)
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
)

(add-hook 'yaml-mode-hook 'my-yaml-mode-hook)
