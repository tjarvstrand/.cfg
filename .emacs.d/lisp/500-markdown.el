(use-package markdown-mode)


(with-eval-after-load 'yaml-mode
  (setq markdown-command "pandoc")
  (setq markdown-split-window-direction 'right)
  (keymap-unset markdown-mode-map "M-p" t)
  (keymap-unset markdown-mode-map "M-n" t))

(defun my-markdown-mode-hook ()
  (display-line-numbers-mode 1))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

