(setq compilation-ask-about-save nil)
(setq compilation-scroll-output t)

(require 'ansi-color)

(defun my/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)

(defun my/compilation-mode-hook ()
  (setq show-trailing-whitespace nil))

(add-hook 'compilation-mode-hook #'my/compilation-mode-hook)


