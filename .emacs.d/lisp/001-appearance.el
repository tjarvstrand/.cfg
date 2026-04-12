(use-package catppuccin-theme)

(setq catppuccin-flavor 'macchiato)
(setq catppuccin-highlight-matches nil)
(setq catppuccin-dark-line-numbers-background t)
(setq catppuccin-italic-comments t)
(load-theme 'catppuccin t)
(set-face-attribute 'highlight nil :background "#3d3e74")
(set-face-attribute 'show-paren-match nil :foreground "firebrick1")

(use-package nerd-icons
  :demand t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;;:config (nerd-icons-install-fonts)
  )



(let ((font "JetBrainsMono Nerd Font 12"))
  (condition-case
      err
      (set-frame-font font nil t)
      (error
       (message "Failed to set font \"%s\": %s" font (error-message-string err)))))

(set-fringe-mode '(4 . 0))

;; Set font
(defun set-font-size (size)
  (interactive "nSize: ")
  (set-face-attribute 'default nil :height (* size 10)))

(defun font-size ()
  (/ (face-attribute 'default :height) 10))

(defun inc-font-size ()
  (interactive)
  (change-font-size 1))

(defun dec-font-size ()
  (interactive)
  (change-font-size -1))

(defun change-font-size (change)
  (set-font-size (+ (font-size) change)))

(defun frame-monitor-pixel-density ()
  (let* ((attrs  (frame-monitor-attributes))
         (mm     (apply '* (cdr (assoc 'mm-size attrs))))
         (pixels (apply '* (cdddr (assoc 'geometry attrs)))))
    (/ pixels mm)))

(defun auto-set-font-size ()
  (interactive)
  (condition-case nil
      (if (> (frame-monitor-pixel-density) 40)
          (set-font-size 14)
        (set-font-size 12))
    (error nil)))

(auto-set-font-size)

(global-font-lock-mode t)
