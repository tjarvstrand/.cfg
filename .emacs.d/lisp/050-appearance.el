(use-package darcula-theme)

(load-theme 'darcula t)
(set-face-attribute 'highlight nil :background "#3a3a3a")
;; Darcula's company selection inherits `highlight`; force readable contrast.
(set-face-attribute 'company-tooltip-selection nil :foreground "#f8f8f2" :background "#4a4a4a")
(set-face-attribute 'company-tooltip-common-selection nil :foreground "#f8f8f2" :background "#4a4a4a" :weight 'bold)
(set-face-inverse-video 'mode-line nil)
(set-face-inverse-video 'mode-line-inactive nil)
(set-face-attribute 'mode-line-inactive nil :foreground "grey80")

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
