;;;;;;;;;;;;;;;;;;;;;
;; Windows

(defvar my-current-window nil)
(defvar my-previous-window nil)

(defun my-track-selected-window (_frame)
  "Track selected windows globally."
  (let ((now (selected-window)))
    (when (and (window-live-p now)
               (not (window-minibuffer-p now))
               (not (eq now my-current-window)))
      (setq my-previous-window my-current-window)
      (setq my-current-window now))))

(add-hook 'window-selection-change-functions #'my-track-selected-window)

(defun my-switch-to-previous-window ()
  "Switch to previously selected window."
  (interactive)
  (if (window-live-p my-previous-window)
      (let ((w my-previous-window))
        (select-window w))
    (user-error "No previous window")))


(defun my-select-window (&optional prefix)
  (interactive "P")
  (if prefix
      (my-switch-to-previous-window)
    (my-consult-window)))

(global-set-key (kbd "C-x C-o") 'my-select-window)

(winner-mode 1)
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)

(defun my-display-buffer-window-picker (buffer alist)
    (let ((mru (get-mru-window nil nil nil 'current-frame))
          (inhibit-same (cdr (assq 'inhibit-same-window alist))))
      (-first
       (lambda (w)
         (and
          (not (window-dedicated-p w))
          (not (window-parameter w 'window-side))
          (not (and inhibit-same (eq w (selected-window))))))
       (if mru
           (cons mru (window-list nil 'nomini))
         (window-list)))))

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-use-some-window)
        (reusable-frames . visible)
        (some-window . my-display-buffer-window-picker)
        ;;(inhibit-same-window . t)
))

(setq display-buffer-alist
      `(
        ("\\*Help\\*"
         (display-buffer-in-side-window )
         (side . right)
         (slot . 0)
         (dedicated . t)
         )
        ("\\*Messages\\*"
         (display-buffer-in-side-window display-buffer-pop-up-window)
         (slot . 0)
         (dedicated . t))
        ("\\*Backtrace\\*"
         (display-buffer-in-side-window display-buffer-pop-up-window)
         (slot . 1)
         (dedicated . t))
        ("\\*compilation\\*"
         (display-buffer-in-side-window)
         (slot . 0))))

(setq help-window-select t)

(defun my/xref-goto-obey-display-buffer-alist (orig &optional quit)
  (let ((xref--original-window nil)
        (xref--original-window-intent nil))
    (funcall orig quit)))

(advice-add 'xref-goto-xref :around #'my/xref-goto-obey-display-buffer-alist)

;; Fixme: not working?
(defun my/help-xref-obey-display-buffer-alist (orig pos function args)
  "Ensure help xrefs follow `display-buffer-alist' placement rules."
  (let ((help-window-keep-selected nil)
        (display-buffer-overriding-action
         '((display-buffer-reuse-window
            my-display-buffer-window-picker)
           (inhibit-same-window . t)
           (reusable-frames . visible))))
    (funcall orig pos function args)))

 (advice-add 'help-do-xref :around #'my/help-xref-obey-display-buffer-alist)

(setq split-window-height nil)
