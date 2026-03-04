(require 'dash)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default fill-column 120)
(global-display-fill-column-indicator-mode 1)

(setq-default show-trailing-whitespace t)
(scroll-bar-mode -1)
(show-paren-mode t)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

(line-number-mode 1)
(column-number-mode 1)

(setq-default whitespace-mode 1)
(setq whitespace-style       (quote (face tabs tab-mark lines-tail)))
(setq whitespace-display-mappings '((tab-mark 9 [9655 9] [92 9])))

;; Column marker to show when text crosses column 80
(require 'column-marker)

(add-hook 'find-file-hook (lambda () (interactive) (column-marker-3 fill-column)))

(set-frame-parameter nil 'right-fringe 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(use-package mise :delight mise-mode :demand t)
(add-hook 'after-init-hook 'global-mise-mode)

;; Scratch buffer
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      "# This buffer is for notes you don't want to save.\n\n")

;; Messages buffer
(load-library "buffer-tail")
(toggle-buffer-tail "*Messages*" "on")
(setq message-log-max 10000)

;; Minibuffer
(setq max-mini-window-height 10)
(set-face-foreground 'minibuffer-prompt "white")
(set-face-bold-p 'minibuffer-prompt t)

(defun close-minibuffer (frame)
  "Aborts the minibuffer if focus moves to another window."
  (when (and (active-minibuffer-window)
             (not (minibuffer-window-active-p (selected-window))))
    (abort-recursive-edit)))

(add-hook 'window-selection-change-functions 'close-minibuffer)

;;;;;;;;;;;;;;;;;;;;;
;; Windows

(winner-mode 1)
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)

(defun is-file-buffer-p (buffer _action)
  (buffer-local-value 'buffer-file-name (get-buffer buffer)))

(defun my-display-buffer-window-picker (buffer alist)
  "Picks the most recently used window if it is not a dedicated or side window, otherwise picks the first "
  (let ((mru (get-mru-window nil nil nil 'current-frame)))
    (-first
     (lambda (w)
       (and
        (not (window-dedicated-p w))
        (not (window-parameter w 'window-side))))
    (if mru
        (cons mru (window-list))
      (window-list)))))

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
         (slot . 0))
        (nil
         (display-buffer-reuse-window
          display-buffer-in-previous-window
          my-display-buffer-window-picker)
         (reusable-frames . visible))))

(setq help-window-select t)

(defun my/xref-goto-obey-display-buffer-alist (orig &optional quit)
  (let ((xref--original-window nil)
        (xref--original-window-intent nil))
    (funcall orig quit)))

(advice-add 'xref-goto-xref :around #'my/xref-goto-obey-display-buffer-alist)

;; Fixme: not working
;; (defun my/help-xref-obey-display-buffer-alist (orig pos function args)
;;   "Ensure help xrefs follow `display-buffer-alist' placement rules."
;;   (let ((help-window-keep-selected nil)
;;         (display-buffer-overriding-action
;;          '((display-buffer-reuse-window
;;             my-display-buffer-window-picker)
;;            (inhibit-same-window . t)
;;            (reusable-frames . visible))))
;;     (funcall orig pos function args)))

;; (advice-add 'help-do-xref :around #'my/help-xref-obey-display-buffer-alist)

(setq split-window-height nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames

(tool-bar-mode -1)
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;
;; Misc

(fset 'yes-or-no-p ' y-or-n-p)
(put 'erase-buffer 'disabled nil)

(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq inhibit-startup-screen t)
(setq comment-multi-line t)
(setq create-lockfiles nil)
(setq electric-indent-mode nil)
(setq graphviz-dot-dot-program "/opt/homebrew/bin/dot")
(setq js-indent-level 2)

(delete-selection-mode  1)
(transient-mark-mode    1)

(global-auto-revert-mode)

(use-package syntax-subword)

(defun my-prog-mode-hook ()
  (syntax-subword-mode)
  (setq truncate-lines t)
)

(add-hook 'prog-mode-hook #'my-prog-mode-hook)
