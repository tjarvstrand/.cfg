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

;; Scratch buffer
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      "# This buffer is for notes you don't want to save.\n\n")
(add-hook 'after-init-hook 'global-mise-mode)


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
  (buffer-local-value 'buffer-file-name buffer))

(setq display-buffer-alist
      (list
       '("\\*Help\\*"
         (display-buffer-in-side-window )
         (side . right)
         (slot . 0)
         (dedicated . t)
         )
       '("\\*Messages\\*"
         (display-buffer-in-side-window display-buffer-pop-up-window)
         (slot . 0)
         (dedicated . t))
       '("\\*Backtrace\\*"
         (display-buffer-in-side-window display-buffer-pop-up-window)
         (slot . 1)
         (dedicated . t))
       '("\\*compilation\\*"
         (display-buffer-at-bottom)
         (slot . 0)
         (window-height . 0.3))
       `(is-file-buffer-p
         (display-buffer-reuse-window
          display-buffer-in-previous-window
          display-buffer-use-some-window
          display-buffer-no-window) ; never create a new window
         (inhibit-same-window . t)
         (reusable-frames . visible)
         (window-predicate . ,(lambda (w)
                                (not (window-parameter w 'window-side)))))
      ))

(setq help-window-select t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames

(tool-bar-mode -1)
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;
;; Misc

(fset 'yes-or-no-p ' y-or-n-p)
(put 'erase-buffer 'disabled nil)

(recentf-mode 1)
(setq inhibit-startup-screen t)
(setq comment-multi-line t)
(setq create-lockfiles nil)
(setq electric-indent-mode nil)
(setq graphviz-dot-dot-program "/opt/homebrew/bin/dot")
(setq js-indent-level 2)

(delete-selection-mode  1)
(transient-mark-mode    1)

(global-auto-revert-mode)


