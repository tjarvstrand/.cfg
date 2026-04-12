(use-package rainbow-mode :demand t)

(require 'dash)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default fill-column 120)

(scroll-bar-mode -1)
(global-hl-line-mode 1)

(line-number-mode 1)
(column-number-mode 1)

(setq whitespace-style
      '(
        face ;; Use faces to highlight whitespace
        tabs ;; Detect/display tabs
        lines-tail ;; Highlight lines extending beyond whitespace-line-column or fill-column
        trailing ;; Highlight trailing whitespace
        tab-mark ;; Use a visible mark (not just face) to show tabs
        ))
(setq whitespace-line-column nil) ;; Use fill-column instead
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when buffer-file-name
              (whitespace-mode))))

;; Column marker to show when text crosses column 80
(require 'column-marker)

(add-hook 'find-file-hook (lambda () (interactive) (column-marker-3 fill-column)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(show-paren-mode t)

(use-package highlight-parentheses :demand t)
(setq highlight-parentheses-colors '("#ed8796" "#a6da95" "#8aadf4" "#f5a97f"))
(setq highlight-parentheses-delay 0.5)
(add-hook 'prog-mode-hook #'highlight-parentheses-mode)


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

(defun copy-buffer-filename ()
  "Copy the filename of the node at point."
  (interactive)
  (kill-new buffer-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames

(tool-bar-mode -1)
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;
;; Mode line

(use-package doom-modeline
  :ensure t
  :demand t
  :init (progn
          (setq doom-modeline-buffer-file-name-style 'file-name-with-project)
          (doom-modeline-mode 1)))

;;;;;;;;;;;;;;;;;;;;;
;; Misc

(fset 'yes-or-no-p ' y-or-n-p)
(put 'erase-buffer 'disabled nil)

(recentf-mode 1)
(setq recentf-max-saved-items 1000)
(setq inhibit-startup-screen t)
(setq comment-multi-line t)
(setq create-lockfiles nil)
(setq electric-indent-mode nil)

(delete-selection-mode  1)
(transient-mark-mode    1)

(global-auto-revert-mode)

(use-package syntax-subword)

(defun my-prog-mode-hook ()
  (syntax-subword-mode)
  (display-line-numbers-mode 1)
  (setq truncate-lines t)
)

(add-hook 'prog-mode-hook #'my-prog-mode-hook)

(add-hook 'minibuffer-setup-hook #'subword-mode)
