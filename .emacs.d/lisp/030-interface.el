
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."

  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(setq-default indent-tabs-mode nil)
(setq ring-bell-function (lambda ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key [C-down-mouse-1] #'xref-find-definitions)))

(define-key key-translation-map (kbd "C-x M-a") (kbd "å"))
(define-key key-translation-map (kbd "C-x M-A") (kbd "Å"))
(define-key key-translation-map (kbd "C-x M-o") (kbd "ä"))
(define-key key-translation-map (kbd "C-x M-O") (kbd "Ä"))
(define-key key-translation-map (kbd "C-x M-u") (kbd "ö"))
(define-key key-translation-map (kbd "C-x M-U") (kbd "Ö"))

;; C-k at beginning of line kills the entire line including newline
(setq kill-whole-line 1)

(global-set-key (kbd "C-c p") 'symbol-next)
(global-set-key (kbd "C-c n") 'symbol-previous)

(global-set-key (kbd "C-c P") 'erl-print)
(global-set-key (kbd "C-c M-p") 'erl-print-res)

(global-set-key (kbd "C-S-Y") 'yank-and-inc)
(global-set-key (kbd "C-c C-c") 'comment-line)

(global-set-key (kbd "C-c i") 'insert-filename)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-e") 'eval-buffer)
(global-set-key (kbd "C-x +") 'inc-font-size)
(global-set-key (kbd "C-x -") 'dec-font-size)

;; rectangleMark
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)


(global-set-key (kbd "C-x M-g") (lambda () (interactive) (goto-long-line 81)))

(global-set-key (kbd "M-F") 'fullscreen-toggle)
(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key [s-left] 'windmove-left)     ; move to windnow to the left
(global-set-key [s-right] 'windmove-right)   ; move to window to the right
(global-set-key [s-up] 'windmove-up)         ; move window above
(global-set-key [s-down] 'windmove-down)     ; move window below
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)

;; (global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-x O") #'(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-tab>") #'(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "<C-iso-lefttab>") #'(lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


(global-set-key [f8] 'neotree-toggle)
(global-set-key [(shift f8)] 'neotree-show)



;;;;;;;;;;;;;;;;;;;;;
;; Completion

(use-package company :demand t)
(company-mode)
(setq company-minimum-prefix-length 1)

(use-package orderless
  :demand t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package consult :demand t)
(global-set-key (kbd "C-x b") 'consult-buffer)
(setq consult-preview-key nil)

(use-package
  vertico
  :demand t
  :config
    (require 'vertico-directory)

    (define-key vertico-directory-map (kbd "<backspace>") #'vertico-directory-delete-char)
    (define-key vertico-directory-map (kbd "M-<backspace>") #'vertico-directory-delete-word)
    (define-key vertico-directory-map (kbd "RET") #'vertico-directory-enter)

    (setq vertico-preselect 'first)

    (setq vertico-multiform-categories
          '((file (:keymap . vertico-directory-map))))

    (vertico-mode)
    (vertico-multiform-mode)
)

;;;;;;;;;;;;;;;;;;;;;
;; Buffers

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default fill-column 120)
(global-display-fill-column-indicator-mode 1)

(setq-default show-trailing-whitespace t)
(scroll-bar-mode -1)
(show-paren-mode t)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

(use-package diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

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
  (when (and (>= (recursion-depth) 1)
             (not (minibuffer-window-active-p (selected-window))))
    (abort-recursive-edit)))

(add-hook 'window-selection-change-functions 'close-minibuffer)

;;;;;;;;;;;;;;;;;;;;;
;; Windows

(winner-mode 1)
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)

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
      ))

(setq help-window-select t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames

(tool-bar-mode -1)
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comint

(setq comint-buffer-maximum-size 15000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp debugging

;(setq debug-on-error t)
(setq debugger-bury-or-kill 'append)
(load-library "show-point-mode")

;;;;;;;;;;;;;;;;;;;;;
;; Misc

(fset 'yes-or-no-p ' y-or-n-p)
(put 'erase-buffer 'disabled nil)

(recentf-mode 1)
(setq inhibit-startup-screen t)
(setq comment-multi-line t)
(setq create-lockfiles nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq electric-indent-mode nil)
(setq graphviz-dot-dot-program "/opt/homebrew/bin/dot")
(setq js-indent-level 2)

(delete-selection-mode  1)
(transient-mark-mode    1)

(global-auto-revert-mode)


