(use-package company :delight :demand t)
(company-mode)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.2)
;; Darcula's company selection inherits `highlight`; force readable contrast.
(set-face-attribute 'company-tooltip-selection nil :foreground "#f8f8f2" :background "#4a4a4a")
(set-face-attribute 'company-tooltip-common nil :foreground "#f8f8f2" :background "#2f2f2f")
(set-face-attribute 'company-tooltip-common-selection nil :foreground "#f8f8f2" :background "#4a4a4a" :weight 'bold)
(set-face-attribute 'company-tooltip-search nil :foreground "#f8f8f2" :background "#4a4a4a")
(set-face-attribute 'company-tooltip-search-selection nil :foreground "#f8f8f2" :background "#4a4a4a")

(defvar-local my-company-idle-suspended nil)

(defun my-company-restore-idle-delay-after-change (&rest _)
  (when my-company-idle-suspended
    (kill-local-variable 'company-idle-delay)
    (setq my-company-idle-suspended nil)))

(defun my-company-mode-hook ()
  (add-hook 'after-change-functions 'my-company-restore-idle-delay-after-change nil t))

(add-hook 'company-mode-hook 'my-company-mode-hook)

(defun my-company-abort-and-suppress ()
  (interactive)
  ;; Suspend auto popup until this buffer text is edited again.
  (setq my-company-idle-suspended t)
  (setq-local company-idle-delay nil)
  (add-hook 'after-change-functions #'my-company-restore-idle-delay-after-change nil t)
  (company-abort)
  ;; Keep Company from immediately re-beginning in this command cycle.
  (setq this-command 'company-abort))

(keymap-set company-active-map "C-g" #'my-company-abort-and-suppress)
(keymap-set company-search-map "C-g" #'my-company-abort-and-suppress)
(keymap-set company-active-map "<escape>" #'my-company-abort-and-suppress)
(keymap-set company-search-map "<escape>" #'my-company-abort-and-suppress)

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
(setq consult-narrow-key ",")

(use-package
  vertico
  :demand t
  :config
    (require 'vertico-directory)

    (define-key vertico-directory-map (kbd "<backspace>") #'vertico-directory-delete-char)
    (define-key vertico-directory-map (kbd "M-<backspace>") #'vertico-directory-delete-word)
    (define-key vertico-directory-map (kbd "RET") #'vertico-directory-enter)

    (keymap-set vertico-map "C-M-n" #'vertico-next-group)
    (keymap-set vertico-map "C-M-p" #'vertico-previous-group)

    (setq vertico-preselect 'first)

    (setq vertico-multiform-categories
          '((file (:keymap . vertico-directory-map))))

    (vertico-mode)
    (vertico-multiform-mode)
)

(defun my-file-minibuffer-clear-before-home ()
  (when (and minibuffer-completing-file-name
             (eq this-command 'self-insert-command)
             (>= (point) (+ (minibuffer-prompt-end) 2))
             (string= (buffer-substring-no-properties (- (point) 2) (point)) "~/"))
    (delete-region (minibuffer-prompt-end) (- (point) 2))))

(defun my-file-minibuffer-setup ()
  (add-hook 'post-self-insert-hook #'my-file-minibuffer-clear-before-home nil t))

(add-hook 'minibuffer-setup-hook #'my-file-minibuffer-setup)

(with-eval-after-load 'xref
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))
