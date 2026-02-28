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
(setq consult-narrow-key ",")

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

(defun my-file-minibuffer-clear-before-home ()
  (when (and minibuffer-completing-file-name
             (eq this-command 'self-insert-command)
             (>= (point) (+ (minibuffer-prompt-end) 2))
             (string= (buffer-substring-no-properties (- (point) 2) (point)) "~/"))
    (delete-region (minibuffer-prompt-end) (- (point) 2))))

(defun my-file-minibuffer-setup ()
  (add-hook 'post-self-insert-hook #'my-file-minibuffer-clear-before-home nil t))

(add-hook 'minibuffer-setup-hook #'my-file-minibuffer-setup)
