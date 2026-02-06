;;; full-config.el --- Full configuration for GUI Emacs

;;; Commentary:
;; This file contains heavy packages and GUI-specific configuration
;; that is only loaded when running Emacs in graphical mode.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heavy Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mise)
(use-package python-mode)
(use-package s)
(use-package web-mode)
(use-package yaml-mode)
(use-package diff-hl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot (LSP)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eglot)
(set-face-attribute 'eglot-highlight-symbol-face nil :background "#3d3e74")

(defun my-eglot-code-lens-at-point (&optional filter-fn)
  "Get code lens at point, optionally filtered by FILTER-FN.
  FILTER-FN should take a code lens and return non-nil if it matches.
  Returns (lens . command) or nil if not found."
  (when-let* ((server (eglot-current-server))
              (code-lenses (jsonrpc-request
                            server
                            :textDocument/codeLens
                            `(:textDocument ,(eglot--TextDocumentIdentifier))))
                (pos (eglot--pos-to-lsp-position))
                (line (plist-get pos :line))
                (lenses-at-line
                 (seq-filter
                  (lambda (lens)
                    (let* ((range (plist-get lens :range))
                           (start (plist-get range :start))
                           (lens-line (plist-get start :line)))
                      (= lens-line line)))
                  code-lenses)))
    (when-let ((matching-lens
                (if filter-fn
                    (seq-find filter-fn lenses-at-line)
                  (car lenses-at-line))))
      (cons matching-lens (plist-get matching-lens :command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq display-buffer-alist
      (append display-buffer-alist
              '(("\\*Treemacs"
                 (display-buffer-in-side-window)
                 (side . left)
                 (slot . 0)
                 (dedicated . t)
                 (window-parameters
                  (no-other-window . t)
                  (no-delete-other-windows . t))))))

(save-selected-window (treemacs))
(treemacs-filewatch-mode)
(treemacs-project-follow-mode)
(setq treemacs--project-follow-delay 0.5)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package run-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dape (Debugger)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  dape
  :demand t
  :bind
  (("C-c d d" . dape)
   ("C-c d b" . dape-breakpoint-toggle)
   ("C-c d n" . dape-next)
   ("C-c d s" . dape-step-in)
   ("C-c d c" . dape-continue)))

(defun my-dape (config-name overrides)
  (dape (dape--config-eval config-name overrides)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-Specific Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-library "my-dart")
(load-library "my-python")
(load-library "my-javascript")

;; YAML mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; show-point-mode for debugging
(load-library "show-point-mode")

;;; full-config.el ends here
