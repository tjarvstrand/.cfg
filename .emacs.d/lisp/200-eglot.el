(use-package eglot)
(require 'eglot)
(set-face-attribute 'eglot-highlight-symbol-face nil :background "#3d3e74")
(setq eglot-code-action-indications '(eldoc-hint))
(setq eglot-events-buffer-config '(:size 200000 :format full))
(setq eglot-report-progress nil)

(defun my-eglot-managed-mode-hook ()
  (eglot-inlay-hints-mode -1))

(add-hook 'eglot-managed-mode-hook 'my-eglot-managed-mode-hook)


(define-key eglot-mode-map (kbd "C-c e h") #'eglot-inlay-hints-mode)

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

