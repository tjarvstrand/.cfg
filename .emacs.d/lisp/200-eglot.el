(use-package eglot :demand t)
(use-package dash :demand t)

(require 'xref)

(set-face-attribute 'eglot-highlight-symbol-face nil :background "#3d3e74")
(setq eglot-code-action-indications '(eldoc-hint))
(setq eglot-events-buffer-config '(:size 200000 :format full))
(setq eglot-report-progress nil)

(defun my-eglot-managed-mode-hook ()
  (eglot-inlay-hints-mode -1))

(add-hook 'eglot-managed-mode-hook 'my-eglot-managed-mode-hook)

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

(defvar my-eglot-event-listener-alist nil)

(cl-defun my-eglot-event-listener (conn origin &key kind message json &allow-other-keys)
  ;; Only Eglot server->client traffic
  (when (and (object-of-class-p conn 'eglot-lsp-server) (eq origin 'server))
    (let ((method (plist-get message :method)))
      ;(message "Eglot event: kind=%s method=%s" kind method)
      (dolist
          (listener my-eglot-event-listener-alist)
        (when (string= (car listener) method)
          (funcall (cdr listener) (plist-get message :params)))))))

(add-hook 'jsonrpc-event-hook #'my-eglot-event-listener)


(defun my/eglot-next-reference-in-file (&optional prefix)
  (interactive "P")
  (let* ((backend (or (xref-find-backend) (user-error "No xref backend here")))
         (id (or
              (and backend (xref-backend-identifier-at-point backend))
             (user-error "No symbol at point")))
         (file (or
                (and (buffer-file-name) (file-truename (buffer-file-name)))
                (user-error "Current buffer is not visiting a file")))
         (pos (point))
         (refs (xref-backend-references backend id))
         (positions
            (-sort
             (if prefix #'> #'<)
             (--map-when
              #'identity
              (when-let* ((loc (xref-item-location it))
                          (group (xref-location-group loc))
                          (m (xref-location-marker loc)))
                (when (equal (file-truename group) file)
                  (marker-position m)))
              refs)))
         (next (seq-find (lambda (p) (> p pos)) positions)))
    (cond
     (next
      (goto-char next))
     ((length< positions 2)
      (message "No other references in this file"))
     (t
      (goto-char (car positions))
      (message "Wrapped to first reference in file")
      ))))

(defun my-eglot-rename-and-save (newname)
  "Run `eglot-rename' and auto-save file buffers changed by the rename."
  (let ((before (make-hash-table :test 'eq)))
    (dolist (b (buffer-list))
      (puthash b (buffer-modified-tick b) before))
    (eglot-rename newname)
    (save-some-buffers
     t
     (lambda ()
       (and (buffer-file-name)
            (buffer-modified-p)
            (> (buffer-modified-tick)
               (gethash (current-buffer) before 0)))))))

(defun my-eglot-rename-prefill ()
  "Like `eglot-rename', but prefill minibuffer with symbol at point."
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
         (shown (or sym "unknown symbol"))
         (initial (or sym ""))
         (newname (read-string (format "Rename `%s' to: " shown)
                               initial nil initial)))
    (my-eglot-rename-and-save newname)))


;; (define-key eglot-mode-map (kbd "M-g n") 'my/eglot-next-reference-in-file)
(define-key eglot-mode-map (kbd "C-c e h") #'eglot-inlay-hints-mode)
(define-key eglot-mode-map (kbd "C-c e n") #'my-eglot-rename-prefill)
(define-key eglot-mode-map (kbd "M-RET") #'eglot-code-actions)
