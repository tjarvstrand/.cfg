(require 'project)
(require 'treesit)

(use-package dape :demand t)
(use-package f :demand t)
(use-package yaml :demand t)

(add-to-list
 'treesit-language-source-alist
 '(dart "https://github.com/UserNobody14/tree-sitter-dart"))

(use-package
  dart-ts-mode
  :demand t
  :vc (:url "https://github.com/50ways2sayhard/dart-ts-mode.git" :branch main))

(unless (treesit-language-available-p 'dart)
  (treesit-install-language-grammar 'dart))

(defun my-dart--nearest-file (file &optional from-file)
  (when-let* ((file (or file buffer-file-name))
              (start (f-dirname (expand-file-name from-file)))
              (nearest (f-traverse-upwards (lambda (it) (f-exists? (f-join it file)))
                                           start)))
    (f-join nearest file)))

(defun my-dart--analysis-options-page-width (analysis-options-file)
  (let* ((yaml (yaml-parse-string (f-read analysis-options-file)))
         (formatter (gethash 'formatter yaml)))
    (and formatter (gethash 'page_width formatter))))

;; (defun my-dart-effective-page-width-or-nil (&optional file)
;;   (let* ((target (or file (buffer-file-name)))
;;          (analysis-options-file (my-dart--nearest-file "analysis_options.yaml" target))
;;          (parent* (f-dirname target)))
;;     (if analysis-options-file
;;         (if-let ((page-width (when analysis-options-file
;;                        (my-dart--analysis-options-page-width analysis-options-file))))
;;             page-width
;;           (unless (f-root? target)
;;             (my-dart-effective-page-width-or-nil parent)))
;;       (unless (f-root? target)
;;         (my-dart-effective-page-width-or-nil parent)))))

;; fixme
(defun my-dart-effective-page-width (&optional file)
  150
  ;;(or (my-dart-effective-page-width-or-nil file) 120)
  )


(add-to-list 'dape-configs
             '(dart
               ensure dape-ensure-command
               modes (dart-ts-mode)
               command "dart"
               command-args ("debug_adapter")
               command-cwd dape-command-cwd
               :type "dart"
               :cwd "."
               :program "lib/main.dart"
               ))

(defun my-dart--ensure-dart-parser ()
  (unless (treesit-parser-list nil 'dart)
    (treesit-parser-create 'dart)))

(defun my-dart-organize-imports ()
  (save-restriction
    (widen)
    (eglot-code-action-organize-imports (point-min) (point-max))))

(defun my-dart-before-save-hook ()
  (let ((deadline (+ (float-time) 1.0))
        done
        last-no-action-error)
    (while (and (not done)
                (< (float-time) deadline))
      (condition-case err
          (progn
            (my-dart-organize-imports)
            (setq done t))
        (error
         (if (string-match-p "No \"source.organizeImports\" code actions here"
                             (error-message-string err))
             (progn
               (setq last-no-action-error err)
               (accept-process-output nil 0.1))
           (signal (car err) (cdr err))))))
    (unless done
      (signal (car last-no-action-error) (cdr last-no-action-error)))
    (eglot-format-buffer)))

(defun my-dart--terminate-statement ()
  (interactive)
  (save-excursion
    (end-of-line)
    (when (eq (char-before) ?\;)
      ;; Delete the semicolon and reinsert to trigger indentation
      (delete-char -1))
    (let ((last-command-event ?\;))
      (call-interactively #'self-insert-command))))

(defun my-dart-mode-hook ()
  (company-mode)
  (eglot-ensure)
  (setq fill-column (my-dart-effective-page-width))
  (add-hook 'before-save-hook 'my-dart-before-save-hook nil t)
  )

(add-hook 'dart-ts-mode-hook 'my-dart-mode-hook)
