(require 'project)

(use-package dart-mode :demand t)
(use-package dape :demand t)
(use-package f :demand t)
(use-package yaml :demand t)

(add-to-list
 'treesit-language-source-alist
 '(dart "https://github.com/UserNobody14/tree-sitter-dart")

(defun my-dart--nearest-analysis-options (&optional from-file)
  (when-let* ((file (or from-file buffer-file-name))
              (start (f-dirname (expand-file-name file)))
              (nearest (f--traverse-upwards (f-exists? (f-join it "analysis_options.yaml")) start)))
    (f-join nearest "analysis_options.yaml")))

(defun my-dart--analysis-options-page-width (analysis-options-file)
  (let* ((yaml (yaml-parse-string (f-read analysis-options-file)))
         (formatter (gethash 'formatter yaml)))
    (and formatter (gethash 'page_width formatter))))

(defun my-dart-effective-page-width (&optional file)
  (let* ((target (or file (buffer-file-name)))
         (analysis-options-file (my-dart--nearest-analysis-options target)))
    (or
     (when analysis-options-file (my-dart--analysis-options-page-width analysis-options-file))
     120)))


(add-to-list 'dape-configs
             '(dart
               ensure dape-ensure-command
               modes (dart-mode)
               command "dart"
               command-args ("debug_adapter")
               command-cwd dape-command-cwd
               :type "dart"
               :cwd "."
               :program "lib/main.dart"
               ))

(defun my-dart--call-encloses-pos-p (call-start pos)
  (save-excursion
    (goto-char call-start)
    (ignore-errors
      (forward-sexp 2)
      (> (point) pos))))

(defconst my-dart--test-call-re
  "\\(group\\|test\\|testWidgets\\)\\s-*(\\s-*['\"]\\([^'\"]+\\)['\"]")

(defun my-dart--enclosing-calls-at-point ()
  "Return enclosing group/test/testWidgets calls from outer to inner."
  (save-excursion
    (let ((origin (point))
          calls)
      (while (re-search-backward my-dart--test-call-re nil t)
        (let ((start (match-beginning 0)))
          (when (my-dart--call-encloses-pos-p start origin)
            (push (list :kind (match-string-no-properties 1)
                        :name (match-string-no-properties 2)
                        :start start)
                  calls))))
      calls)))

(defun my-dart-group-at-point ()
  "Return the full enclosing group path at point, or nil."
  (let (groups)
    (dolist (call (my-dart--enclosing-calls-at-point))
      (when (string= (plist-get call :kind) "group")
        (push (plist-get call :name) groups)))
    (setq groups (nreverse groups))
    (when groups
      (mapconcat #'identity groups " "))))

(defun my-dart-test-name-at-point ()
  "Return the enclosing test name at point, or nil."
  (let ((nearest (car (last (my-dart--enclosing-calls-at-point)))))
    (when nearest
      (let ((kind (plist-get nearest :kind)))
        (when (or (string= kind "test") (string= kind "testWidgets"))
          (plist-get nearest :name))))))

(defun my-dart-test-at-point ()
  "Return full test path at point, or full group path if not in a test."
  (let ((group-name (my-dart-group-at-point))
        (test-name (my-dart-test-name-at-point)))
    (cond
     ((and group-name test-name) (format "%s %s" group-name test-name))
     (test-name test-name)
     (group-name group-name))))

(add-to-list 'dape-configs
             `(dart-test
               modes (dart-mode)
               ensure dape-ensure-command
               command "dart"
               command-args ("debug_adapter" "--test")
               command-cwd dape-command-cwd
               :type "dart"
               :request "launch"
               :name "Dart Test"
               :cwd ,(lambda () (expand-file-name (project-root (project-current))))
               :program ,(lambda () (buffer-file-name))
               :args ["--concurrency=1"]
               :console "terminal"
               ))

(defun my-dart-debug-test-at-point ()
  "Debug the Dart test at point, or all tests in file if no test found."
  (interactive)
  (my-dape 'dart-test
           (when-let ((test-name (my-dart-test-at-point)))
             (list :args (vector "--concurrency=1" "--plain-name" test-name)))))


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

(defun my-dart-test-program ()
  (or (executable-find "flutter") (executable-find "dart")))

(defun my-dart-run-test-at-point (&optional prefix)
  (interactive "P")
  (let* ((test (or (my-dart-test-at-point) (error "No test at point!")))
         (reporter (if prefix "expanded" "compact"))
         (name-arg (if test (format " --name \"%s\"" test) "")))
    (compile (format "%s test%s -r %s %s" (my-dart-test-program) name-arg reporter buffer-file-name)
    )
  ))

(defun my-dart-run-tests-in-file (&optional prefix)
  (interactive "P")
  (compile (format "%s test -r %s %s" (my-dart-test-program) (if prefix "expanded" "compact") buffer-file-name)))

(define-key dart-mode-map (kbd "C-c t") #'my-dart-run-test-at-point)
(define-key dart-mode-map (kbd "C-c T") #'my-dart-run-tests-in-file)


(defun my-dart-mode-hook ()
  (company-mode)
  (eglot-ensure)
  (setq fill-column (my-dart-effective-page-width))
  (add-hook 'before-save-hook 'my-dart-before-save-hook nil t)
  )

(add-hook 'dart-mode-hook 'my-dart-mode-hook)
