(require 'project)
(require 'treesit)

(use-package dart-mode :demand t)
(use-package dape :demand t)
(use-package f :demand t)
(use-package yaml :demand t)

(add-to-list
 'treesit-language-source-alist
 '(dart "https://github.com/UserNobody14/tree-sitter-dart"))

(unless (treesit-language-available-p 'dart)
  (treesit-install-language-grammar 'dart))

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

(defconst my-dart--test-call-kinds '("group" "test" "testWidgets"))

(defun my-dart--ensure-dart-parser ()
  (unless (treesit-parser-list nil 'dart)
    (treesit-parser-create 'dart)))

(defun my-dart--dart-string-literal-content (node)
  "Return NODE string literal content without delimiters, or nil."
  (when (and node (string= (treesit-node-type node) "string_literal"))
    (let ((raw (treesit-node-text node t)))
      ;; fixme, this is stupid
      (cond
       ((string-prefix-p "r\"\"\"" raw)
        (substring raw 4 -3))
       ((string-prefix-p "R\"\"\"" raw)
        (substring raw 4 -3))
       ((string-prefix-p "r'''" raw)
        (substring raw 4 -3))
       ((string-prefix-p "R'''" raw)
        (substring raw 4 -3))
       ((string-prefix-p "\"\"\"" raw)
        (substring raw 3 -3))
       ((string-prefix-p "'''" raw)
        (substring raw 3 -3))
       ((string-prefix-p "r\"" raw)
        (substring raw 2 -1))
       ((string-prefix-p "R\"" raw)
        (substring raw 2 -1))
       ((string-prefix-p "r'" raw)
        (substring raw 2 -1))
       ((string-prefix-p "R'" raw)
        (substring raw 2 -1))
       ((string-prefix-p "\"" raw)
        (substring raw 1 -1))
       ((string-prefix-p "'" raw)
        (substring raw 1 -1))
       (t raw)))))

(defun my-dart--test-call-info-from-node (node)
  "Return plist for NODE if it's a supported Dart test call, else nil."
  (when (string= (treesit-node-type node) "expression_statement")
    (let* ((callee (treesit-node-child node 0))
           (selector (treesit-node-child node 1))
           (kind (and callee (string= (treesit-node-type callee) "identifier")
                      (treesit-node-text callee t))))
      (when (and kind (member kind my-dart--test-call-kinds)
                 selector (string= (treesit-node-type selector) "selector"))
        (let* ((argument-part (treesit-node-child selector 0))
               (arguments (and argument-part
                               (string= (treesit-node-type argument-part) "argument_part")
                               (treesit-node-child argument-part 0)))
               (first-argument (and arguments
                                    (string= (treesit-node-type arguments) "arguments")
                                    (treesit-node-child arguments 1)))
               (string-node (and first-argument
                                 (string= (treesit-node-type first-argument) "argument")
                                 (treesit-node-child first-argument 0)))
               (name (my-dart--dart-string-literal-content string-node)))
          (when name
            (list :kind kind
                  :name name
                  :start (treesit-node-start node))))))))

(defun my-dart--enclosing-calls-at-point ()
  "Return enclosing group/test/testWidgets calls from outer to inner."
  (my-dart--ensure-dart-parser)
  (let ((node (treesit-node-at (point) 'dart))
        calls)
    (while node
      (when-let ((call (my-dart--test-call-info-from-node node)))
        (push call calls))
      (setq node (treesit-node-parent node)))
    calls))

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

(defvar my-dart-last-test nil)

(defun my-dart-run-test-at-point (&optional prefix)
  (interactive "P")
  (let* ((test (or (my-dart-test-at-point) (error "No test at point!"))))
    (setq my-dart-last-test (cons buffer-file-name test))
    (my-dart--run-tests prefix buffer-file-name test)))

(defun my-dart-run-last-test (&optional prefix)
  (interactive "P")
  (unless my-dart-last-test-name
    (error "No last test to run"))
  (my-dart--run-tests prefix (car my-dart-last-test) (cdr my-dart-last-test)))


(defun my-dart-run-tests-in-file (&optional prefix)
  (interactive "P")
  (setq my-dart-last-test (list buffer-file-name))
  (my-dart--run-tests prefix buffer-file-name nil))

(defun my-dart--run-tests (prefix test-file test-name)
  (interactive "P")
  (let ((program (my-dart-test-program))
        (reporter (if prefix "expanded" "compact"))
        (name-arg (if test-name (format " --name \"%s\"" test-name) "")))
  (compile (format "%s test%s -r %s %s" program name-arg reporter test-file))))

(defun my-dart--terminate-statement ()
  (interactive)
  (save-excursion
    (end-of-line)
    (when (eq (char-before) ?\;)
      ;; Delete the semicolon and reinsert to trigger indentation
      (delete-char -1))
    (let ((last-command-event ?\;))
      (call-interactively #'self-insert-command))))

(define-key dart-mode-map (kbd "C-c t t") #'my-dart-run-test-at-point)
(define-key dart-mode-map (kbd "C-c t T") #'my-dart-run-tests-in-file)
(define-key dart-mode-map (kbd "C-c t d") #'my-dart-debug-test-at-point)
;;(define-key dart-mode-map (kbd "C-c t D") #'my-dart-debug-tests-in-file) fixme
(define-key dart-mode-map (kbd "C-c t r") #'my-dart-run-last-test)

(define-key dart-mode-map (kbd "C-x ;") #'my-dart--terminate-statement)


(defun my-dart-mode-hook ()
  (company-mode)
  (eglot-ensure)
  (setq fill-column (my-dart-effective-page-width))
  (add-hook 'before-save-hook 'my-dart-before-save-hook nil t)
  )

(add-hook 'dart-mode-hook 'my-dart-mode-hook)
