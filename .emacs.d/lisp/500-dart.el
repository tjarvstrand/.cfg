(require 'project)

(use-package dart-mode :demand t)
(use-package dape :demand t)
(use-package f :demand t)
(use-package yaml :demand t)

(defun my-dart--nearest-analysis-options (&optional from-file)
  (let* ((start (f-dirname (expand-file-name (or from-file buffer-file-name))))
         (nearest (f--traverse-upwards (f-exists? (f-join it "analysis_options.yaml")) start)))
    (when nearest (f-join nearest "analysis_options.yaml"))))


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

(defun my-dart-test-at-point ()
  "Find the full test name at point using regex (fallback).
Returns the full test path string or nil if no test found."
  (save-excursion
    (when (re-search-backward
           "\\(test\\|testWidgets\\)\\s-*(\\s-*['\"]\\([^'\"]+\\)['\"]"
           nil t)
      (let ((test-name (match-string-no-properties 2))
            (test-pos (point))
            (groups '()))
        ;; Find all enclosing groups
        (goto-char test-pos)
        (while (re-search-backward
                "group\\s-*(\\s-*['\"]\\([^'\"]+\\)['\"]"
                nil t)
          (let ((group-name (match-string-no-properties 1))
                (group-start (match-beginning 0)))
            ;; Check if this group encloses our test
            (save-excursion
              (goto-char group-start)
              (when (ignore-errors
                      (forward-sexp 2) ;; Skip past group declaration
                      (> (point) test-pos))
                (push group-name groups)))))
        ;; Build full test name
        (if groups
            (mapconcat 'identity (append groups (list test-name)) " ")
          test-name)))))

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

(defun my-dart-mode-hook ()
  (company-mode)
  (eglot-ensure)
  (let ((page-width (my-dart-effective-page-width)))
    (setq fill-column page-width)))

(add-hook 'dart-mode-hook 'my-dart-mode-hook)
