(add-to-list 'project-vc-extra-root-markers "pubspec.yaml")
(use-package dart-mode)
(require 'dape)

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
  (eglot-ensure))

(add-hook 'dart-mode-hook 'my-dart-mode-hook)
