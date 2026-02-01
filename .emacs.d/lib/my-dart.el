(add-to-list 'project-vc-extra-root-markers "pubspec.yaml")
(use-package dart-mode)

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

(setq dape-configs (cdr dape-configs))


(defun my-dart-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  (company-mode)
  (eglot-ensure))

(add-hook 'dart-mode-hook 'my-dart-mode-hook)



             ;; ('dart 'ensure 'dape-ensure-command 'modes '(dart-mode) 'command
             ;;       "dart" 'command-args '("debug_adapter") 'command-cwd
             ;;       'dape-command-cwd :type "dart" :cwd "." :program
             ;;       "lib/main.dart" :toolArgs ["-d" "all"]))
