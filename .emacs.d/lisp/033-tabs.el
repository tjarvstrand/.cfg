
(setq tab-bar-select-tab-modifiers '(super))
(tab-bar-mode 1)

;; (use-package tabspaces
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   ; (tabspaces-remove-to-default t)
;;   ;; (tabspaces-include-buffers '("*scratch*"))
;;   ;; (tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*"))  ; Additional buffers to exclude
;;   ;; sessions
;;   ;; (tabspaces-session t)
;;   ;(tabspaces-session-auto-restore t)
;;   ;; additional options
;;   (tabspaces-fully-resolve-paths t)  ; Resolve relative project paths to absolute
;;   (tpab-bar-new-tab-choice "*scratch*"))

(use-package otpp
  :after project
  :init
  (setq otpp-find-file-integration t) ; route find-file to project tab
  (otpp-mode 1))

(defun my/otpp-detach-current-buffer-to-project-tab (&rest _)
  (when-let ((buf (current-buffer)))
    (when (ignore-errors (project-current nil (buffer-local-value 'default-directory buf)))
      (ignore-errors (otpp-detach-buffer-to-tab buf)))))

(advice-add 'switch-to-buffer :after #'my/otpp-detach-current-buffer-to-project-tab)

(with-eval-after-load 'consult
  (advice-add 'consult-buffer :after #'my/otpp-detach-current-buffer-to-project-tab))
