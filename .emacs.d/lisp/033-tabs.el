
(setq tab-bar-select-tab-modifiers '(super))
(tab-bar-mode 1)

(use-package otpp
  :after project
  :init
  (setq otpp-find-file-integration t) ; route find-file to project tab
  (otpp-mode 1))

(defvar my--otpp-detaching nil)

(defun my/otpp-detach-current-buffer-to-project-tab (&rest _)
  "When opening a new buffer make sure to switch to that buffer's project's tab"
  (unless my--otpp-detaching
    (let ((my--otpp-detaching t))
      (when-let ((buf (current-buffer)))
        (when (ignore-errors (project-current nil (buffer-local-value 'default-directory buf)))
          (ignore-errors (otpp-detach-buffer-to-tab buf)))))))

(advice-add 'switch-to-buffer :after #'my/otpp-detach-current-buffer-to-project-tab)

(with-eval-after-load 'consult
  (advice-add 'consult-buffer :after #'my/otpp-detach-current-buffer-to-project-tab))
