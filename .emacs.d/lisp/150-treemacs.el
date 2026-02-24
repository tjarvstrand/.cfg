(use-package treemacs :demand t)

(setq display-buffer-alist
      (append display-buffer-alist
              '(("\\*Treemacs"
                 (display-buffer-in-side-window)
                 (side . left)
                 (slot . 0)
                 (dedicated . t)
                 (window-parameters
                  (no-other-window . t)
                  (no-delete-other-windows . t))))))

(save-selected-window (treemacs))
(treemacs-filewatch-mode)
(treemacs-project-follow-mode)
(setq treemacs--project-follow-delay 0.5)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
