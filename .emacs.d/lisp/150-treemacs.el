(use-package treemacs :demand t)

(add-to-list 'display-buffer-alist
             '("\\*Treemacs"
               (display-buffer-in-side-window)
               (side . left)
               (slot . 0)
               (dedicated . t)
               (window-parameters
                (no-other-window . t)
                (no-delete-other-windows . t))))

(save-selected-window (treemacs))
(treemacs-filewatch-mode)
(treemacs-project-follow-mode)
(treemacs-fringe-indicator-mode 'always)
(let ((git (executable-find "git"))
      (python3 (executable-find "python3")))
  (setq treemacs-git-executable (or git treemacs-git-executable)
        treemacs-python-executable python3)
  (cond
   ((and git python3) (treemacs-git-mode 'deferred))
   (git (treemacs-git-mode 'simple))))
(setq treemacs--project-follow-delay 1.0)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
