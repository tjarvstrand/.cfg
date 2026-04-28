(setq compilation-ask-about-save nil)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length nil)
(require 'ansi-color)

(defun my/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)

(defun my/compilation-mode-hook ()
  (setq show-trailing-whitespace nil))

(add-hook 'compilation-mode-hook #'my/compilation-mode-hook)

(add-to-list 'display-buffer-alist
             '("\\*Flymake diagnostics"
               (display-buffer-in-side-window display-buffer-pop-up-window)
               (slot . 0)
               (dedicated . t)))

(with-eval-after-load 'flymake
  (defcustom my-flymake-project-diagnostics-debounce-delay 0.75
    "Idle seconds before refreshing Flymake project diagnostics buffers."
    :type 'number
    :group 'flymake)

  (defvar my-flymake--project-diagnostics-refresh-timers (make-hash-table :test #'eq)
    "Per-buffer timers used to debounce Flymake project diagnostics refreshes.")

  (defun my-flymake--project-diagnostics-refresh-now (buffer)
    (remhash buffer my-flymake--project-diagnostics-refresh-timers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq major-mode 'flymake-project-diagnostics-mode)
          (revert-buffer)))))

  (defun my-flymake--schedule-project-diagnostics-refresh (buffer)
    (when-let ((timer (gethash buffer my-flymake--project-diagnostics-refresh-timers)))
      (cancel-timer timer))
    (puthash buffer
             (run-with-idle-timer
              my-flymake-project-diagnostics-debounce-delay
              nil
              #'my-flymake--project-diagnostics-refresh-now
              buffer)
             my-flymake--project-diagnostics-refresh-timers))

  (defun my-flymake--update-diagnostics-listings-debounced (buffer)
    "Update Flymake listing buffers relevant to BUFFER with debounce for project views."
    (dolist (probe (buffer-list))
      (with-current-buffer probe
        (cond
         ((and (eq major-mode 'flymake-project-diagnostics-mode)
               flymake--project-diagnostic-list-project
               (buffer-file-name buffer)
               (memq buffer
                     (project-buffers flymake--project-diagnostic-list-project)))
          (my-flymake--schedule-project-diagnostics-refresh (current-buffer)))
         ((and (eq major-mode 'flymake-diagnostics-buffer-mode)
               (eq flymake--diagnostics-buffer-source buffer))
          (revert-buffer))))))

  (advice-add #'flymake--update-diagnostics-listings
              :override
              #'my-flymake--update-diagnostics-listings-debounced))
