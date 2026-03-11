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


(with-eval-after-load 'flymake
  (put 'flymake-error 'mode-line-face
       `(:foreground ,(face-foreground 'error nil t)))
  (put 'flymake-warning 'mode-line-face
       `(:foreground ,(face-foreground 'warning nil t)))
  (put 'flymake-note 'mode-line-face
       `(:foreground ,(face-foreground 'success nil t))))

(defun my-flymake-next-error (&optional n reset)
  "Move to the Nth next Flymake diagnostic in the current buffer.
This is compatible with `next-error-function'.  With negative N,
move backward.  RESET is accepted for compatibility and ignored."
  (ignore reset)
  (setq n (or n 1))
  (when (/= n 0)
    (let* ((diagnostics (-sort (-on (if (>= n 0) '< '>) 'flymake-diagnostic-beg) (flymake-diagnostics)))
           (following (-drop-while (lambda (diagnostic)
                                     (if (>= n 0)
                                         (<= (flymake-diagnostic-beg diagnostic) (point))
                                       (>= (flymake-diagnostic-end diagnostic) (point))))
                                   diagnostics))
           (next (or (nth (1- (abs n)) following) (car diagnostics))))
      (if next
          (progn
            (goto-char (flymake-diagnostic-beg next))
            (pulse-momentary-highlight-one-line (point)))
        (user-error "No diagnostics found in this buffer")))))

(defun my-flymake-next-error-command ()
  (interactive)
  (my-flymake-next-error 1))

(defun my-flymake-previous-error-command ()
  (interactive)
  (my-flymake-next-error -1))

(defhydra hydra-flymake-nav (:hint nil)
  "
Xref errors: _e_: next _E_: previous _q_: quit"
  ("e" my-flymake-next-error-command)
  ("E" my-flymake-previous-error-command)
  ("q" nil :exit t)
  ("RET" nil :exit t))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-g e") (lambda ()
                                               (interactive)
                                               (my-flymake-next-error-command)
                                               (hydra-flymake-nav/body)))

  (define-key flymake-mode-map (kbd "M-g E") (lambda ()
                                               (interactive)
                                               (my-flymake-previous-error-command)
                                               (hydra-flymake-nav/body))))

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
