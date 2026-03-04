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

(defun my-flymake-mode-hook ()
  (setq-local next-error-function #'my-flymake-next-error))

(add-hook 'flymake-mode-hook #'my-flymake-mode-hook)
