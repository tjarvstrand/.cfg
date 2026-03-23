(setq vc-follow-symlinks t)

;; Show no vc info at all in the mode line. vc-display-status only controls the revision part, not the backend
(setq-default mode-line-format (delete '(vc-mode vc-mode) mode-line-format))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package diff-hl :demand t)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

(defadvice vc-git-working-revision (around vc-git-working-revision-detached activate)
  "Get the git working revision when detached"
  ad-do-it
  (when (string= ad-return-value "")
    (setq ad-return-value
          (with-output-to-string
            (with-current-buffer standard-output
              (vc-git--out-ok "describe" "--tags" "--exact-match" "HEAD")))))
  (when (string= ad-return-value "")
    (setq ad-return-value
          (with-output-to-string
            (with-current-buffer standard-output
              (vc-git--out-ok "rev-parse" "HEAD")))))
  (when (stringp ad-return-value)
    (setq ad-return-value (replace-regexp-in-string "\n$" "" ad-return-value))))
(ad-activate 'vc-git-working-revision t)


(defhydra hydra-hunk-nav (:hint nil)
  "
Hunk nav: _[_: previous  _]_: next  _q_/_RET_: quit"
  ("[" diff-hl-previous-hunk)
  ("]" diff-hl-next-hunk)
  ("q" nil :exit t)
  ("RET" nil :exit t))

(defun my-diff-hl-previous-hunk ()
  (interactive)
  (diff-hl-previous-hunk)
  (hydra-hunk-nav/body))

(defun my-diff-hl-next-hunk ()
  (interactive)
  (diff-hl-next-hunk)
  (hydra-hunk-nav/body))

(define-key diff-hl-mode-map (kbd "C-x v [") #'my-diff-hl-previous-hunk)
(define-key diff-hl-mode-map (kbd "C-x v ]") #'my-diff-hl-next-hunk)
