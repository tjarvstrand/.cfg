(setq vc-follow-symlinks t)

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
  (setq ad-return-value (replace-regexp-in-string "\n$" "" ad-return-value)))
(ad-activate 'vc-git-working-revision t)
