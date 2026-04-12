(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(let ((coursier-bin (format "%s/Library/Application Support/Coursier/bin" user-home)))
  (add-to-list 'exec-path coursier-bin)
  (setenv "PATH" (concat coursier-bin ":" (getenv"PATH"))))

(defun my-scala-mode-hook ()
  (company-mode)
  (eglot-ensure))


(use-package eglot
  ;; (optional) Automatically start metals for Scala files.
  :hook (scala-mode . my-scala-mode-hook))
