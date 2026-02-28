(require 'ansi-color)

(use-package
  dape
  :demand t
  :bind
  (("C-c d d" . dape)
   ("C-c d b" . dape-breakpoint-toggle)
   ("C-c d n" . dape-next)
   ("C-c d s" . dape-step-in)
   ("C-c d c" . dape-continue)))

(defun my-dape (config-name overrides)
  (dape (dape--config-eval config-name overrides)))

;; Ansi colors
(defun my/dape-repl-insert-ansi-around (orig string)
  (funcall orig (if (stringp string) (ansi-color-apply string) string)))

(advice-add 'dape--repl-insert :around #'my/dape-repl-insert-ansi-around)
