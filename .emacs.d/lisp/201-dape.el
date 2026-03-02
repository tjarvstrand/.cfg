(use-package hydra :demand t)
(require 'ansi-color)

(use-package dape :demand t)

(defhydra hydra-dape (:hint nil)
  "
dape: _d_: debug _b_: breakpoint toggle _n_/_RET_: next _s_: step in _c_/_SPC_: continue _q_: quit
"
  ("d" dape)
  ("b" dape-breakpoint-toggle)
  ("n" dape-next)
  ("RET" dape-next)
  ("s" dape-step-in)
  ("M-RET" dape-next)
  ("c" dape-contine)
  ("SPC" dape-contine)
  ("q" nil :exit t))

(global-set-key (kbd "C-c d") #'hydra-dape/body)

(defun my-dape (config-name overrides)
  (dape (dape--config-eval config-name overrides)))

;; Ansi colors
(defun my/dape-repl-insert-ansi-around (orig string)
  (funcall orig (if (stringp string) (ansi-color-apply string) string)))

(advice-add 'dape--repl-insert :around #'my/dape-repl-insert-ansi-around)
(toggle-buffer-tail "*dape-repl*" "on")
