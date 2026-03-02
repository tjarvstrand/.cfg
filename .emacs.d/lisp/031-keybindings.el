(use-package hydra :demand t)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."

  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(setq-default indent-tabs-mode nil)
(setq ring-bell-function (lambda ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key [C-down-mouse-1] #'xref-find-definitions)))

(define-key key-translation-map (kbd "C-x M-a") (kbd "å"))
(define-key key-translation-map (kbd "C-x M-A") (kbd "Å"))
(define-key key-translation-map (kbd "C-x M-o") (kbd "ä"))
(define-key key-translation-map (kbd "C-x M-O") (kbd "Ä"))
(define-key key-translation-map (kbd "C-x M-u") (kbd "ö"))
(define-key key-translation-map (kbd "C-x M-U") (kbd "Ö"))

;; C-k at beginning of line kills the entire line including newline
(setq kill-whole-line 1)

(global-set-key (kbd "M-g p") 'symbol-previous)
(global-set-key (kbd "M-g n") 'symbol-next)

(global-set-key (kbd "C-c P") 'erl-print)
(global-set-key (kbd "C-c M-p") 'erl-print-res)

(global-set-key (kbd "C-S-Y") 'yank-and-inc)
(global-set-key (kbd "C-c C-c") 'comment-line)

(global-set-key (kbd "C-c i") 'insert-filename)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-e") 'eval-buffer)
(global-set-key (kbd "C-x +") 'inc-font-size)
(global-set-key (kbd "C-x -") 'dec-font-size)

;; rectangleMark
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)


(global-set-key (kbd "C-x M-g") (lambda () (interactive) (goto-long-line 81)))

(global-set-key (kbd "M-F") 'fullscreen-toggle)
(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


(global-set-key [f8] 'neotree-toggle)
(global-set-key [(shift f8)] 'neotree-show)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window management

(global-set-key (kbd "C-x O") #'(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-tab>") #'(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "<C-iso-lefttab>") #'(lambda () (interactive) (other-window -1)))

(defhydra hydra-window-nav (:hint nil)
  "
Window nav: _p_: up  _P_: delete-up  _n_: down  _N_: delete-down  _b_: left  _B_: delete-left  _f_: right  _F_: delete-right _ q_: quit
"
  ("p" windmove-up)
  ("P" windmove-delete-up)
  ("n" windmove-down)
  ("N" windmove-delete-down)
  ("b" windmove-left)
  ("B" windmove-delete-left)
  ("f" windmove-right)
  ("F" windmove-delete-right)
  ("q" nil :exit t)
  ("RET" nil :exit t))

(global-set-key (kbd "C-c w") #'hydra-window-nav/body)
