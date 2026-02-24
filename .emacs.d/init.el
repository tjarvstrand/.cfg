(setq inhibit-x-resources t)

(defconst user-home (getenv "HOME"))
(defconst user-lisp-directory (concat user-emacs-directory "/lisp"))

(add-to-list 'exec-path (format "%s/.local/share/mise/shims" user-home))
(add-to-list 'exec-path "/opt/homebrew/bin")

(add-to-list 'load-path user-lisp-directory)
(add-to-list 'load-path (concat user-lisp-directory "/lib"))

;; GUI-specific frame settings
(when (display-graphic-p)
  (setq default-frame-alist '((left . 0) (width . 0) (fullscreen . maximized))))

(setq-default c-basic-offset 2)

(let ((graphical (display-graphic-p))
      (files (sort (directory-files user-lisp-directory t "\\`[^.].*\\.el\\'")
                   #'string<)))
  (dolist (file files)
    (let ((name (file-name-nondirectory file)))
      (when (or graphical
                (and (string-match "\\`\\([0-9]+\\)-" name)
                     (< (string-to-number (match-string 1 name)) 100)))
        (load file nil 'nomessage)))))
