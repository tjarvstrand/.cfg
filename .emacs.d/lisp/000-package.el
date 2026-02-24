;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package.el

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))
(setq package-user-dir (expand-file-name "~/.emacs.d/packages"))
(unless (file-directory-p package-user-dir)
    (make-directory package-user-dir))

(require 'package)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;
;; use-package

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)


;;;;;;;;;;;;;;;;;;;;;
;; Universal packages

(use-package dash :demand t)
(use-package f :demand t)

(use-package diff-hl :demand t)
(use-package find-file-in-project :demand t)
(use-package mise :demand t)

(use-package s :demand t)

(use-package flycheck)
(use-package flymd)
(use-package idle-highlight-mode)
