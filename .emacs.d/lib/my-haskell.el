
(add-to-list 'exec-path "/home/tjarvstrand/.cabal/bin")
(add-to-list 'exec-path "/opt/ghc/bin")

(require 'lsp)
(require 'lsp-ui)
(require 'lsp-haskell)


;; (setq haskell-hasktags-path "/home/tjarvstrand/.cabal/bin/hasktags")
;; (require 'f)
;; (defvar lib-tags-file "TAGS_")x
;; (defun haskell-update-tags ()
;;   (let* ((dir )
;;          (tags (f-join dir "TAGS"))
;;          (lib-tags (f-join dir lib-tags-file)))
;;     (shell-command
;;      (format "cat %s >> %s"
;;              (shell-quote-argument lib-tags)
;;              (shell-quote-argument tags)))))

(defun my-haskell-mode-hook ()
  (interactive)
  (electric-indent-local-mode 1)
  (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
  ;; (setq tags-table-list
  ;;           (list (f-join (haskell-cabal--find-tags-dir) "TAGS" )
  ;;                 (f-join (haskell-cabal--find-tags-dir) "TAGS_" )))
  ;; (setq haskell-tags-on-save t)
  ;; (add-hook 'after-save-hook 'haskell-update-tags t)
  ;; (lsp)
  ;; (lsp-mode t)
  )

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
