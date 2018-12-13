
(add-hook 'haskell-mode-hook
          (lambda ()
            (electric-indent-local-mode 1)
            (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)))
