(use-package java-ts-mode)
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

(add-to-list
 'treesit-language-source-alist
 '(java "https://github.com/tree-sitter/tree-sitter-java"))

(unless (treesit-language-available-p 'java)
  (treesit-install-language-grammar 'java))

(defun my-java-mode-hook ()
  (company-mode)
  (eglot-ensure))

(add-hook 'java-ts-mode-hook 'my-java-mode-hook)


