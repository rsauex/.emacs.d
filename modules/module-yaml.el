(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)
         ("jsTestDriver\\.conf$" . yaml-mode)))

(provide 'module-yaml)
