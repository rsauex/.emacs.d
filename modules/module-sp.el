(use-package smartparens :ensure t
  :commands (smartparens-mode smartparens-strict-mode)
  :hooks ((java-mode . (smartparens-mode)))
  :config (require' smartparens-config))

(provide 'module-sp)
