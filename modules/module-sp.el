(use-package smartparens
  :ensure t
  :commands (smartparens-mode smartparens-strict-mode)
  :hooks ((java-mode . (smartparens-mode))
          (python-mode-hook . (smartparens-mode)))
  :config (use-package setup-smartparens))

(provide 'module-sp)
