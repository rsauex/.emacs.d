(use-package smartparens :ensure t
  :hooks
  (java-mode . (smartparens-mode))
  :config
  (require 'smartparens-config))

(provide 'module-sp)
