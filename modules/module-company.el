(use-package company
  :ensure t
  :diminish  company-mode
  :hooks ((after-init-hook . (global-company-mode)))
  :config
  (setq-default company-tooltip-align-annotations t))

(use-package company-quickhelp
  :ensure t
  :after company-mode
  :config
  (company-quickhelp-mode +1))

(provide 'module-company)
