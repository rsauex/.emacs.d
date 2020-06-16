(use-package company
  :ensure t
  :diminish company-mode
  :hooks ((after-init-hook . (global-company-mode)))
  :config
  (setq-default company-tooltip-align-annotations t))

(use-package company-box
  :ensure t
  :hooks ((company-mode-hook . (company-box-mode)))
  :config
  (setq company-box-enable-icon nil))

(provide 'core-company)
