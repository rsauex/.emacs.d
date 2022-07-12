(use-package company
  :ensure t
  :diminish company-mode
  :hooks ((after-init-hook . (global-company-mode)))
  :custom
  (company-tooltip-align-annotations t))
;; -*- lexical-binding: t; -*-

(use-package company-box
  :ensure t
  :hooks ((company-mode-hook . (company-box-mode)))
  :custom
  (company-box-enable-icon nil))

(provide 'core-company)
