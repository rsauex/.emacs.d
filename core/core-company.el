;; -*- lexical-binding: t; -*-

(use-package company-box
  :ensure t
  :diminish company-box-mode
  :custom
  (company-box-enable-icon nil))

(use-package company
  :ensure t
  :diminish company-mode
  :hooks
  ((after-init-hook . (global-company-mode))
   (company-mode-hook . (company-box-mode)))
  :bind
  (;; Remap completion-at-point
   :map company-mode-map
   ("C-M-i" . company-complete))
  :custom
  (company-tooltip-align-annotations t))

(provide 'core-company)
