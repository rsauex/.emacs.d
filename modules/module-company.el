(use-package company
  :ensure t
  :diminish  company-mode
  :commands (company-mode global-company-mode company-complete
                          company-complete-common company-manual-begin company-grab-line))

(use-package company-quickhelp
  :ensure t
  :after company-mode
  :config
  (company-quickhelp-mode +1))

(provide 'module-company)
