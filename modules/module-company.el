(use-package company
  :commands (company-mode global-company-mode company-complete
                          company-complete-common company-manual-begin company-grab-line)
  :config
  (require 'company-quickhelp)
  (company-quickhelp-mode +1))

(provide 'module-company)
