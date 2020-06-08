(use-package module-company)
(use-package module-lsp)

(use-package lsp-pwsh
  :commands (powershell powershell-mode)
  :hooks ((powershell-mode-hook . (lsp-mode)))
  :config
  (setq lsp-pwsh-exe (or (executable-find "pwsh-preview")
                         (executable-find "pwsh"))))

(use-package powershell :ensure t
  :commands (powershell powershell-mode)
  :hooks ((powershell-mode-hook . (company-mode)))
  :config
  (setq powershell-location-of-exe (or (executable-find "pwsh-preview")
                                       (executable-find "pwsh"))))

(provide 'module-powershell)
