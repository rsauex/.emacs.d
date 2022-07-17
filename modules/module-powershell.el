(require 'module-lsp)
(require 'module-sp)

(use-package lsp-pwsh
  :custom
  (lsp-pwsh-exe (or (executable-find "pwsh-preview")
                    (executable-find "pwsh"))))

(use-package powershell :ensure t
  :hooks
  (powershell-mode-hook . (lsp-mode
                           smartparens-mode))
  :custom
  (powershell-location-of-exe (or (executable-find "pwsh-preview")
                                  (executable-find "pwsh"))))

(provide 'module-powershell)
