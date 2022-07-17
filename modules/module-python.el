(use-package module-lsp)
(use-package module-sp)

(use-package lsp-python-ms :ensure t
  :commands (python-mode))

(use-package python-mode
  :commands (python-mode)
  :hooks
  (python-mode-hook . (lsp-mode
                       smartparens-mode)))

(provide 'module-python)
