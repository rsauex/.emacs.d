(use-package lsp-mode :ensure t
  :commands (lsp lsp-mode)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-completion-show-detail nil
        lsp-session-file (expand-file-name "lsp-session-v1" my-cache-dir)))
  
(provide 'module-lsp)
