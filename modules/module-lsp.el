(use-package lsp-mode :ensure t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-show-detail nil))

(provide 'module-lsp)
