;; -*- lexical-binding: t; -*-

(use-package lsp-mode :ensure t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-show-detail nil)
  (lsp-completion-provider :none)
  :hooks
  (lsp-completion-mode-hook . (my/lsp-mode-setup-completion))
  (lsp-managed-mode-hook . (my/lsp-mode--setup-eldoc))
  :init
  (defun my/lsp-mode-setup-completion ()
    (setq completion-category-defaults
          (assq-delete-all 'lsp-capf completion-category-defaults)))
  (defun my/lsp-mode--setup-eldoc ()
    "Change LSP's eldoc function depth to 0 just to be sure."
    (when (member #'lsp-eldoc-function eldoc-documentation-functions)
      (remove-hook 'eldoc-documentation-functions #'lsp-eldoc-function t)
      (add-hook 'eldoc-documentation-functions #'lsp-eldoc-function 0 t))))

(provide 'module-lsp)
