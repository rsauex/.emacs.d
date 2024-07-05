(require 'module-paredit)
(require 'module-aggressive-indent)
(require 'module-eglot)

(use-package cider
  :ensure t
  :hooks
  (cider-mode-hook . (my/cider--disable-completion
                      my/cider--disable-xref
                      my/cider--setup-eldoc))
  :extra-modes
  (cider-repl-mode . (paredit-mode))
  :custom
  ;; TODO: (cider-repl-history-file ".cider-repl-history")
  (cider-repl-history-file (expand-file-name "cider-history" my-cache-dir))
  (cider-repl-history-size 1000)
  (cider-font-lock-dynamically nil)
  (cider-repl-use-clojure-font-lock nil)
  (cider-repl-display-help-banner nil)
  (cider-print-options '(("length" 20)))
  (cider-eldoc-display-for-symbol-at-point t)
  :init
  (defun my/cider--disable-completion ()
    "Use completion provided by LSP."
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t))
  (defun my/cider--disable-xref ()
    "Use xref provided by LSP."
    (remove-hook 'xref-backend-functions #'cider--xref-backend t))
  (defun my/cider--setup-eldoc ()
    "Cider's eldoc function should have higher priority than the LSP's one."
    (remove-hook 'eldoc-documentation-functions #'cider-eldoc t)
    (add-hook 'eldoc-documentation-functions #'cider-eldoc -10 t)))

(use-package clojure-mode
  :ensure t
  :diminish (clojure-mode . "Clj")
  :hooks
  (clojure-mode-hook . (eglot-ensure))
  :extra-modes
  (clojure-mode . (paredit-mode
                   cider-mode
                   aggressive-indent-mode))
  :custom-local
  (clojure-mode . (;; Make sure that CIDER is used for indentation.
                   (eglot-ignored-server-capabilities '(:documentFormattingProvider
                                                        :documentRangeFormattingProvider
                                                        :documentOnTypeFormattingProvider))
                   ;; Don't change eldoc display strategy. CIDER
                   ;; provide more accurate arglists.
                   (eglot-stay-out-of '(eldoc-documentation-strategy)))))

(provide 'module-clojure)
