(require 'module-paredit)
(require 'module-lsp)
(require 'module-aggressive-indent)

(use-package cider
  :ensure t
  :hooks
  (cider-repl-mode-hook . (enable-paredit-mode))
  (cider-mode-hook . (disable-cider-completion
                      disable-cider-xref
                      my/cider--setup-eldoc))
  :custom-local
  (cider-mode . ((lsp-enable-indentation nil)))
  :custom
  (cider-repl-history-file (expand-file-name "cider-history" my-cache-dir))
  (cider-repl-history-size 1000)
  (cider-font-lock-dynamically nil)
  (cider-repl-use-clojure-font-lock nil)
  (cider-repl-display-help-banner nil)
  (cider-print-options '(("length" 20)))
  (cider-eldoc-display-for-symbol-at-point t)
  :init
  (defun disable-cider-completion ()
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t))
  (defun disable-cider-xref ()
    (remove-hook 'xref-backend-functions #'cider--xref-backend t))
  (defun my/cider--setup-eldoc ()
    "Cider's eldoc function should have higher priority than the LSP's one."
    (remove-hook 'eldoc-documentation-functions #'cider-eldoc t)
    (add-hook 'eldoc-documentation-functions #'cider-eldoc -10 t)))

(use-package clojure-mode
  :ensure t
  :diminish (clojure-mode . "Clj")
  :hooks
  (clojure-mode-hook . (enable-paredit-mode
                        cider-mode
                        aggressive-indent-mode
                        lsp-deferred)))

(provide 'module-clojure)
