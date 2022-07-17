(use-package module-paredit)
(use-package module-lsp)
(use-package aggressive-indent :ensure t)

(use-package cider
  :ensure t
  :hooks
  (cider-repl-mode-hook . (enable-paredit-mode))
  (cider-mode-hook . (disable-cider-completion
                      disable-cider-xref))
  :custom
  (cider-repl-history-file (expand-file-name "cider-history" my-cache-dir))
  (cider-repl-history-size 1000)
  (cider-font-lock-dynamically nil)
  (cider-repl-use-clojure-font-lock nil)
  (cider-repl-display-help-banner nil)
  (cider-print-options '(("length" 20)))
  (cider-eldoc-display-for-symbol-at-point nil)
  :init
  (defun disable-cider-completion ()
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t))
  (defun disable-cider-xref ()
    (remove-hook 'xref-backend-functions #'cider--xref-backend 'local)))

(use-package clojure-mode
  :ensure t
  :diminish (clojure-mode . "Clj")
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode))
  :hooks
  (clojure-mode-hook . (enable-paredit-mode
                        cider-mode
                        aggressive-indent-mode
                        lsp-deferred))
  :custom-local
  (clojure-mode . ((lsp-enable-indentation nil))))

(provide 'module-clojure)
