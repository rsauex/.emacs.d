(use-package module-flycheck)

(use-package cider
  :ensure t
  :commands (cider-mode cider-jack-in)
  :hooks ((cider-mode-hook . (ac-cider-setup))
          (cider-repl-mode-hook . (enable-paredit-mode auto-complete-mode ac-cider-setup))))

(use-package ac-cider
  :ensure t
  :commands (ac-cider-setup))

(use-package clojure-mode
  :ensure t
  :defer (clojure-mode . "Clj")
  :commands (clojure-mode)
  :hooks ((clojure-mode-hook . (enable-paredit-mode auto-complete-mode cider-mode flycheck-mode)))
  :config (use-package setup-clojure-mode)
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode)))

(provide 'module-clojure)
