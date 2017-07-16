(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :hooks ((flycheck-mode-hook . (flycheck-pos-tip-mode flycheck-clojure-setup))))

(use-package flycheck-pos-tip
  :ensure t
  :commands (flycheck-pos-tip-mode))

(use-package flycheck-clojure
  :ensure t
  :commands (flycheck-clojure-setup))

(provide 'module-flycheck)
