(use-package flycheck
  :ensure t
  :hooks
  (flycheck-mode-hook . (flycheck-pos-tip-mode flycheck-clojure-setup)))

(use-package flycheck-pos-tip
  :ensure t)

(use-package flycheck-clojure
  :ensure t)

(provide 'module-flycheck)
