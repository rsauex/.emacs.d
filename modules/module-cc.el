(use-package ggtags
  :ensure t)

(use-package c-mode
  :defer t
  :hooks ((c-mode-hook . (ggtags-mode)))
  :config
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode t)
  (setq c-default-style "linux"))

(provide 'module-cc)
