;; -*- lexical-binding: t; -*-

(use-package ggtags
  :ensure t)

(use-package c-mode
  :extra-modes
  (c-mode . (ggtags-mode))
  :custom-local
  (c-mode . ((tab-width 4)
             (indent-tabs-mode t)))
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

(provide 'module-cc)
