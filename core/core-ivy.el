;; -*- lexical-binding: t; -*-

(use-package counsel :ensure t
  :demand t
  :diminish ivy-mode
  :bind (([remap isearch-forward]          . swiper)
         ([remap describe-bindings]        . counsel-descbinds))
  :custom
  (suggest-key-bindings nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-fixed-height-minibuffer t)
  ;; (ivy-mode t)
  )

(provide 'core-ivy)
