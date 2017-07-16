;; Ivy mode
(use-package swiper
  :demand t
  :diminish ivy-mode
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

(provide 'core-ivy)
