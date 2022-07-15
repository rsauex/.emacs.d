;; -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :ensure t)

(use-package project-rsync
  :defer 2
  :bind (("C-c p p" . project-rsync-push)
         ("C-c p l" . project-rsync-pull)))

(provide 'core-project)
