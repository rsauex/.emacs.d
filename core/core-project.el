;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package project-rsync
  :bind (("C-c p p" . project-rsync-push)
         ("C-c p l" . project-rsync-pull)))

(provide 'core-project)
