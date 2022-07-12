;; -*- lexical-binding: t; -*-

(use-package project
  :defer t
  :custom
  (project-list-file (expand-file-name "projects" my-cache-dir)))

(use-package magit
  :defer t
  :ensure t
  :custom
  (transient-levels-file  (expand-file-name "transient/levels.el" my-cache-dir))
  (transient-values-file  (expand-file-name "transient/values.el" my-cache-dir))
  (transient-history-file (expand-file-name "transient/history.el" my-cache-dir)))

(provide 'core-project)
