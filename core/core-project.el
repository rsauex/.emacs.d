;; -*- lexical-binding: t; -*-

(use-package project
  :defer t
  :custom
  (project-list-file (concat my-cache-dir "projects")))

(use-package magit
  :defer t
  :ensure t
  :custom
  (transient-levels-file  (concat my-cache-dir "transient/levels.el"))
  (transient-values-file  (concat my-cache-dir "transient/values.el"))
  (transient-history-file (concat my-cache-dir "transient/history.el")))

(provide 'core-project)
