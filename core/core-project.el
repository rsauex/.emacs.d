(use-package projectile :ensure t
  :defer 1
  :custom
  (projectile-cache-file (concat my-cache-dir "projectile.cache"))
  (projectile-known-projects-file (concat my-cache-dir "projectile.projects"))
  (projectile-completion-system 'ivy)
  :config
  (projectile-global-mode))

(provide 'core-project)
