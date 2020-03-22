(use-package find-file-in-project
  :ensure t
  :defer 1
  :config

  ;; No need to be stingy
  (setq ffip-limit 4096)

  ;; Use full project path for ffip

  ;; (defun ffip-project-files ()
  ;;   "Return an alist of all filenames in the project and their path."
  ;;   (let ((file-alist nil))
  ;;     (mapcar (lambda (file)
  ;;               (let ((file-cons (cons (s-chop-prefix (file-truename (ffip-project-root)) (expand-file-name file))
  ;;                                      (expand-file-name file))))
  ;;                 (add-to-list 'file-alist file-cons)
  ;;                 file-cons))
  ;;             (split-string (shell-command-to-string
  ;;                            (format "find %s -type f \\( %s \\) %s | head -n %s"
  ;;                                    (or ffip-project-root
  ;;                                        (ffip-project-root)
  ;;                                        (error "No project root found"))
  ;;                                    (ffip-join-patterns)
  ;;                                    ffip-find-options
  ;;                                    ffip-limit))))))

  ;; Helper methods to create local settings

  (defun ffip--create-exclude-find-options (names)
    (mapconcat (lambda (name)
                 (concat "-not -regex \".*" name ".*\"")) names " "))

  (defun ffip-local-excludes (&rest names)
    "Given a set of names, will exclude results with those names in the path."
    (set (make-local-variable 'ffip-find-options)
         (ffip--create-exclude-find-options names)))

  (defun ffip-local-patterns (&rest patterns)
    "An exhaustive list of file name patterns to look for."
    (set (make-local-variable 'ffip-patterns) patterns))

  ;; Function to create new functions that look for a specific pattern
  (defun ffip-create-pattern-file-finder (&rest patterns)
    (lexical-let ((patterns patterns))
      (lambda ()
        (interactive)
        (let ((ffip-patterns patterns))
          (find-file-in-project)))))

  ;; Default excludes - override with ffip-local-excludes

  (setq-default ffip-find-options
                (ffip--create-exclude-find-options
                 '("/node_modules"
                   "/bower_components"
                   "/target"
                   "/out"
                   "/overlays"
                   "/build"
                   "/dist"
                   "/vendor"
                   ".cask"
                   "/generated"
                   "/resources/public/js/compiled"
                   "/.repl"
                   "/.tmp")))) 

;; Magit
(use-package magit
  :ensure t
  :commands (magit-status-fullscreen)
  :bind (("C-x m" . magit-status-fullscreen))
  :init
  (setq transient-levels-file  (concat my-cache-dir "transient/levels.el")
        transient-values-file  (concat my-cache-dir "transient/values.el")
        transient-history-file (concat my-cache-dir "transient/history.el"))
  :config
  ;; full screen magit-status
  (defun magit-status-fullscreen (prefix)
    (interactive "P")
    (magit-status)
    (unless prefix
      (delete-other-windows)))

  ;; move cursor into position when entering commit message
  (defun my-magit-cursor-fix ()
    (beginning-of-buffer)
    (when (looking-at "#")
      (forward-line 2)))

  (add-hook 'git-commit-mode-hook 'my-magit-cursor-fix)

  ;; full screen vc-annotate
  (defun vc-annotate-quit ()
    "Restores the previous window configuration and kills the vc-annotate buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :vc-annotate-fullscreen))

  (eval-after-load "vc-annotate"
    '(progn
       (defadvice vc-annotate (around fullscreen activate)
         (window-configuration-to-register :vc-annotate-fullscreen)
         ad-do-it
         (delete-other-windows))

       (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit))))

(provide 'core-vc)
