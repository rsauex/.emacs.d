(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(define-key global-map (kbd "M-<f6>") 'org-capture)

(use-package org-variable-pitch
  :ensure t
  :commands (org-variable-pitch-minor-mode)
  :custom
  (org-variable-pitch-fixed-font (face-attribute 'fixed-pitch :family))
  (org-variable-pitch-fixed-faces '(org-block
                                    org-block-begin-line
                                    org-block-end-line
                                    org-code
                                    org-document-info-keyword
                                    org-done
                                    org-formula
                                    org-indent
                                    org-meta-line
                                    org-property-value
                                    org-special-keyword
                                    org-table
                                    org-todo
                                    org-verbatim
                                    org-date))
  :config
  (let* ((base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit variable-pitch :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline))))
     `(org-level-7 ((t (,@headline))))
     `(org-level-6 ((t (,@headline))))
     `(org-level-5 ((t (,@headline))))
     `(org-level-4 ((t (,@headline :height 1.1))))
     `(org-level-3 ((t (,@headline :height 1.25))))
     `(org-level-2 ((t (,@headline :height 1.5))))
     `(org-level-1 ((t (,@headline :height 1.75))))
     `(org-document-title ((t (,@headline :height 2.0 :underline nil)))))))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("› ")))

(use-package org-mode
  :commands (org-mode)
  :hooks ((org-mode-hook . (org-bullets-mode
                            org-variable-pitch-minor-mode)))
  :custom-local
  (org-mode . (;; Bar cursor in org-mode
               (cursor-type 'bar)
               ;; Enable Ispell completion
               (company-backends (cons 'company-ispell company-backends))))
  :custom
  ;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
  (org-replace-disputed-keys t)
  ;; Fontify org-mode code blocks
  (org-src-fontify-natively t)
  (org-startup-indented t)
  (org-ellipsis "  ") ;; folding symbol
  (org-pretty-entities t)
  (org-hide-emphasis-markers t) ;; show actually italicized text instead of /italicized text/
  (org-agenda-block-separator "")
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  ;; org-mode colors
  (org-todo-keyword-faces '(("INPR" . (:foreground "yellow" :weight bold))
                            ("DONE" . (:foreground "green" :weight bold))
                            ("IMPEDED" . (:foreground "red" :weight bold)))))

(provide 'module-org)
