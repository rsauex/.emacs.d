(setq-default
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Formatting
 delete-trailing-lines nil
 sentence-end-double-space nil
 word-wrap t
 ;; Whitespace
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 ;;tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50)

 ;; whitespace-mode
(setq whitespace-line-column fill-column
      whitespace-style '(face indentation tabs tab-mark spaces space-mark newline newline-mark
                              trailing lines-tail)
      whitespace-display-mappings '((tab-mark     ?\t [?› ?\t])
                                    (newline-mark ?\n [?¬ ?\n])
                                    (space-mark   ?\  [?·] [?.])))

(use-package highlight-escape-sequences
  :ensure t
  :defer 2
  :config
  (hes-mode)
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face))

(use-package multiple-cursors
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :commands (browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package ace-jump-mode
  :ensure t
  :commands (ac-jump-mode ace-jump-mode-pop-mark)
  :config (ace-jump-mode-enable-mark-sync)
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package subword
  :commands (subword-mode)
  :diminish (subword-mode))

(use-package visual-regexp
  :ensure t
  :bind (("M-&" . vr/query-replace)
         ("M-/" . vr/replace)))

(use-package undo-tree
  :ensure t
  :defer 1
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-enable-undo-in-region 0
        undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat my-cache-dir "undo-tree-hist/")))))

(use-package adaptive-wrap
  :ensure t
  :defer t
  :hooks ((visual-line-mode-hook . (adaptive-wrap-prefix-mode)))
  :config
  (csetq
    (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))))

(provide 'core-editing)
