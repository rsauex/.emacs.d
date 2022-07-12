;; -*- lexical-binding: t; -*-

(csetq
  ;; Save clipboard contents into kill-ring before replacing them
  (save-interprogram-paste-before-kill t)
  ;; Formatting
  (delete-trailing-lines nil)
  (sentence-end-double-space nil)
  (word-wrap t)
  ;; Whitespace
  (indent-tabs-mode nil)
  (require-final-newline t)
  (tab-always-indent t)
  (tabify-regexp "^\t* [ \t]+") ; for :retab
  ;; Wrapping
  (truncate-lines t)
  (truncate-partial-width-windows 50))

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
  :custom
  (browse-kill-ring-quit-action 'save-and-restore))

(use-package ace-jump-mode
  :ensure t
  :commands (ac-jump-mode ace-jump-mode-pop-mark)
  :config (ace-jump-mode-enable-mark-sync)
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package subword
  :diminish (subword-mode)
  :custom
  (global-subword-mode 1))

(use-package visual-regexp
  :ensure t
  :bind (("M-&" . vr/query-replace)
         ("M-/" . vr/replace)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :custom
  (global-undo-tree-mode 1)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist
   (list (cons "." (expand-file-name "undo-tree-hist/" my-cache-dir)))))

(use-package adaptive-wrap
  :ensure t
  :diminish adaptive-wrap-prefix-mode
  :commands (adaptive-wrap-prefix-mode)
  :hooks ((visual-line-mode-hook . (adaptive-wrap-prefix-mode)))
  :custom
  (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(provide 'core-editing)
