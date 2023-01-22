;; -*- lexical-binding: t; -*-

;; Wrap lines at whitespaces, not in a middle of a word
(csetq
  (word-wrap t))

;; Don't wrap lines by default
(csetq
  (truncate-lines t)
  (truncate-partial-width-windows nil))

;; TAB key should only indent by default
(csetq
  (tab-always-indent t))

;; Require an empty line in the of files
(csetq
  (require-final-newline t))

;; Never use tabs unless explicitly asked to
(csetq
  (indent-tabs-mode nil))

;; Use single space to separate sentences
(csetq
  (sentence-end-double-space nil))

;; Save clipboard contents into kill-ring before replacing them
(csetq
  (save-interprogram-paste-before-kill t))

;; Real emacs knights don't use shift to mark things
(csetq
  (shift-select-mode nil))

;; Remove text in active region if inserting text
(csetq
  (delete-selection-mode t))

;; Move files to trash when deleting
(csetq
  (delete-by-moving-to-trash t))

;; Disable recursive minibuffers
(csetq
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1))

;; Do not allow the cursor in the minibuffer prompt
(csetq
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package electric
  :custom
  (electric-indent-mode nil)
  (electric-layout-mode nil)
  (electric-quote-mode nil))

(use-package highlight-escape-sequences
  :ensure t
  :custom
  (hes-mode t))

(use-package multiple-cursors
  :ensure t)

(use-package ace-jump-mode
  :ensure t
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

(csetq
  (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(use-package visual-fill-column
  :ensure t
  :diminish visual-fill-column-mode
  :hooks
  (visual-line-mode-hook . (visual-fill-column-mode))
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-adjust-for-text-scale nil))

(use-package adaptive-wrap
  :ensure t
  :diminish adaptive-wrap-prefix-mode
  :hooks
  (visual-line-mode-hook . (adaptive-wrap-prefix-mode)))

;; Show trailing whitespaces only when visiting a file
(defun maybe-toggle-show-trailing-whitespace ()
  (setq-local show-trailing-whitespace (not (null (buffer-file-name)))))

(add-hook 'after-change-major-mode-hook #'maybe-toggle-show-trailing-whitespace)
(add-hook 'after-save-hook #'maybe-toggle-show-trailing-whitespace)

;; Remove trailing whitespaces on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(provide 'core-editing)
