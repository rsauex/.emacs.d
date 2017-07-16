;; Highlight escape sequences
(use-package highlight-escape-sequences
  :ensure t
  :config
  (hes-mode)
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face))

;; Expand region
;; (use-package expand-region
;;   :config (progn
;;             (setq expand-region-fast-keys-enabled nil)
;;             (setq er--show-expansion-message t)))

;; (use-package multiple-cursors)

;; (use-package change-inner)

;; (use-package multifiles)

;; Browse kill ring
(use-package browse-kill-ring
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

;; Ace jump mode
(use-package ace-jump-mode
  :ensure t
  :commands (ac-jump-mode ace-jump-mode-pop-mark)
  :config (ace-jump-mode-enable-mark-sync)
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark)))

;; Unclutter the modeline
(use-package subword
  :commands (subword-mode)
  :diminish (subword-mode))

;; Visual regexp
(use-package visual-regexp
  :ensure t
  :bind (("M-&" . vr/query-replace)
         ("M-/" . vr/replace)))

(provide 'core-editing)
