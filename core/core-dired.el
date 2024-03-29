;; -*- lexical-binding: t; -*-

(use-package dired
  :custom
  ;; Better sorting
  (dired-listing-switches "-lahv --group-directories-first")
  ;; Move files between split panes
  (dired-dwim-target t)

  :config

  ;; Better font-locking
  (use-package diredfl
    :ensure t
    :init (diredfl-global-mode))

  ;; Reload dired after making changes
  (defadvice dired-do-rename (after revert-buffer activate)
    (revert-buffer))
  (defadvice dired-do-copy (after revert-buffer activate)
    (revert-buffer))
  (defadvice dired-create-directory (after revert-buffer activate)
    (revert-buffer))
  (defadvice wdired-abort-changes (after revert-buffer activate)
    (revert-buffer))

  ;; C-a is nicer in dired if it moves back to start of files
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))

  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key dired-mode-map (kbd "k") 'dired-do-delete)

  ;; M-up is nicer in dired if it moves to the fourth line - the first file
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 4))

  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)

  ;; M-down is nicer in dired if it moves to the last file
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))

  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)

  ;; Delete with C-x C-k to match file buffers and magit
  (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

  (eval-after-load "wdired"
    '(progn
       (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
       (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
       (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

  ;; MC-like key bindings
  (define-key dired-mode-map (kbd "<f7>") 'dired-create-directory)
  (define-key dired-mode-map (kbd "<f5>") 'dired-do-copy)
  (define-key dired-mode-map (kbd "<f6>") 'dired-do-rename))

(provide 'core-dired)
 
