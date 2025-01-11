;; -*- lexical-binding: t; -*-

;; No blinking...
(csetq
  (visible-bell nil)
  (visible-cursor nil)
  (ring-bell-function #'ignore))

;; More fringe indicators
(csetq
  (indicate-buffer-boundaries t)
  (indicate-empty-lines t))

;; Draw block cursor as wide as the glyph under it
(csetq
  (x-stretch-cursor t))

;; Minibuffer resizing
(csetq
  (resize-mini-windows 'grow-only))

;; Show keystrokes in progress
(csetq
  (echo-keystrokes 0.2))

;; Answer just 'y' or 'n'
(csetq
  (use-short-answers t))

;; Always confirm when leaving Emacs
(csetq
  (confirm-kill-emacs #'yes-or-no-p))

;; Always display line and column numbers
(csetq
  (line-number-mode t)
  (column-number-mode t))

;; Don't blink when close-paren is inserted
(csetq
  (blink-matching-paren nil))

;; Less GUI, please
(csetq
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (use-dialog-box nil))

;; Yet, enable tooltips
(csetq
  (tooltip-mode t))

;; Smooth window resizing
(csetq
  (window-resize-pixelwise t))

;; Customizable window divider (bonus: more distinct doom modeline)
(csetq
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  (window-divider-mode t))

;; Initial buffer
(csetq
  (initial-buffer-choice t)
  (initial-major-mode 'text-mode))

;; Highlight matching paren
(use-package paren
  :custom
  (show-paren-mode t))

;; Highlight current line
(use-package hl-line
  :custom
  (global-hl-line-mode t))

;; Prettier modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-height 30)
  (doom-modeline-mode 1))

;; Themes

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(defconst my-dark-theme 'doom-nord)
(defconst my-light-theme 'doom-nord-light)

(defvar my-current-theme-is-dark t)

(load-theme my-dark-theme t t)
(load-theme my-light-theme t t)

(csetq
  (custom-enabled-themes '(doom-nord)))

(defun my-toggle-light-theme ()
  (interactive)
  (setq my-current-theme-is-dark (not my-current-theme-is-dark))
  (let ((theme (if my-current-theme-is-dark
                   my-dark-theme
                 my-light-theme)))
    (csetq (custom-enabled-themes (list theme)))))

;; Beacon

(require 'pulse)

(defface my-pulse-face
  '((t :inherit pulse-highlight-start-face :extend t))
  "Face used at beginning of a highlight extended to full width.")

(defun my-pulse-momentary-highlight-current-line (&rest _)
  (when (or (minibufferp)
            (not (string-prefix-p " " (buffer-name (window-buffer (selected-window))))))
    (let ((o (make-overlay (line-beginning-position) (line-beginning-position 2))))
      ;; Only in selected window
      (overlay-put o 'window (selected-window))
      ;; Mark it for deletion
      (overlay-put o 'pulse-delete t)
      ;; Pulse overlay
      (pulse-momentary-highlight-overlay o 'my-pulse-face))))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom
                   other-window
                   after-focus-change-function))
  (advice-add command :after #'my-pulse-momentary-highlight-current-line))

(add-hook 'window-configuration-change-hook #'my-pulse-momentary-highlight-current-line)

;; Scrolling

(use-package ultra-scroll
  :vc (ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll")
  :custom

  ;; --- Regular scrolling
  ;; Smooth horizontal scrolling
  (hscroll-margin 0) ;; allow the point to get right to the edge
  (hscroll-step 1)   ;; scroll one column at a time
  ;; Smooth vertical scrolling
  (scroll-margin 0) ;; allow the point to get right to the edge
  (scroll-conservatively 101) ;; scroll just enough to bring point into view
  ;; Preserve cursor position when scrolling
  (scroll-preserve-screen-position t)

  ;; --- Mouse scrolling
  ;; Scroll with mouse faster
  (mouse-wheel-scroll-amount '(5 ;; scroll 5 lines at a time
                               ((shift) . hscroll) ;; (S-) scroll horizontally when holding
                               ((meta) . nil) ;; (M-) scroll by screens
                               ((control meta) . global-text-scale) ;; (C-M-) zoom all windows
                               ((control) . text-scale))) ;; (C-) zoom window under cursor
  (mouse-wheel-scroll-amount-horizontal 5) ;; scroll 5 columns at a time
  (mouse-wheel-progressive-speed nil) ;; disable acceleration

  ;; --- Ultra scroll mode
  ;; Enable pixel-wise scrolling
  (ultra-scroll-mode 1))

(provide 'core-appearance)
