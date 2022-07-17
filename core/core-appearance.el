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

;; Customizable window divider (bonus: more distinct doom modeline)
(csetq
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  (window-divider-mode t))

;; Scroll
(csetq
  (hscroll-margin 1)
  (hscroll-step 1)
  (scroll-conservatively 1001)
  (scroll-margin 0)
  (scroll-preserve-screen-position t))

;; Mouse scroll
(csetq
  (mouse-wheel-scroll-amount '(5 ((shift) . 2)))  ; one line at a time
  (mouse-wheel-progressive-speed nil))            ; don't accelerate scrolling

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

(provide 'core-appearance)
