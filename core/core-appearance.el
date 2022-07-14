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

(setq my-current-theme-is-dark t)

(defun my-load-theme (theme)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme theme t))))
    (load-theme theme t))
  
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-themes
  :ensure t
  :config
  (my-load-theme 'doom-nord))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 30)
  (doom-modeline-mode 1))

(defun my-toggle-light-theme ()
  (interactive)
  (my-load-theme
   (if my-current-theme-is-dark
       'doom-nord-light
     'doom-nord))
  (setq my-current-theme-is-dark (not my-current-theme-is-dark)))

(provide 'core-appearance)
