;; -*- lexical-binding: t; -*-

(csetq
  (inhibit-default-init t)

  (indicate-buffer-boundaries t) ; show where buffer starts/ends
  (indicate-empty-lines t)  ; show empty lines
  ;; Keep cursors and highlights in current window only
  (cursor-in-non-selected-windows nil)
  (highlight-nonselected-windows nil)
  ;; Disable bidirectional text support for slight performance bonus
  (bidi-display-reordering nil)
  ;; Remove continuation arrow on right fringe
  (visible-bell nil)
  (visible-cursor nil)
  (ring-bell-function #'ignore)
  (x-stretch-cursor t)
  (use-dialog-box nil) ; always avoid GUI
  (redisplay-dont-pause t) ; don't pause display on input
  (show-help-function nil)         ; hide :help-echo text
  (jit-lock-defer-time nil)
  (jit-lock-stealth-nice 0.1)
  (jit-lock-stealth-time 0.2)
  (jit-lock-stealth-verbose nil)
  ;; Minibuffer resizing
  (resize-mini-windows 'grow-only)
  (max-mini-window-height 0.3))

(csetq
  (blink-matching-paren nil) ; don't blink--too distracting
  (show-paren-delay 0.075)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-mode 1))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(csetq
  (inhibit-startup-message t))

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

;; Highlight current line
(global-hl-line-mode 1)

;; Always display line and column numbers
(line-number-mode 1)
(column-number-mode 1)

(provide 'core-appearance)
