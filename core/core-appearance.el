(defvar my/ui-font
  (font-spec :family "Fira Mono" :size 14)
  "The font currently in use.")

(defvar my/ui-variable-pitch-font
  (font-spec :family "Fira Sans" :size 14)
  "The font currently in use.")

(defvar my/ui-japanese-font
  (font-spec :family "IPAexGothic" :size 14)
  "Japanese font currently in use.")

;; Fonts
(create-fontset-from-fontset-spec "-*-Fira Mono-*-*-*-*-14-*-*-*-*-*-fontset-my")
(set-fontset-font "fontset-my" nil my/ui-font)
(set-fontset-font "fontset-my" 'latin my/ui-font)
(set-fontset-font "fontset-my" 'cyrillic my/ui-font)
(set-fontset-font "fontset-my" 'han my/ui-japanese-font)
(set-fontset-font "fontset-my" 'kana my/ui-japanese-font)
(set-face-attribute 'variable-pitch nil :font my/ui-variable-pitch-font)
(push `(font . "fontset-my") default-frame-alist)

(setq-default
 inhibit-default-init t

 indicate-buffer-boundaries t ; show where buffer starts/ends
 indicate-empty-lines t  ; show empty lines
 ;; Keep cursors and highlights in current window only
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 ;; Disable bidirectional text support for slight performance bonus
 bidi-display-reordering nil
 ;; Remove continuation arrow on right fringe
 visible-bell nil
 visible-cursor nil
 ring-bell-function #'ignore
 x-stretch-cursor t
 use-dialog-box nil ; always avoid GUI
 redisplay-dont-pause t ; don't pause display on input
 show-help-function nil         ; hide :help-echo text
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 ;; Minibuffer resizing
 resize-mini-windows 'grow-only
 max-mini-window-height 0.3)

(setq-default
 blink-matching-paren nil ; don't blink--too distracting
 show-paren-delay 0.075
 show-paren-highlight-openparen t
 show-paren-when-point-inside-paren t)
(show-paren-mode 1)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme 'doom-tomorrow-night t))))
    (load-theme 'doom-tomorrow-night t))
  
  (doom-themes-neotree-config))

(use-package solaire-mode
  :ensure t
  :hooks ((after-change-major-mode-hook . (turn-on-solaire-mode))
          (minibuffer-setup-hook . (solaire-mode-in-minibuffer))))

;; full filename in frame title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Scroll
(setq hscroll-margin 1
      hscroll-step 1
      scroll-conservatively 1001
      scroll-margin 0
      scroll-preserve-screen-position t)

;; Mouse scroll
(setq mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
      mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling

;; Highlight current line
(global-hl-line-mode 1)

;; Always display line and column numbers
(line-number-mode 1)
(column-number-mode 1)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (setq sml/name-width 20)
  (setq sml/mode-width 'right)
  (sml/setup))


(provide 'core-appearance)
