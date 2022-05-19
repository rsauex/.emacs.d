(use-package autorevert
  :custom
  ;; Auto refresh buffers
  (global-auto-revert-mode 1)

  ;; Also auto refresh dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

(csetq
  ;; Show keystrokes in progress
  (echo-keystrokes 0.1)
  
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)

  ;; Real emacs knights don't use shift to mark things
  (shift-select-mode nil)

  ;; Don't highlight matches with jump-char - it's distracting
  (jump-char-lazy-highlight-face nil)

  ;; Disable recursive minibuffers
  (enable-recursive-minibuffers nil)

  ;; Major mode for scratch buffer
  (initial-major-mode 'text-mode)

  ;; No key suggesings
  (suggest-key-bindings nil)

  ;; No electric indent
  (electric-indent-mode nil)

  ;; Easily navigate sillycased words
  (global-subword-mode t)

  ;; Show me empty lines after buffer end
  (indicate-empty-lines t))

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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

(provide 'core-sane-defaults)
