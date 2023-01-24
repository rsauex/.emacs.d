;; -*- lexical-binding: t; -*-

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :hooks
  (minibuffer-setup-hook . (corfu-enable-in-minibuffer))
  :init
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil)
      (corfu-mode 1)))

  (global-corfu-mode)
  (corfu-popupinfo-mode))

(provide 'core-corfu)
