(use-package paredit
  :ensure t
  :diminish paredit-mode
  :bind
  ((:map paredit-mode-map)
   ;; don't hijack RET please
   ("RET"   . nil)
   ;; don't hijack \ please
   ("\\"    . nil)
   ;;
   ("M-)"   . paredit-wrap-round-from-behind)
   ("M-["   . paredit-wrap-square)
   ("M-]"   . paredit-wrap-square-from-behind)
   ("M-{"   . paredit-wrap-curly)
   ("M-}"   . paredit-wrap-curly-from-behind))
  :init
  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-round)
    (insert " ")
    (forward-char -1))

  (defun paredit-wrap-square-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-square))

  (defun paredit-wrap-curly-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-curly))

  (defun paredit-kill-region-or-backward-word ()
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (paredit-backward-kill-word)))

  :config
  ;; making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t))

(provide 'module-paredit)
