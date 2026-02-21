;; -*- lexical-binding: t; -*-

(use-package shell
  :config
  ;; bash-completion
  (use-package bash-completion
    :ensure t
    :hooks
    (shell-dynamic-complete-functions . (bash-completion-dynamic-complete)))

  ;; C-d to kill buffer if process is dead.
  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))

(provide 'module-shell)
