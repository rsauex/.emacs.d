;; -*- lexical-binding: t; -*-

(use-package eglot
  :ensure t
  :custom
  ;; Don't block emacs, please
  (eglot-sync-connect nil)
  ;; Clojure LSP takes a looong time to start
  (eglot-connect-timeout 600)
  ;; Don't store events, they just take up space unless we're debugging a server or eglot
  (eglot-events-buffer-size 0)
  ;; Activate Eglot in cross-referenced non-project files
  (eglot-extend-to-xref t)
  ;; Bind keys similar to how lsp-mode does it
  :bind ((:map eglot-mode-map)
         ("C-c l w q" . eglot-shutdown)
         ("C-c l w r" . eglot-reconnect)
         ("C-c l g d" . eglot-find-declaration)
         ("C-c l g g" . xref-find-definitions)
         ("C-c l g i" . eglot-find-implementation)
         ("C-c l g r" . xref-find-references)
         ("C-c l r o" . eglot-code-action-organize-imports)
         ("C-c l r r" . eglot-rename)
         ("C-c l a a" . eglot-code-actions))
  :init
  (defvar my-eglot--started-servers
    (make-hash-table :test #'equal))

  (define-advice eglot--connect
      (:around (orig-fn &rest args) dont-start-the-same-server-multiple-times)
    "Eglot doesn't register a server before it's fully initialized. Because
of this, a new instances of the same server will be started for each
opened file before at least of one of those instances finishes
initializing.

This advice prevents eglot from starting multiple instances of the same
server by keeping track of all the servers (not just the fully
initialized ones), and throwing an error on an attempt to start a
duplicate."
    (if (let ((existing-server (gethash args my-eglot--started-servers)))
          (and existing-server (jsonrpc-running-p existing-server)))
        (eglot--error "Server already running!")
      (cl-labels ((remember-started-server (server)
                    (remove-hook 'eglot-server-initialized-hook #'remember-started-server t)
                    (puthash args server my-eglot--started-servers)))
        (remhash args my-eglot--started-servers)
        (unwind-protect
            (progn
              (add-hook 'eglot-server-initialized-hook #'remember-started-server 'append t)
              (apply orig-fn args))
          (ignore-errors
            (remove-hook 'eglot-server-initialized-hook #'remember-started-server t))))))

  (defun eglot-ensure-only-in-project ()
    "Only start eglot if the file is a part of a project."
    (when (project-current)
      (eglot-ensure))))

(use-package eglot-supplements
  :vc (eglot-supplements :url "https://codeberg.org/harald/eglot-supplements"))

(use-package eglot-marocc ;; from eglot-supplements
  :commands (eglot-marocc-request-highlights
             eglot-marocc-goto-next-highlight
             eglot-marocc-goto-previous-highlight)
  :custom-face
  (eglot-marocc-occurence-text ((t (:inherit highlight :foreground "black"))))
  :bind ((:map eglot-mode-map)
         ("C-c l a h" . eglot-marocc-request-highlights)
         ;; TODO:
         ;; ("??" . eglot-marocc-goto-next-highlight)
         ;; ("??" . eglot-marocc-goto-previous-highlight)
         ))

;; https://github.com/joaotavora/eglot/issues/661
(use-package jarchive
  :ensure t
  :init
  (jarchive-setup))

(provide 'module-eglot)
