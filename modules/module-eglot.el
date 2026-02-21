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
  ;; Don't start a separate lsp server when visiting files not in the project
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
         ("C-c l a a" . eglot-code-actions)))

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
