;;(use-package module-flycheck)
(use-package module-paredit)
(use-package aggressive-indent :ensure t)

(use-package cider
  :ensure t
  :hooks ((cider-repl-mode-hook . (enable-paredit-mode)))
  :custom
  (cider-repl-history-file (expand-file-name "cider-history" my-cache-dir))
  (cider-repl-history-size 1000)
  (cider-font-lock-dynamically nil)
  (cider-repl-use-clojure-font-lock nil)
  (cider-repl-display-help-banner nil)
  (cider-print-options '(("length" 20)))
  :config
  ;; (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc clojure-cider-typed))
  ;; (flycheck-clojure-setup)
  )

(use-package clojure-mode
  :ensure t
  :diminish (clojure-mode . "Clj")
  :hooks ((clojure-mode-hook . (enable-paredit-mode
                                cider-mode
                                aggressive-indent-mode)))
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode))
  :config
  ;; (clojure/fancify-symbols 'clojure-mode)
  ;; (clojure/enable-special-indent)
  )

;; (defun clojure/fancify-symbols (mode)
;;   "Pretty symbols for Clojure's anonymous functions and sets,
;;    like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
;;   (font-lock-add-keywords mode
;;                           `(("(\\(fn\\)[\[[:space:]]"
;;                              (0 (progn (compose-region (match-beginning 1)
;;                                                        (match-end 1) "λ")
;;                                        nil)))
;;                             ("(\\(partial\\)[\[[:space:]]"
;;                              (0 (progn (compose-region (match-beginning 1)
;;                                                        (match-end 1) "Ƥ")
;;                                        nil)))
;;                             ("(\\(comp\\)[\[[:space:]]"
;;                              (0 (progn (compose-region (match-beginning 1)
;;                                                        (match-end 1) "∘")
;;                                        nil)))
;;                             ("\\(#\\)("
;;                              (0 (progn (compose-region (match-beginning 1)
;;                                                        (match-end 1) "ƒ")
;;                                        nil)))
;;                             ("\\(#\\){"
;;                              (0 (progn (compose-region (match-beginning 1)
;;                                                        (match-end 1) "∈")
;;                                        nil))))))


;; (defvar clojure/special-indent-list
;;   '((defsymbolmacro 1)
;;     (symbol-macrolet 1)
;;     (macrolet 1)
    
;;     (match 1)
;;     (domonad 1)
;;     (m-when 1)
;;     (block 1)

;;     (!let 1)
;;     (!let* 1)
;;     (!defvar 2)
;;     (!with-var-sorts 1)))

;; (defun clojure/enable-special-indent ()
;;   (mapcar (lambda (x)
;;             (put-clojure-indent (car x) (cadr x)))
;;           clojure/special-indent-list))

(provide 'module-clojure)
