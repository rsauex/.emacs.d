(use-package counsel :ensure t
  :demand t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap switch-to-buffer]         . ivy-switch-buffer)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-bindings]        . counsel-descbinds)
         ([remap find-library]             . counsel-find-library)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate)
         
         ;; ("C-x C-p" . counsel-project)
         
         ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-magic-slash-non-match-action nil)
  :config
  (ivy-mode 1)
  ;; (counsel-projectile-mode)
  )

(provide 'core-ivy)
