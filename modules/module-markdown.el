(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))
  :hooks
  (markdown-mode . (smartparens-mode))
  :custom-local
  (markdown-mode . ((imenu-generic-expression markdown-imenu-generic-expression)))
  :custom
  (markdown-imenu-generic-expression '(("title"  "^\\(.*\\)[\n]=+$" 1)
                                       ("h2-"    "^\\(.*\\)[\n]-+$" 1)
                                       ("h1"   "^# \\(.*\\)$" 1)
                                       ("h2"   "^## \\(.*\\)$" 1)
                                       ("h3"   "^### \\(.*\\)$" 1)
                                       ("h4"   "^#### \\(.*\\)$" 1)
                                       ("h5"   "^##### \\(.*\\)$" 1)
                                       ("h6"   "^###### \\(.*\\)$" 1)
                                       ("fn"   "^\\[\\^\\(.*\\)\\]" 1))))

(provide 'module-markdown)
