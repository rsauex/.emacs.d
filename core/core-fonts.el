(defvar my/ui-font
  (font-spec :family "Monospace" :size 16)
  "The font currently in use.")

;; (defvar my/ui-variable-pitch-font
;;   (font-spec :family "Source Sans Pro" :size 16)
;;   "The font currently in use.")

(defvar my/ui-japanese-font
  (font-spec :family "Monospace" :size 16 :lang "ja")
  "Japanese font currently in use.")

(defun my--fontset-name (name font)
  (let ((xlfd (font-xlfd-name font)))
    (concat (cl-subseq xlfd 0 (- (length xlfd) 3)) name)))

;;;; Fixed fontset

(defvar my/fixed-fontset (my--fontset-name "fontset-fixed-my" my/ui-font))

(new-fontset
 my/fixed-fontset
 `((nil       ,my/ui-font)
   (latin     ,my/ui-font)
   (cyrillic  ,my/ui-font)
   (han       ,my/ui-japanese-font)
   (kana      ,my/ui-japanese-font)
   (cjk-misc  ,my/ui-japanese-font)
   ))

;; Faces

;; (set-face-attribute 'default t :inherit 'fixed-pitch)

(set-face-font 'default        my/fixed-fontset t)
;; (set-face-font 'fixed-pitch    my/fixed-fontset t)
;; (set-face-font 'variable-pitch my/ui-variable-pitch-font)

;; (add-to-list 'default-frame-alist `(font . ( 'fixed-pitch)))

(provide 'core-fonts)
