;; -*- lexical-binding: t; -*-

;; Do not add (package-initialize)...
(setq package--init-file-ensured t)

(csetq
  (package-user-dir (concat my-cache-dir "elpa/"))
  (package-gnupghome-dir (concat my-cache-dir "elpa/gnupg"))
  (package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                      ("melpa" . "http://melpa.org/packages/")
                      ("org"   . "http://orgmode.org/elpa/"))))

(package-initialize)

(unless (file-exists-p (concat package-user-dir "archives/melpa"))
  (package-refresh-contents))

(defun my-package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

(provide 'core-package)
