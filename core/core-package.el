;; -*- lexical-binding: t; -*-

;; Do not add (package-initialize)...
(setq package--init-file-ensured t)

(csetq
  (package-user-dir (expand-file-name "elpa/" my-cache-dir))
  (package-gnupghome-dir (expand-file-name "elpa/gnupg/" my-cache-dir))
  (package-archives `(,@package-archives
                      ("melpa" . "https://melpa.org/packages/")
                      ("org"   . "https://orgmode.org/elpa/")))
  (package-install-upgrade-built-in t))

(package-initialize)

(unless (file-exists-p (expand-file-name "archives" package-user-dir))
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
