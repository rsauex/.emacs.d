;;; project-rsync.el --- Sync project with remote computer using rsync -*- lexical-binding: t; -*-

(require 'project)

(defgroup project-rsync nil
  "Sync project with remote computer using rsync."
  :prefix "project-rsync-"
  :group 'project)

(defcustom project-rsync-additional-options (list)
  "A list of additional options for rsync."
  :type '(list string))

(defcustom project-rsync-remote-projects-dir nil
  "The remote root dir for project dirs."
  :type 'string)

(defvar project-rsync-common-options
  (list "--rsh=ssh -oBatchMode=yes"
        "--recursive"
        "--links"
        "--times"
        "--compress"
        "--partial"
        "--progress"
        "--update"))

(defconst project-rsync-buffer-name "*project-rsync*")

(defun project-rsync-local-dir ()
  (let ((project (or (project-current)
                     (error "Current buffer is not a part of any project"))))
    (file-truename (project-root project))))

(defun project-rsync-project-filters ()
  (let ((project-vcs (vc-responsible-backend (project-rsync-local-dir))))
    (cond
     ((eq project-vcs 'Git)
      (list "--exclude=.git/"
            "--filter=:- .gitignore"))
     (t
      (error "Unsupported vcs '%s'" project-vcs)))))

(defun project-rsync--get-remote-projects-dir ()
  (file-name-as-directory
   (or project-rsync-remote-projects-dir
       (error "`project-rsync-remote-projects-dir' is not defined for current project"))))

(defun project-rsync--get-project-name ()
  (file-name-base (directory-file-name (project-rsync-local-dir))))

(defun project-rsync-remote-dir ()
  (let ((project-name (project-rsync--get-project-name)))
    (file-name-as-directory
     (file-name-concat (project-rsync--get-remote-projects-dir)
                       project-name))))

(defun project-rsync--rsync-sentinel (p e)
  (if (= 0 (process-exit-status p))
      (message "Done!")
    (message "Failed!")))

(defun project-rsync--run-rsync (extra-options from to)
  (let ((process (let ((default-directory (project-rsync-local-dir)))
                   (apply #'start-process
                          "rsync-process"
                          project-rsync-buffer-name
                          "rsync"
                          (append project-rsync-common-options
                                  project-rsync-additional-options
                                  (project-rsync-project-filters)
                                  extra-options
                                  (list from
                                        to))))))
    (set-process-sentinel process #'project-rsync--rsync-sentinel)))

;;;###autoload
(defun project-rsync-pull ()
  (interactive)
  (message "Pulling %s from rsync remote..." (project-rsync--get-project-name))
  (project-rsync--run-rsync (list)
                            (project-rsync-remote-dir)
                            (project-rsync-local-dir)))

;;;###autoload
(defun project-rsync-push ()
  (interactive)
  (message "Pushing %s to rsync remote..." (project-rsync--get-project-name))
  (project-rsync--run-rsync (list "--delete")
                            (project-rsync-local-dir)
                            (project-rsync-remote-dir)))

(provide 'project-rsync)
