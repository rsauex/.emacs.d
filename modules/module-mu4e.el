;; -*- lexical-binding: t; -*-

(use-package mu4e
  :when (executable-find "mu")
  :ensure nil
  :custom-local
  (mu4e-view-mode . ((cursor-type 'bar)
                     (my-inhibit-side-window-special t)))
  (mu4e-main-mode . ((cursor-type nil)
                     (global-hl-line-mode nil)
                     (my-inhibit-side-window-special t)))
  (mu4e-compose-mode . ((my-inhibit-side-window-special t)))
  (mu4e-headers-mode . ((my-inhibit-side-window-special t)))
  :custom-face
  (mu4e-header-highlight-face ((t :inherit hl-line :underline nil :weight bold :extend t)))
  :custom
  ;; Use msmtp to send mail
  (sendmail-program (executable-find "msmtp-private-config"))
  (send-mail-function #'sendmail-send-it)
  ;; Use header from as envelope from
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  ;; Use Mu4e to compose emails
  (mail-user-agent 'mu4e-user-agent)
  (message-mail-user-agent 'mu4e-user-agent)
  ;; Don't sync mail automatically
  (mu4e-update-interval nil)
  ;; Use ordinary `completing-read' for completion
  (mu4e-completing-read-function #'completing-read)
  ;; Make mbsync happy
  (mu4e-change-filenames-when-moving t)
  ;; TODO: don't use absolute path
  ;; How to sync mail
  (mu4e-get-mail-command (string-join (list (executable-find "mbsync-private-config") "-a") " "))
  ;; Change how headers are displayed
  (mu4e-headers-fields '((:human-date    .   12)
                         (:flags         .    6)
                         (:from-or-to    .   22)
                         (:fixed-subject .   nil)))
  (mu4e-headers-from-or-to-prefix '("" . "To: "))
  (mu4e-use-fancy-chars t)
  (mu4e-headers-draft-mark     '("D" . "D"))
  (mu4e-headers-flagged-mark   '("F" . "F"))
  (mu4e-headers-new-mark       '("N" . "N"))
  (mu4e-headers-passed-mark    '("P" . "P"))
  (mu4e-headers-replied-mark   '("R" . "R"))
  (mu4e-headers-seen-mark      '("S" . "S"))
  (mu4e-headers-trashed-mark   '("T" . "T"))
  (mu4e-headers-attach-mark    '("a" . "a"))
  (mu4e-headers-encrypted-mark '("x" . "x"))
  (mu4e-headers-signed-mark    '("s" . "s"))
  (mu4e-headers-unread-mark    '("u" . "u"))
  (mu4e-headers-list-mark      '("l" . "l"))
  (mu4e-headers-personal-mark  '("p" . "p"))
  (mu4e-headers-calendar-mark  '("c" . "c"))
  (mu4e-headers-thread-root-prefix          '("* "  . "□ "))
  (mu4e-headers-thread-duplicate-prefix     '("= "  . "≡ "))
  (mu4e-headers-thread-single-orphan-prefix '("-- " . "─▶ "))
  (mu4e-headers-thread-orphan-prefix        '(",- " . "┬▶ "))
  (mu4e-headers-thread-connection-prefix    '("|  " . "│  "))
  (mu4e-headers-thread-first-child-prefix   '("|- " . "├▶ "))
  (mu4e-headers-thread-child-prefix         '("|- " . "├▶ "))
  (mu4e-headers-thread-last-child-prefix    '("`- " . "╰▶ "))
  (mu4e-headers-thread-blank-prefix         '("   " . "   "))
  ;; Store attachments to the Downloads dir, same as web clients
  (mu4e-attachment-dir "~/Downloads")
  ;; Show all the messages
  (mu4e-headers-skip-duplicates nil)
  ;; One of the mail servers I use sends out wired strange IMAP
  ;; responses which make mbsync exit with non-zero
  (mu4e-index-update-error-continue t)
  ;; Don't use flowed format
  (mu4e-compose-format-flowed nil)
  ;; Use the first context as a default
  (mu4e-context-policy 'pick-first)
  :init
  (defun my--mu4e-subject-without-newlines (msg)
    (let ((text (concat
                 ;; prefix subject with a thread indicator
                 (mu4e~headers-thread-prefix (mu4e-message-field msg :meta))
                 ;;  "["(plist-get (mu4e-message-field msg :meta) :path) "] "
                 ;; work-around: emacs' display gets really slow when lines are too long;
                 ;; so limit subject length to 600
                 (truncate-string-to-width (mu4e-message-field msg :subject) 600))))
      (replace-regexp-in-string "\n\\|\r" "" text)))

  (defun my-mu4e--trash-folders ()
    (mapcar (lambda (context)
              (alist-get 'mu4e-trash-folder (mu4e-context-vars context)))
            mu4e-contexts))

  (defun my-mu4e-sexp->query (sexp)
    (pcase sexp
      (`(:and . ,sexps)
       (concat "(" (mapconcat #'my-mu4e-sexp->query sexps " AND ") ")"))
      (`(:or . ,sexps)
       (concat "(" (mapconcat #'my-mu4e-sexp->query sexps " OR ") ")"))
      (`(:not ,sexp)
       (concat "(NOT " (my-mu4e-sexp->query sexp) ")"))
      (`,value (concat "(" value ")"))))

  (defun my-mu4e--and-not-in-trash (sexp)
    `(:and ,sexp (:not "maildir://[Tt]rash/")))

  (define-popup-frame-fns mu4e
    (mu4e)
    (mu4e-quit))

  (define-popup-frame-fns message
    (browse-url-mail compose-mail)
    (message-send-and-exit message-kill-buffer))

  ;; Start mu4e in background when running in daemon mode
  (when (daemonp)
    (add-hook 'after-init-hook (lambda () (mu4e t))))

  :config
  ;; Contexts:
  (require 'my-mu4e-contexts)

  ;; Add a subject header with newlines removed
  (setf (alist-get :fixed-subject mu4e-header-info-custom)
        (list :name "Subject"
              :shortname "Subj"
              :help "Subject without newlines"
              :function #'my--mu4e-subject-without-newlines))

  (csetq
    (mu4e-bookmarks (list (list :name "Unread messages"
                                :query (my-mu4e-sexp->query
                                        (my-mu4e--and-not-in-trash
                                         `(:and "flag:unread" (:not "flag:trashed"))))
                                :key ?u)
                          (list :name "Today's messages"
                                :query (my-mu4e-sexp->query
                                        (my-mu4e--and-not-in-trash
                                         "date:today..now"))
                                :key ?t)
                          (list :name "Last 7 days"
                                :query (my-mu4e-sexp->query
                                        (my-mu4e--and-not-in-trash
                                         "date:7d..now"))
                                :hide-unread t
                                :key ?w))))

  ;; Don't set 'trashed' flag on mails moved to a Trash folder
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
	                (mu4e--server-move docid (mu4e--mark-check-target target) "-N"))))

  ;; Prefer plain messages
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))

  ;; Don't stop mu server when running in daemon
  (define-advice mu4e-quit
      (:filter-args (&optional bury) dont-stop-on-server)
    (list (if server-process t bury))))

;; ICalendar
;; ...

;; Org
;; ...

;; (use-package org-mime
;;   :ensure t
;;   :init
;;   (defun my-org-mime-edit-mail-in-org-mode ()
;;     "Call a special editor to edit the mail body in `org-mode'."
;;     (interactive)
;;     (setq org-mime--saved-temp-window-config (current-window-configuration))
;;     (let* ((beg (copy-marker (org-mime-mail-body-begin)))
;;            (end (copy-marker (or (org-mime-mail-signature-begin) (point-max))))
;;            (bufname "OrgMimeMailBody")
;;            (buffer (generate-new-buffer bufname))
;;            (overlay (org-mime-src--make-source-overlay beg end))
;;            (text (buffer-substring-no-properties beg end)))

;;       (setq org-mime-src--beg-marker beg)
;;       (setq org-mime-src--end-marker end)
;;       ;; don't use local-variable because only user can't edit multiple emails
;;       ;; or multiple embedded org code in one mail
;;       (setq org-mime-src--overlay overlay)

;;       (with-current-buffer buffer
;;         (erase-buffer)
;;         (insert org-mime-src--hint)
;;         (insert text)
;;         (goto-char (point-min))
;;         (org-mode)
;;         (org-mime-src-mode))

;;       (display-buffer buffer '(display-buffer-same-window)))))

;; (require 'org-mime)

(provide 'module-mu4e)
