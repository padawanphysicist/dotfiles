;; mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; Contacts in org mode =D
(load "org-contacts.el")

(setq mu4e-mu-binary "/usr/bin/mu")
(setq mu4e-maildir "~/.mail/gmail")
(setq mu4e-view-show-images t)
(setq mu4e-html2text-command "w3m -dump -T text/html")
;(setq mu4e-view-prefer-html t)
;(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-get-mail-command "offlineimap -q")
(setq mu4e-update-interval 300)
(setq mu4e-attachment-dir  "~/0.inbox")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-trash-folder  "/trash")
(setq mu4e-sent-messages-behavior 'delete)
(setq message-kill-buffer-on-exit t)
(setq mu4e-hide-index-messages t)
(setq
 user-mail-address "victor.phb@gmail.com"
 user-full-name  "Victor Santos"
 mu4e-compose-signature
 (concat
  "vct\n"))

;; smtpmail
(require 'smtpmail)
(require 'starttls)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user "victor.phb@gmail.com")
;(setq starttls-extra-arguments '("--x509cafile" "/usr/pkg/share/ncat/ca-bundle.crt"))

(defun vct:mail-compose-hooks ()
  "Settings for message composition."
  (flyspell-mode)
  (turn-off-auto-fill)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (visual-line-mode 1))

(add-hook 'mu4e-compose-mode-hook 'vct:mail-compose-hooks)
(add-hook 'message-mode-hook 'vct:mail-compose-hooks)

(setq org-contacts-files '("~/1.documents/0.annotations/0.organizer.org"))
(setq mu4e-org-contacts-file  "~/1.documents/0.annotations/0.organizer.org")

(defun insert-emails-from-tags (tag-expression)
  "insert emails from org-contacts that match the tags expression. For example:
group-phd will match entries tagged with group but not with phd."
  (interactive "sTags: ")
  (insert
    (mapconcat 'identity
      (loop for contact in (org-contacts-filter)
        for contact-name = (car contact)
		  for email = (org-contacts-strip-link (car (org-contacts-split-property
            (or
              (cdr (assoc-string org-contacts-email-property
                (caddr contact)))
                  ""))))
                for tags = (cdr (assoc "TAGS" (nth 2 contact)))
		          for tags-list = (if tags
					(split-string (substring (cdr (assoc "TAGS" (nth 2 contact))) 1 -1) ":")
				      '())
		    if (let ((todo-only nil))
			 (eval (cdr (org-make-tags-matcher tag-expression))))
		    collect (org-contacts-format-email contact-name email))
	      ",")))
