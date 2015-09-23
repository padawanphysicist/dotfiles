;; mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-mu-binary "/usr/bin/mu")
(setq mu4e-maildir "~/.mail/gmail")
(setq mu4e-view-show-images t)
(setq mu4e-html2text-command "w3m -dump -T text/html")
(setq mu4e-view-prefer-html t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-get-mail-command "offlineimap -q")
(setq mu4e-update-interval 300)
(setq mu4e-attachment-dir  "~/0.inbox")
(setq mu4e-sent-messages-behavior 'delete)
(setq message-kill-buffer-on-exit t)
(setq mu4e-hide-index-messages t)
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
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


(setq mu4e-headers-seen-mark '("S" . "☑")) ;seen
(setq mu4e-headers-unseen-mark '("u" . "☐")) ; unseen
(setq mu4e-headers-flagged-mark '("F" .  "⚵"))  ;flagged
(setq mu4e-headers-new-mark '("N" .  "✉"))  ;new
(setq mu4e-headers-replied-mark '("R" . "↵")) ;replied
(setq mu4e-headers-passed-mark '("P" . "⇉")) ;passed
(setq mu4e-headers-encrypted-mark '("x" . "⚷")) ;encrypted
(setq mu4e-headers-signed-mark '("s" . "✍")) ;signed

(add-hook 'mu4e-compose-mode-hook
   (defun vct-compose ()
      "My settings for message composition."
      (flyspell-mode)))

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
