;;;; -*- mode: Emacs-Lisp -*-
;;;;
;;;; Mail configuration

;;; mu4e Configuration

(add-to-list 'load-path (expand-file-name "~/emacs/mu4e"))
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Exchange"))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "exchange.corp.epnet.com"
      smtpmail-smtp-server "exchange.corp.epnet.com"
      smtpmail-local-domain "ebscohost.com")

(setq mu4e-get-mail-command "offlineimap"
      mu4e-html2text-command "html2text -nobs -utf8 -width 72")

(setq mu4e-reply-to-address "temerson@ebscohost.com"
      mu4e-user-mail-address-list '("temerson@ebscohost.com")
      user-mail-address "temerson@ebscohost.com")

(add-hook 'mu4e-compose-mode-hook
          (lambda () (message-add-header "X-Attribution: tree\n")))

(setq mu4e-refile-folder
      (lambda (msg)
        (cond
          ;; status messages go to /status
          ((and (mu4e-message-contact-field-matches msg :to "BBuckley@ebscohost.com")
                (string-match "^Status, " (or (mu4e-message-field msg :subject) "")))
           "/status")
          (t "/archive"))))

