;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Configuration for semantic-web related modes

;; sparql-mode isn't handled by any package managers, and I haven't setup GIT externals
;; yet to pull the version I use, so you will need to grab it yourself from GitHub
;;
;; https://github.com/candera/sparql-mode
;;
;; and put it in ~/emacs.

(autoload 'sparql-mode "sparql-mode.el")
(add-to-list 'auto-mode-alist '("\\.sparql\\'" . sparql-mode))

;; ttl-mode isn't handled by any package managers and his hosted on BitBucket,
;; so you have to pull it yourself and put ttl-mode.el in ~/emacs.
;;
;; https://bitbucket.org/nxg/ttl-mode

(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
(add-hook 'ttl-mode-hook 'turn-on-font-lock)
(add-to-list 'auto-mode-alist '("\\.ttl\\'" . ttl-mode))
