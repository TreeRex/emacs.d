;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; XML configuration

(maybe-install-packages '(rnc-mode))

;; sparql-mode isn't handled by any package managers, and I haven't setup GIT externals
;; yet to pull the version I use, so you will need to grab it yourself from GitHub
;;
;; https://github.com/candera/sparql-mode
;;
;; and put it in ~/emacs.

(autoload 'sparql-mode "sparql-mode.el")
(add-to-list 'auto-mode-alist '("\\.rq\\'" . sparql-mode))

(setq rnc-enable-imenu t)
(setq rnc-enable-flymake t)
(setq rnc-jing-jar-file "/Users/tree/src/jing-20091111/bin/jing.jar")
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))

(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
(setq rng-schema-locating-files (list (expand-file-name "~/schemas/schemas.xml") "schemas.xml"))
