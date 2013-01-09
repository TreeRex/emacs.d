;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; XML configuration

(maybe-install-packages '(rnc-mode))

(setq rnc-enable-imenu t)
(setq rnc-enable-flymake t)
(setq rnc-jing-jar-file "/Users/tree/src/jing-20091111/bin/jing.jar")
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))

(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
(setq rng-schema-locating-files (list (expand-file-name "~/schemas/schemas.xml") "schemas.xml"))
