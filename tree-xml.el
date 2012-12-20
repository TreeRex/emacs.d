;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; XML configuration

(maybe-install-packages '(rnc-mode))

(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
(setq rng-schema-locating-files (list (expand-file-name "~/schemas/schemas.xml") "schemas.xml"))

