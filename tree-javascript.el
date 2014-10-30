;;; -*- Mode: Emacs-Lisp -*-

(require 'speedbar)

(speedbar-add-supported-extension ".js")

(maybe-install-packages '(js2-mode json-mode))
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json" . json-mode))
