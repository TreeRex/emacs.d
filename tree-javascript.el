;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-

(require 'speedbar)
(speedbar-add-supported-extension ".js")

;; <https://github.com/mooz/js2-mode>
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js" . js2-mode)))

;; <https://github.com/joshwnj/json-mode>
(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json" . json-mode)))
