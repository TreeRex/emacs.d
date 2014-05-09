;;;; -*- mode: EmacsLisp -*-

(require 'speedbar)
(maybe-install-packages '(markdown-mode))

(speedbar-add-supported-extension ".md")

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
                                (visual-line-mode)))



