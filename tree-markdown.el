;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-

(require 'speedbar)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))

  :config
  (speedbar-add-supported-extension ".md")
  (add-hook 'markdown-mode-hook (lambda ()
                                  (visual-line-mode))))



