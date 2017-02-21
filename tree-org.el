;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-

(require 'speedbar)

(use-package org
  :commands (org-store-link)
  :bind (("\C-cl" . org-store-link))
  :config
  (setq org-clock-idle-time 10
        org-log-done 'time
        org-babel-awk-command (if (string-equal system-type "darwin") "gawk" "awk")
        org-export-backends '(ascii html latex confluence md)
        org-todo-keywords '((sequence "TODO" "ONGOING" "|" "DONE")
                            (sequence "QUESTION(q)" "|" "ANSWERED(a)")
                            (sequence "TASK" "|" "COMPLETE")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (sh . t)
                                 (awk . t)
                                 (clojure . t)))
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode)
                             (speedbar-add-supported-extension ".org")))

  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  
  (define-skeleton org-boilerplate
    "My common Org file header"
    nil
    "#+TITLE:   \n"
    "#+AUTHOR:  " (progn user-full-name) "\n"
    "#+EMAIL:   " (progn user-mail-address) "\n"
    "#+STARTUP: showall\n"
    "\n\n\n\n"
    "# Local Variables:\n"
    "# mode: org\n"
    "# coding: utf-8\n"
    "# End:\n")

  (define-auto-insert 'org-mode 'org-boilerplate))







