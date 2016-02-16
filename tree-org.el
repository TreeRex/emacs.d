;;;; -*- Mode: Emacs-Lisp

(require 'speedbar)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cb" 'org-iswitchb)
(setq org-clock-idle-time 10
      org-log-done 'time)

(setq org-export-backends '(ascii html latex confluence md))

(setq org-todo-keywords
      '((sequence "TODO" "ONGOING" "|" "DONE")
        (sequence "QUESTION" "|" "ANSWERED")
        (sequence "TASK" "|" "COMPLETE")))

(setq org-babel-awk-command "gawk")
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (sh . t)
                               (awk . t)
                               (clojure . t)))

(add-hook 'org-mode-hook (lambda ()
                           (visual-line-mode)
                           (speedbar-add-supported-extension ".org")))

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

(define-auto-insert 'org-mode 'org-boilerplate)
