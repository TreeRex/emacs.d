;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-

(require 'speedbar)

;;; Org is a RPITA because it is built into Emacs and is loaded by
;;; (package-initialize) before the use-package call here. See
;;; <https://github.com/jwiegley/use-package/issues/319> for work-arounds and
;;; stuff.
;;;
;;; Possible future additions:
;;;   - Projectile integration: https://github.com/IvanMalison/org-projectile
;;; These are part of the org+contrib repo, see init.el
;;;   - Decision management: http://orgmode.org/worg/org-contrib/org-choose.html
;;;   - Git links: http://orgmode.org/worg/org-contrib/org-git-link.html

(use-package org
  :ensure org-plus-contrib
  :bind (("\C-cc" . org-capture)) 
  :config
  (setq org-clock-idle-time 10
        org-default-notes-file "~/org/notes.org"
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

;;; -- I have no idea how to make this work, and I can't find any examples that work.
;; (use-package org-choose
;;   :ensure org-plus-contrib)
