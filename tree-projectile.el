;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-

(use-package projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories ".svn")
  (projectile-global-mode))
