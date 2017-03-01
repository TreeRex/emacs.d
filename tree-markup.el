;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-
;;;;
;;;; Support for miscellaneous data and markup formats

(use-package yaml-mode)


;;; Support for UML diagrams via <http://plantuml.sourceforge.net/>

(use-package plantuml-mode
  :init
  ;; ick, I don't like specific paths like this... 
  (setq-default plantuml-jar-path "~/tools/plantuml.jar")
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))



