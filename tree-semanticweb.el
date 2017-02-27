;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-
;;;;
;;;; Configuration for semantic-web related modes

;; <https://github.com/ljos/sparql-mode>
(use-package sparql-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))
  (add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode)))

;; <https://bitbucket.org/nxg/ttl-mode>
(use-package ttl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ttl\\'" . ttl-mode))
  (add-to-list 'auto-mode-alist '("\\.n3\\'" . ttl-mode)))
