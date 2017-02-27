;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-

;;; ido-mode (build-in) configuration
;;;
;;; ido and helm don't play well together: pick one or the other

(setq-default ido-create-new-buffer 'always)
(ido-mode 1)
(ido-everywhere 1)

(use-package flx-ido
  :config
  (flx-ido-mode 1))

