;;;; -*- mode: Emacs-Lisp -*-
;;;;
;;;; Dylan configuration

(maybe-install-packages '(dylan-mode))

(dime-setup '(dime-dylan dime-repl dime-compiler-notes-tree))
(setq dime-dylan-implementations
      '((opendylan ("/home/tree/tools/opendylan/bin/dswank")
         :env ("OPEN_DYLAN_USER_REGISTRIES=/home/tree/src/dylan-src/registry:/home/tree/tools/opendylan/sources/registry"))))
