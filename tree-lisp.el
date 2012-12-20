;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Lisp configuration

(maybe-install-packages '(clojure-mode mic-paren nrepl paredit parenface))

(defun add-lisp-hook (func)
  (add-hooks '(emacs-lisp lisp clojure) func))

;;; General Goodness

(add-lisp-hook (lambda () (paredit-mode 1)))

;; mic-paren
(paren-activate)
(setq paren-priority 'both)

(require 'parenface)

;;; Common Lisp

;; Slime is setup via QuickLisp
(setq slime-net-coding-system 'utf-8-unix)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/Users/temerson/bin/ccl -K utf-8"
      common-lisp-hyperspec-root "file:///Users/temerson/Documents/HyperSpec/"
      slime-startup-animation nil
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      lisp-indent-function 'common-lisp-indent-function)

(global-set-key "\C-cs" 'slime-selector)

;;; Clojure

(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(autoload 'nrepl-jack-in "clojure-mode" nil t)
(eval-after-load "clojure-mode" '(require 'nrepl))
(setq nrepl-lein-command "lein"
      nrepl-server-command "echo \"lein repl :headless\" | $SHELL -l")

