;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Lisp configuration

(maybe-install-packages '(clojure-mode mic-paren nrepl paredit parenface pretty-mode))

(require 'pretty-mode)

(defun add-lisp-hook (func)
  (add-hooks '(emacs-lisp lisp clojure) func))

;;; General Goodness

(add-lisp-hook (lambda ()
                 (paredit-mode 1)
                 (pretty-mode 1)))

(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 0)))

;; mic-paren
(paren-activate)
(setq paren-priority 'both)

(require 'parenface)

;;; Common Lisp

;; Slime is setup via QuickLisp
(setq slime-net-coding-system 'utf-8-unix)
(load "~/quicklisp/slime-helper.el")

(setq slime-lisp-implementations
      '((sbcl ("/home/tree/lisp/sbcl/bin/sbcl") :coding-system utf-8-unix)))

;; use the local HyperSpec if it's available, otherwise LispWorks'.
(let ((local-hyperspec-root "~/Documents/HyperSpec/"))
  (when (file-directory-p local-hyperspec-root)
    (setq common-lisp-hyperspec-root (concat "file://" (expand-file-name local-hyperspec-root)))))

;; setup pathname translations for Slime when connecting to a remote Lisp
;; this assumes the slime-tramp contrib has been enabled
;; (add-hook 'slime-load-hook
;;           (lambda () (push (list "^ese-dev3$"
;;                                  (lambda (emacs-filename)
;;                                    (subseq emacs-filename (length "/scpc:ese-dev3:")))
;;                                  (lambda (emacs-filename)
;;                                    (concat "/scpc:ese-dev3:" emacs-filename)))
;;                            slime-filename-translations)) t)

(setq slime-startup-animation nil
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

(eval-after-load "folding-mode"
  '(progn
    (folding-add-to-marks-list 'clojure-mode ";;{{{"  ";;}}}" nil t)))
