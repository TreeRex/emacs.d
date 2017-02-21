;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-
;;;;
;;;; Lisp configuration

(defun tree/add-lisp-hook (func)
  (tree/add-hooks '(lisp clojure emacs-lisp) func))

(global-eldoc-mode)

;; structured sexp editing <https://www.emacswiki.org/emacs/ParEdit>
(use-package paredit
  :config
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;; better highlighting of matching parens
(use-package mic-paren
  :config
  (paren-activate)
  (setq paren-priority 'both))

;; dim parens
(use-package paren-face
  :config
  (global-paren-face-mode))

;; indentation with extreme prejudice
(use-package aggressive-indent)

;; common modes across all lisp modes
(tree/add-lisp-hook (lambda ()
                      (paredit-mode 1)
                      (form-feed-mode)
                      (aggressive-indent-mode)))
(require 'speedbar)


;;; Clojure
(use-package company)

(use-package clojure-mode
  :config
  (define-clojure-indent
    ;; Compojure
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    ;; Midje
    (fact 'defun)
    (facts 'defun)
    (fact-group 'defun)
    (silent-fact 'defun)
    (future-fact 'defun)
    (tabular 'defun)
    (against-background 'defun)
    (provided 0)
    ;; Custom Macros
    (in-thread-context :defn))
  (speedbar-add-supported-extension ".clj")
  (add-hook 'clojure-mode-hook
            #'(lambda ()
                (company-mode)
                (clj-refactor-mode 1))))

(use-package clj-refactor
  :init
  (setq-default cljr-suppress-middleware-warnings t
                cljr-warn-on-eval nil)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package cider
  :config
  (setq cider-prompt-for-symbol nil
        cider-prompt-save-file-on-load 'always-save
        cider-repl-use-pretty-printing t
        cider-repl-prompt-function #'cider-repl-prompt-abbreviated
        cider-repl-display-help-banner nil
        nrepl-hide-special-buffers t
        nrepl-log-messages nil)
  (add-hook 'cider-repl-mode-hook
            #'(lambda ()
                (company-mode)
                (paredit-mode))))


;;; Common Lisp

;; CAVEAT HACKER: I haven't used CL in a while so the Slime config below is
;; probably broken.

;; (speedbar-add-supported-extension ".lisp")

;; Slime is setup via QuickLisp
;; (setq slime-net-coding-system 'utf-8-unix)
;; (load "~/quicklisp/slime-helper.el")

;; (setq slime-lisp-implementations
;;       `((sbcl (,(expand-file-name "~/lisp/sbcl/bin/sbcl")
;;                 "--core" ,(expand-file-name "~/lisp/sbcl/lib/sbcl/sbcl.core")
;;                 "--dynamic-space-size" "2000")
;;               :env ,(concat "SBCL_HOME=" (expand-file-name "~/lisp/sbcl/lib/sbcl"))
;;               :coding-system utf-8-unix)))

;; ;; use the local HyperSpec if it's available, otherwise LispWorks'.
;; (let ((local-hyperspec-root "~/Documents/HyperSpec/"))
;;   (when (file-directory-p local-hyperspec-root)
;;     (setq common-lisp-hyperspec-root (concat "file://" (expand-file-name local-hyperspec-root)))))

;; setup pathname translations for Slime when connecting to a remote Lisp
;; this assumes the slime-tramp contrib has been enabled
;; (add-hook 'slime-load-hook
;;           (lambda () (push (list "^ese-dev3$"
;;                                  (lambda (emacs-filename)
;;                                    (subseq emacs-filename (length "/scpc:ese-dev3:")))
;;                                  (lambda (emacs-filename)
;;                                    (concat "/scpc:ese-dev3:" emacs-filename)))
;;                            slime-filename-translations)) t)

;; (setq slime-startup-animation nil
;;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;;       lisp-indent-function 'common-lisp-indent-function)

;; (global-set-key "\C-cs" 'slime-selector)

