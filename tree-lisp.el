;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Lisp configuration


(maybe-install-packages '(clojure-mode mic-paren paredit paren-face dash cider
                          clj-refactor))

(defun add-lisp-hook (func)
  (add-hooks '(lisp clojure emacs-lisp) func))

;;; General Goodness

(eldoc-mode 1)

(add-lisp-hook (lambda ()
                 (paredit-mode 1)))

;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 0)))

;; mic-paren
(paren-activate)
(setq paren-priority 'both)

(global-paren-face-mode)
(require 'speedbar)

;;; Common Lisp

(speedbar-add-supported-extension ".lisp")

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

;;; Clojure

(require 'clojure-mode)

(setq-default cljr-suppress-middleware-warnings t)

(speedbar-add-supported-extension ".clj")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c m")))

(when (fboundp 'folding-add-to-marks-list)
  (folding-add-to-marks-list 'clojure-mode ";;{{{" ";;}}}"))

;; Cider configuration (https://github.com/clojure-emacs/cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(setq nrepl-hide-special-buffers nil)
(setq nrepl-log-messages nil)
(setq cider-repl-use-pretty-printing t)

(define-clojure-indent
  ;; Compojure
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

