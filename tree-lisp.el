;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Lisp configuration

(maybe-install-packages '(clojure-mode cider slamhound align-cljlet mic-paren paredit parenface))

(defun add-lisp-hook (func)
  (add-hooks '(emacs-lisp lisp clojure) func))

;;; General Goodness

(eldoc-mode 1)

(add-lisp-hook (lambda ()
                 (paredit-mode 1)))

(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 0)))

;; mic-paren
(paren-activate)
(setq paren-priority 'both)

(require 'parenface)
(require 'speedbar)

;;; Common Lisp

(speedbar-add-supported-extension ".lisp")

;; Slime is setup via QuickLisp
(setq slime-net-coding-system 'utf-8-unix)
(load "~/quicklisp/slime-helper.el")

(setq slime-lisp-implementations
      `((sbcl (,(expand-file-name "~/lisp/sbcl/bin/sbcl")
                "--core" ,(expand-file-name "~/lisp/sbcl/lib/sbcl/sbcl.core")
                "--dynamic-space-size" "2000")
              :env ,(concat "SBCL_HOME=" (expand-file-name "~/lisp/sbcl/lib/sbcl"))
              :coding-system utf-8-unix)))

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

(require 'clojure-mode)

(speedbar-add-supported-extension ".clj")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

; Cider configuration (https://github.com/clojure-emacs/cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(setq nrepl-hide-special-buffers t)
(setq cider-repl-print-length 100)




;; (eval-after-load "folding-mode"
;;   '(progn
;;     (folding-add-to-marks-list 'clojure-mode ";;{{{"  ";;}}}" nil t)))
