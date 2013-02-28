;;;; -*- Mode: Emacs-Lisp -*-

;;;; The reorganization of my Emacs init is based on Bodil Stokke's emacs.d
;;;; found at https://github.com/bodil/emacs.d
;;;;
;;;; Parts have been borrowed from her configuration as well.

(when (< emacs-major-version 24)
  (error "Initialization hasn't been tested versions of Emacs older than 24.x"))

(dolist (m '(tool-bar-mode scroll-bar-mode))
  (when (fboundp m)
    (funcall m -1)))

(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(setq inhibit-startup-message t)	;I know it's emacs, silly...

;;; Ubiquitous Unicode

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")

;;; Package manager support

(require 'package)

(dolist (p '(("melpa" . "http://melpa.milkbox.net/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages")
             ("org" . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives p t))

(package-initialize)

(setq user-full-name "Tom Emerson")

;; Any packages not contained in a package archive are put into ~/emacs/
(add-to-list 'load-path (expand-file-name "~/emacs"))
(add-to-list 'exec-path "/usr/local/bin")

; I know some files are really large, big deal.
(setq large-file-warning-threshold nil)

(global-unset-key "\M-t")
(global-set-key "\M-t" 'indent-relative)

(require 'doxygen)
;(require 'ep-utils "eputils" t)
(require 'ep-utils)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; better handling for buffers editing the same file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (setq display-time-day-and-date nil
;;       display-time-24hr-format t
;;       display-time-format "%R (%d %b)")
;; (display-time)

;(blink-cursor-mode 1)

(column-number-mode t)
(show-paren-mode t)
(transient-mark-mode t)                 ;show region between point & mark

;(setq message-log-max nil)
;(kill-buffer "*Messages*")

(if (null window-system)
    (global-set-key "\C-h" 'delete-backward-char))

(setq make-backup-files t)

(setq require-final-newline 'ask)
(setq backup-by-copying-when-linked t)  ;preserve links!
(setq-default indent-tabs-mode nil)	;tabs are evil

(setq-default ediff-diff-options "-w")

(setq compilation-read-command nil)     ;don't ask me for the compiler command-line
(global-set-key [f7] 'compile)

;;; Font-lock stuff
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
(setq font-lock-maximum-decoration '((c-mode . 1) (t . 3)))

;;; Key bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\e\e" 'eval-expression)
(put 'eval-expression 'disabled nil)

; Kyle Jones's filladapt package
;(require 'filladapt)
;(add-hook 'text-mode-hook 'turn-on-filladapt-mode)

(setq c-default-style "stroustrup")

(defun my-c-mode-common-hook ()
;  (c-setup-filladapt)
;  (c-toggle-auto-hungry-state 1)
  (setq tab-width 4
        indent-tabs-mode nil
        comment-column 45)
  (c-set-offset 'member-init-intro '++))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(when window-system
  (load-theme 'wombat t))


;; (add-to-list 'default-frame-alist
;;              ;; my old eyes need a bigger font when I'm at my desk on a big screen
;;              (if (and (= (display-pixel-width) 1920)
;;                       (= (display-pixel-height) 1200))
;;                  (cons 'font "Source Code Pro-18:weight=medium")
;;                  (cons 'font "Source Code Pro-14:weight=medium")))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-14:weight=medium"))

(dolist (file '("private.el"
                "tree-defuns.el"
                "tree-advice.el"
                "tree-dired.el"
                "tree-folding.el"
                "tree-gtags.el"
                "tree-lisp.el"
                "tree-mail.el"
                "tree-markdown.el"
                "tree-org.el"
                "tree-semanticweb.el"
                "tree-xml.el"))
  (load (concat "~/.emacs.d/" file)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
