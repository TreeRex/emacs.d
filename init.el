;;;; -*- mode:emacs-lisp; lexical-binding:t -*-

;;;; CAREFUL: THIS IS A MAJOR REORGANIZATION OF MY EMACS CONFIGURATION

;;;; The reorganization of my Emacs init is based on Bodil Stokke's emacs.d
;;;; found at https://github.com/bodil/emacs.d
;;;;
;;;; Parts have been borrowed from her configuration as well.

;;; https://github.com/jwiegley/use-package
;;; https://github.com/CSRaghunandan/.emacs.d/blob/master/init.el
;;; https://github.com/angrybacon/dotemacs

(when (version< emacs-version "24")
  (unless (yes-or-no-p "Your Emacs old and may fail. Continue? ")
    (kill-emacs)))

;;; Effectively disable GC during initialization
(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

;;; Store customized variables in their own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror 'nomessage)

;;; Common functions ahead of everything else
(load "~/.emacs.d/tree-defuns.el")

;;; ??? Where should I group these?
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)	;I know it's emacs, silly...

(setq frame-title-format "%b %+%+ %f %n")

;;; Ubiquitous Unicode

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")

;;; Package manager support.
(require 'package)
(dolist (p '(("melpa" . "http://melpa.org/packages/")
             ("org" . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives p t))
(package-initialize)

;;; safely load use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'diminish)                     ;built-in
(setq-default use-package-always-ensure t)

;;;;
;;;; use-package only from here on
;;;;

;; this requires that The Silver Searcher be in the path
(use-package ag                         ;search
  :init
  (setq ag-reuse-buffers t))

(use-package multiple-cursors           ;editing
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package shrink-whitespace          ;editing
  :bind (("C-\\" . shrink-whitespace)))

(use-package uuid)                      ;programming
(use-package smart-mode-line            ;visual/modeline
  :config
  (sml/setup)
  (sml/apply-theme 'light)
  (dolist (rl '(("^~/Work/" ":W:")
              ("^~/Projects/" ":P:")))
    (add-to-list 'sml/replacer-regexp-list rl t)))


;; displays  as a horizontal line (an alternate implementation is
;; page-break-lines)
(use-package form-feed)                 ;visual/buffer

(global-hl-line-mode)                   ;visual/buffer (built-in)


;;;
;;; ido-mode (built-in) I seem to have commented this out, so I'm not
;;; going to do anything with it now

;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)

;;;
;;; YASnippet
;;;

(use-package yasnippet
  :config
  (yas-global-mode t))


(setq user-full-name "Tom Emerson")
;; this should be set based on which machine I'm working on
(setq user-mail-address "temerson@ebsco.com")

;; Any packages not contained in a package archive are put into ~/emacs/
(add-to-list 'load-path (expand-file-name "~/emacs"))

(unless (string-equal system-type "windows-nt")
  (dolist (p (list "/usr/local/bin" (expand-file-name "~/bin")))
    (add-to-list 'exec-path p)))

; I know some files are really large, big deal.
(setq large-file-warning-threshold nil)

(setq-default indicate-empty-lines t
              require-trailing-newline t
              truncate-lines nil
              indent-tabs-mode nil
              yank-excluded-properties t)

; macOS and Linux only?
(setq-default ispell-program-name "aspell")

(global-unset-key "\M-t")
(global-set-key "\M-t" 'indent-relative)

;; better handling for buffers editing the same file name (built-in)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;
;;; Use ibuffer for better buffer browsing (built-in)
;;;
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*completions\\*$")))
         ("dired" (mode . dired-mode))
         ("writing" (or (mode . org-mode)
                        (mode . markdown-mode)
                        (mode . text-mode)))
         ("lisp" (or (mode . clojure-mode)
                     (mode . lisp-mode)
                     (mode . emacs-lisp-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))


(blink-cursor-mode 1)
(column-number-mode t)
(show-paren-mode t)
(transient-mark-mode t)                 ;show region between point & mark

(unless (display-graphic-p)
  (global-set-key "\C-h" 'delete-backward-char))

(setq make-backup-files t)
(setq require-final-newline t)
(setq backup-by-copying-when-linked t)  ;preserve links!
(setq default-tab-width 4)

;;;
;;; Font-lock stuff
;;;
(global-font-lock-mode)
(setq font-lock-maximum-decoration t)

;; provides toggle-highlight-tabs and toggle-highlight-trailing-whitespace 
(use-package highlight-chars)           ;visual/buffer


;;; Key bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\e\e" 'eval-expression)
; from http://whattheemacsd.com/
(global-set-key (kbd "M-j")
            (lambda ()
              (interactive)
              (join-line -1)))

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; darkokai
;;; monokai
;;; zenburn

;; (when (display-graphic-p)
;;   (load-theme 'zenburn t))

;(auto-insert-mode)
;(setq auto-insert-query nil)



;; (add-to-list 'default-frame-alist
;;              ;; my old eyes need a bigger font when I'm at my desk on a big screen
;;              (if (and (= (display-pixel-width) 1920)
;;                       (= (display-pixel-height) 1200))
;;                  (cons 'font "Source Code Pro-18:weight=medium")
;;                  (cons 'font "Source Code Pro-14:weight=medium")))
;;

;; (setq default-frame-alist (append '((font . "Inconsolata-24:weight=medium")
;;                                     (width . 132)
;;                                     (height . 40))
;;                                   default-frame-alist))

;(setq plantuml-jar-path "~/tools/plantuml.jar")

;; (dolist (file '(
;;                 "tree-advice.el"
;;                 "tree-cpp.el"
;;                 "tree-dired.el"
;; ;                "tree-dylan.el"
;;                 "tree-folding.el"
;;                 ;"tree-gtags.el"
;; ;                "tree-helm.el"
;;                 "tree-javascript.el"
;;                 "tree-lisp.el"
;;                 "tree-markdown.el"
;;                 "tree-octave.el"
;;                 "tree-org.el"
;;                 "tree-projectile.el"
;;                 "tree-semanticweb.el"
;;                 "tree-term.el"
;;                 "tree-vcs.el"
;;                 "tree-xml.el"
;;                 "tree-speedbar.el"))
;;   (load (concat "~/.emacs.d/" file) t))

;; (when (display-graphic-p)
;;     (server-start))


(setq gc-cons-threshold gc-cons-threshold--orig)
