;;;; -*- Mode: Emacs-Lisp -*-

;;;; The reorganization of my Emacs init is based on Bodil Stokke's emacs.d
;;;; found at https://github.com/bodil/emacs.d
;;;;
;;;; Parts have been borrowed from her configuration as well.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(when (< emacs-major-version 24)
  (error "Initialization hasn't been tested versions of Emacs older than 24.x"))

;; load common functions ahead of everything else
(load "~/.emacs.d/tree-defuns.el")

(dolist (m '(tool-bar-mode scroll-bar-mode))
  (when (fboundp m)
    (funcall m -1)))

(setq inhibit-startup-message t)	;I know it's emacs, silly...

(setq frame-title-format "%b %+%+ %f %n")

;;; Ubiquitous Unicode

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")

;;; Package manager support

(require 'package)

(dolist (p '(("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.org/packages/")
             ("org" . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives p t))

(package-initialize)

(maybe-install-packages '(ag multiple-cursors uuid smart-mode-line
                             shrink-whitespace
                             highlight-chars))

(require 'highlight-chars)

;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)

(global-page-break-lines-mode)
(add-to-list 'page-break-lines-modes 'clojure-mode)
(yas-global-mode t)

;; ag configuration
(setq ag-reuse-buffers t)

;; multiple-cursor configuration
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq user-full-name "Tom Emerson")
(setq user-mail-address "temerson@ebsco.com")

;; Any packages not contained in a package archive are put into ~/emacs/
(add-to-list 'load-path (expand-file-name "~/emacs"))

(dolist (p (list "/usr/local/bin" (expand-file-name "~/bin")))
  (add-to-list 'exec-path p))

; I know some files are really large, big deal.
(setq large-file-warning-threshold nil)

(setq-default indicate-empty-lines t
              require-trailing-newline t
              truncate-lines nil
              ediff-diff-options "-w"
              indent-tabs-mode nil
              yank-excluded-properties t)

(setq-default ispell-program-name "aspell")

(global-unset-key "\M-t")
(global-set-key "\M-t" 'indent-relative)

;(require 'doxygen "doxygen" t)
;(require 'ep-utils "ep-utils" t)

;; better handling for buffers editing the same file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; use ibuffer
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


;; (setq display-time-day-and-date nil
;;       display-time-24hr-format t
;;       display-time-format "%R (%d %b)")
;; (display-time)

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

(setq compilation-read-command nil)     ;don't ask me for the compiler command-line
(global-set-key [f7] 'compile)

;;; Font-lock stuff
(global-font-lock-mode)
(setq font-lock-maximum-decoration '((c-mode . 1) (t . 3) (clojure-mode . t)))
;(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)

;;; Key bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\e\e" 'eval-expression)
; from http://whattheemacsd.com/
(global-set-key (kbd "M-j")
            (lambda ()
              (interactive)
              (join-line -1)))

(global-set-key (kbd "C-\\") 'shrink-whitespace)

(put 'eval-expression 'disabled nil)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-to-list 'custom-theme-load-path (expand-file-name "~/emacs/emacs-color-theme-solarized"))

(when (display-graphic-p)
  (load-theme 'zenburn t))

;; smart-mode-line
(sml/setup)
;(sml/apply-theme 'light)

(dolist (rl '(("^~/Work/" ":W:")
              ("^~/Projects/" ":P:")))
  (add-to-list 'sml/replacer-regexp-list rl t))

(auto-insert-mode)
(setq auto-insert-query nil)



;; (add-to-list 'default-frame-alist
;;              ;; my old eyes need a bigger font when I'm at my desk on a big screen
;;              (if (and (= (display-pixel-width) 1920)
;;                       (= (display-pixel-height) 1200))
;;                  (cons 'font "Source Code Pro-18:weight=medium")
;;                  (cons 'font "Source Code Pro-14:weight=medium")))
;;

(setq default-frame-alist (append '((font . "Inconsolata-24:weight=medium")
                                    (width . 132)
                                    (height . 45))
                                  default-frame-alist))



(dolist (file '(
                "tree-advice.el"
                "tree-cpp.el"
                "tree-dired.el"
;                "tree-dylan.el"
                "tree-folding.el"
                ;"tree-gtags.el"
;                "tree-helm.el"
                "tree-javascript.el"
                "tree-lisp.el"
                "tree-markdown.el"
                "tree-octave.el"
                "tree-org.el"
                "tree-projectile.el"
                "tree-semanticweb.el"
                "tree-term.el"
                "tree-vcs.el"
                "tree-xml.el"
                "tree-speedbar.el"))
  (load (concat "~/.emacs.d/" file) t))

(when (display-graphic-p)
    (server-start))


