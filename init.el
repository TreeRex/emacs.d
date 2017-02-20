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
(load (expand-file-name "tree-defuns.el" user-emacs-directory))

;;; Load .secret.el if it exists. That file should not be put into git.
;;; Use it to hold sensitive information like your user-mail-address.
(let ((secret.el (expand-file-name ".secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))


;;; Default values

(tool-bar-mode 'yuck)
(scroll-bar-mode 'ick)
(blink-cursor-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(global-hl-line-mode 1)

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq frame-title-format "%b %+%+ %f %n"
      make-backup-files t
      require-final-newline t
      backup-by-copying-when-linked t
      tab-width 4
      inhibit-startup-message t
      large-file-warning-threshold nil)

;(setq plantuml-jar-path "~/tools/plantuml.jar")

;; Any packages not contained in a package archive are put into ~/emacs/
(add-to-list 'load-path (expand-file-name "~/emacs"))

(unless (string-equal system-type "windows-nt")
  (dolist (p (list "/usr/local/bin" (expand-file-name "~/bin")))
    (add-to-list 'exec-path p)))

(setq-default indicate-empty-lines t
              require-trailing-newline t
              truncate-lines nil
              indent-tabs-mode nil
              yank-excluded-properties t
              ispell-program-name "aspell")


;;; Ubiquitous Unicode

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")


;;; Custom key bindings
(global-unset-key "\M-t")               ;necessary?
(global-set-key "\M-t" 'indent-relative)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\e\e" 'eval-expression)

(unless (display-graphic-p)
  (global-set-key "\C-h" 'delete-backward-char))


;;; Random stuff I don't know what to do with
;(auto-insert-mode)
;(setq auto-insert-query nil)


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


;;;; use-package only from here on


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


;; displays ^L as a horizontal line (an alternate implementation is
;; page-break-lines)
(use-package form-feed)                 ;visual/buffer

;;; ido-mode (built-in) I seem to have commented this out, so I'm not
;;; going to do anything with it now

(setq-default ido-create-new-buffer 'always)
(ido-mode 1)
(ido-everywhere 1)

(use-package flx-ido
  :config
  (flx-ido-mode 1))

;;;
;;; YASnippet
;;;

(use-package yasnippet
  :config
  (yas-global-mode t))

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


;;; Pretty Pretty

;;;
;;; Font-lock stuff
;;;
(global-font-lock-mode)
(setq font-lock-maximum-decoration t)

;; provides toggle-highlight-tabs and toggle-highlight-trailing-whitespace 
(use-package highlight-chars)           ;visual/buffer

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; (add-to-list 'default-frame-alist
;;              ;; my old eyes need a bigger font when I'm at my desk on a big screen
;;              (if (and (= (display-pixel-width) 1920)
;;                       (= (display-pixel-height) 1200))
;;                  (cons 'font "Source Code Pro-18:weight=medium")
;;                  (cons 'font "Source Code Pro-14:weight=medium")))
;;

(setq default-frame-alist (append '((font . "Inconsolata-24:weight=medium")
                                    (width . 96)
                                    (height . 32))
                                  default-frame-alist))

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

(when (display-graphic-p)
    (server-start))

(setq gc-cons-threshold gc-cons-threshold--orig)
