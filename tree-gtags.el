;;; -*- Mode: Emacs-Lisp -*-

;;; gtags.el is part of the GNU GLOBAL package, so you need to install
;;; it manually.

(setq gtags-ignore-case t
      gtags-suggested-key-mapping t
      gtags-auto-update nil)
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))
