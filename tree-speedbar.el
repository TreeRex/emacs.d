;;;; -*- mode:Emacs-Lisp -*-

(maybe-install-packages '(sr-speedbar))

(eval-after-load 'sr-speedbar
    '(setq speedbar-hide-button-brackets-flag t
           speedbar-show-unknown-files t
           speedbar-smart-directory-expand-flag t
           speedbar-directory-button-trim-method 'trim
           speedbar-use-images nil
           speedbar-indentation-width 2
           speedbar-use-imenu-flag t
           speedbar-file-unshown-regexp "flycheck-.*"
           sr-speedbar-width 40
           sr-speedbar-width-x 40
           sr-speedbar-auto-refresh nil
           sr-speedbar-skip-other-window-p t
           sr-speedbar-right-side nil))
