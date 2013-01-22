;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Dired Configurations

;; Use the GNU Coreutils version of ls if available
(when (and (eq system-type 'darwin)
           (file-executable-p "/usr/local/bin/gls"))
  (setq insert-directory-program "/usr/local/bin/gls"))

(setq dired-listing-switches "-Alh")
