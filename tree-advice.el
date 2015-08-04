;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Advice to save me from myself

(require 'cl)

;; http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
           (flet ((process-list ())) ad-do-it))

(defadvice kill-region (around safe-kill-region)
  "If the size of the region is greater than 1K verify the kill."
  (let ((region-size (- (ad-get-arg 1) (ad-get-arg 0))))
    (when (or (< region-size 1024)
              (yes-or-no-p (format "Really delete %d characters? " region-size)))
        ad-do-it)))

(ad-activate 'kill-region)

