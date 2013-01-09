;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Configuration for Folding Mode

;; folding-mode isn't handled by any package managers, so you need to put it
;; into ~/emacs:
;;
;; http://www.emacswiki.org/emacs/FoldingMode

(if (load "folding" 'nomessage 'noerror)
    (folding-mode-add-find-file-hook))

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

