;;;; Settings for various text modes

(require 'speedbar)

;;;
;;; org-mode
;;;

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-clock-idle-time 10
      org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO" "ONGOING" "|" "DONE")
        (sequence "QUESTION" "|" "ANSWERED")
        (sequence "TASK" "|" "COMPLETE")))

(add-hook 'org-mode-hook (lambda ()
                           (visual-line-mode)
                           (speedbar-add-supported-extension ".org")))

;;;
;;; Markdown
;;;

(maybe-install-packages '(markdown-mode))

(speedbar-add-supported-extension ".md")

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
                                (visual-line-mode)))

;;;
;;; AsciiDoc
;;;

;;; I'm not fully sold on this yet, so for now I'm disabling

;; (maybe-install-packages '(adoc-mode))

(add-to-list 'auto-mode-alist '("\\.adoc$" . text-mode))


;; (speedbar-add-supported-extension ".adoc")
;; (add-hook 'adoc-mode-hook (lambda ()
;;                             (auto-fill-mode)
;;                             (flyspell-mode)))



;;;
;;; AucTeX
;;;

(maybe-install-packages '(auctex))

(setq TeX-auto-save t
      TeX-parse-self t)
