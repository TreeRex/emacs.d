;;;; -*- Mode: Emacs-Lisp

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

(add-hook 'org-mode-hook (lambda () (visual-line-mode)))
