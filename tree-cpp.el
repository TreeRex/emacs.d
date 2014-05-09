(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(setq c-default-style "stroustrup")
(setq-default c-electric-flag t)

(defun my-c-mode-common-hook ()
  (setq tab-width 4
        indent-tabs-mode nil
        comment-column 45)
  (c-set-offset 'member-init-intro '++))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

