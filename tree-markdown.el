;;;; -*- mode: EmacsLisp -*-

(maybe-install-packages '(markdown-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
                                (setq fill-column 78)
                                (auto-fill-mode)))



