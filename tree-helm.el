;;; tree-helm.el --- Configuration for Helm. -*- lexical-binding: t -*-

(maybe-install-packages '(helm))

(require 'helm-config)

(helm-mode 1)

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

(global-set-key (kbd "M-x") 'helm-M-x)


;;; tree-helm.el ends here
