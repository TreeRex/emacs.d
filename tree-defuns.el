;;;; -*- mode:emacs-lisp; lexical-binding:t; coding:utf-8 -*-
;;;;
;;;; Utility functions

(defun tree/add-hooks (modes func)
  "Add FUNC as a hook for each mode specified in MODES"
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) func)))

(defun tree/unfill-and-copy-region (beg end)
  "Unfills the selected region and copies to the Window system's clipboard."
  (interactive "r")
  (save-excursion
    (kill-ring-save beg end)
    (with-temp-buffer
      (yank)
      (let ((fill-column (point-max)))
        (fill-region (point-min) (point-max))
        (clipboard-kill-ring-save (point-min) (point-max))))))

(defun tree/maybe-install-packages (packages)
  "Install the packages in PACKAGES if they aren't installed already. Useful
when creating a new emacs installation."
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; http://stackoverflow.com/a/6541072/431344

(defun tree/func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun tree/hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (tree/func-region start end #'url-hexify-string))

(defun tree/unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (tree/func-region start end #'url-unhex-string))

;;;
