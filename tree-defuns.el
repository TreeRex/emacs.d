;;;; -*- Mode: Emacs-Lisp -*-
;;;;
;;;; Miscellaneous functions used by other parts of the initialization

(defun add-hooks (modes func)
  "Add FUNC as a hook for each mode specified in MODES"
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) func)))

(defun unfill-and-copy-region (beg end)
  "Unfills the selected region and copies to the Window system's clipboard."
  (interactive "r")
  (save-excursion
    (kill-ring-save beg end)
    (with-temp-buffer
      (yank)
      (let ((fill-column (point-max)))
        (fill-region (point-min) (point-max))
        (clipboard-kill-ring-save (point-min) (point-max))))))

(defun maybe-install-packages (packages)
  "Install the packages in PACKAGES if they aren't installed already. Useful
when creating a new emacs installation."
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

