(maybe-install-packages '(projectile))

;; this should be marked autoload in projectile.el but isn't
(autoload 'projectile-on "projectile" nil t)

(eval-after-load "projectile"
  '(add-to-list 'projectile-globally-ignored-directories ".svn"))

(add-hook 'clojure-mode-hook #'projectile-on)
