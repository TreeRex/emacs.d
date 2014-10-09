(maybe-install-packages '(projectile))

(eval-after-load "projectile"
  '(add-to-list 'projectile-globally-ignored-directories ".svn"))

(projectile-global-mode)
