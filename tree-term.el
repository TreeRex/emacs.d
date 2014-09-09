;;; If '4m' shows up before each prompt, you need to install the eterm-color terminfo:
;;;
;;; tic -o ~/.terminfo /usr/local/Cellar/emacs/24.3/share/emacs/24.3/etc/e/eterm-color.ti

(maybe-install-packages '(multi-term))

(require 'multi-term)
(setq multi-term-program "/bin/zsh")
