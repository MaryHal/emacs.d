;; bash complete

(req-package bash-completion
  :require auto-complete
  :init (bash-completion-setup))

;; flymake shell

(req-package flymake-shell
  :require (flymake shell)
  :init (add-hook 'sh-set-shell-hook 'flymake-shell-load))

(provide 'init-bash)
