;; lua mode

(req-package lua-mode
  :init (setq lua-indent-level 4))

;; flymake lua

(req-package flymake-lua
  :require (flymake lua-mode)
  :init (add-hook 'lua-mode-hook 'flymake-lua-load))

(provide 'init-lua)
