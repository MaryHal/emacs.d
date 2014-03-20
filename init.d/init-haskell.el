;; haskell mode

(req-package haskell-mode
  :init (progn (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
               (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

;; flymake haskell

(req-package flymake-haskell-multi
  :require (flymake haskell-mode)
  :init (add-hook 'haskell-mode-hook 'flymake-haskell-multi-load))

(provide 'init-haskell)
