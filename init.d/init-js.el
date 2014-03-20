;; js2 mode

(req-package js2-mode
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(provide 'init-js)
