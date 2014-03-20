;; batch mode

(req-package batch-mode
  :init (add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode)))

(provide 'init-batch)
