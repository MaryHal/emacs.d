;; xml

(req-package auto-complete-nxml
  :require auto-complete
  :init (progn (setq-default nxml-child-indent 4)
               (setq nxml-child-indent 4)))

(provide 'init-xml)
