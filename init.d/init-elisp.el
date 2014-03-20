;; use emr

(req-package emr
  :init
  (progn (emr-initialize)
         (define-key emacs-lisp-mode-map
           (kbd "M-RET")
           'emr-show-refactor-menu)))

(provide 'init-elisp)
