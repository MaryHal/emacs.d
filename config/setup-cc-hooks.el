(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun c-mode-common-custom ()
  (setq c-default-style "linux") ; linux-kernel-developers style indentation
  (setq c-basic-offset 4)        ; 4-space tab size

  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'access-label '0)
  (c-set-offset 'inline-open '0)

  (c-set-offset 'brace-list-open '0)
  )

(add-hook 'c-mode-common-hook 'c-mode-common-custom)

(provide 'setup-cc-hooks)

