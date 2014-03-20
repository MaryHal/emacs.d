;; glsl

(req-package glsl-mode
  :init (progn (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
               (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
               (add-to-list 'auto-mode-alist '("\\.gs\\'" . glsl-mode))
               (setq glsl-other-file-alist '(("\\.fs$" (".vs"))
                                             ("\\.vs$" (".fs"))))))

(provide 'init-glsl)
