(setq evil-want-C-u-scroll t)
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil)
(setq evil-cross-lines t)
(setq evil-intercept-esc 'always)

(setq evil-auto-indent t)

(req-package undo-tree
  :diminish undo-tree-mode)

(req-package evil
  :require undo-tree
  :init
  (progn (evil-mode t)
         (evil-set-toggle-key "C-\\")

         ;; Stop evil from overwriting cursor color
         (setq evil-default-cursor t)
         ;; (setq evil-insert-state-cursor '("#aa0000" hbar))
         ))


(provide 'evil-config)
