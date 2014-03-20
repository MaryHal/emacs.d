;; single dired

(req-package dired-single
  :require
  (dired helm-swoop autorevert diff-hl)
  :init
  (progn (define-key dired-mode-map (kbd "f")
           'dired-single-buffer)

         (define-key dired-mode-map (kbd "<RET>")
           'dired-single-buffer)

         (define-key dired-mode-map (kbd "^")
           (function (lambda ()
                       (interactive)
                       (dired-single-buffer ".."))))

         (define-key dired-mode-map (kbd "M-i")
           'helm-swoop)

         (add-hook 'dired-mode-hook (lambda ()
                                      (auto-revert-mode 1)))

         (add-hook 'dired-mode-hook (lambda ()
                                      (diff-hl-dired-mode 1)))))

;; sunrise commander

(req-package sunrise-commander
  :require
  (dired dired-single))

;; dired rainbow

(req-package dired-rainbow
  :require
  dired)

;; dired open

(req-package dired-open
  :require
  dired)

;; lusty

(req-package lusty-explorer
  :init
  (global-set-key (kbd "C-x f") 'lusty-file-explorer))

(provide 'init-dired)
