(req-package ido
  :init
  (progn
    (ido-mode t)
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-max-prospects 10)

    (setq ido-save-directory-list-file (concat user-emacs-directory "cache/ido.last"))

    (defun my-ido-define-keys()
      (define-key ido-file-completion-map (kbd "C-u") 'ido-delete-backward-updir)
      (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
      (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

    (add-hook 'ido-setup-hook 'my-ido-define-keys)

    
    ;; Always rescan buffer for imenu
    (set-default 'imenu-auto-rescan t)

    (add-to-list 'ido-ignore-directories "target")
    (add-to-list 'ido-ignore-directories "node_modules")
    ))

(req-package ido-ubiquitous
  :require ido
  :init
  (progn
    (ido-ubiquitous-mode t)

    ;; Fix ido-ubiquitous for newer packages
    (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
      `(eval-after-load ,package
         '(defadvice ,cmd (around ido-ubiquitous-new activate)
            (let ((ido-ubiquitous-enable-compatibility nil))
              ad-do-it))))

    ;;(ido-ubiquitous-use-new-completing-read webjump 'webjump)
    ;;(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
    ;;(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)
    ))

(req-package flx-ido
  :require (flx ido)
  :init
  (progn
    (flx-ido-mode t)

    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)

    ;; Disable flx highlights
    ;; (setq flx-ido-use-faces nil)
    ))

(req-package smex
  :require ido
  :bind
  (("M-x" . smex)
   )
  :init
  (progn
    (smex-initialize)

    (setq smex-key-advice-ignore-menu-bar t)
    (setq smex-save-file (concat user-emacs-directory "cache/smex-items"))
    ))

(provide 'init-ido)
