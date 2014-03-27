(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("elpa" . "http://tromey.com/elpa/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")
						 ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")))

;; packages paths

(require 'package)

(defun package-update-load-path ()
  "Update the load path for newly installed packages"
  (interactive)
  (let ((package-dir (expand-file-name  package-user-dir)))
	(mapc (lambda (pkg)
			(let ((stem (symbol-name (car pkg)))
				  (mapc (lambda (num)
						  (if first
							  (setq first nil)
							(setq version (format "%s." version)))
						  (setq version (format "%s%s" version num)))
						(aref (cdr pkg) 0))
				  (setq path (format "%s/%s-%s" package-dir stem version))
				  (add-to-list 'load-path path)))
			package-alist))))

(add-to-list 'load-path "~/.emacs.d/")

;; after init

(add-hook 'after-init-hook #'(lambda () (load "~/.emacs.d/init-real.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
