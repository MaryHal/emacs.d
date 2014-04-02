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
 '(custom-safe-themes (quote ("7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:inherit mode-line :inverse-video nil))))
 '(helm-ff-directory ((t (:foreground "red"))))
 '(helm-header ((t (:background "grey10" :weight bold))))
 '(helm-selection ((t (:underline t))))
 '(helm-source-header ((t (:background "grey30" :foreground "white" :weight bold))))
 '(helm-visible-mark ((t (:foreground "grey40"))))
 '(isearch-fail ((t (:background "red" :foreground "brightwhite"))))
 '(lazy-highlight ((t (:background "RoyalBlue" :foreground "gray6"))))
 '(mode-line ((t (:background "gray6" :foreground "gray60"))))
 '(mode-line-inactive ((t (:background "gray10" :foreground "gray60"))))
 '(region ((t (:background "RoyalBlue" :foreground "gray6"))))
 '(trailing-whitespace ((t (:background "goldenrod"))))
 '(vertical-border ((t (:background "gray10" :foreground "gray10")))))
