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
