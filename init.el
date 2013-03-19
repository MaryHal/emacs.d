;; Set-up org babel
(setq org-babel-load-languages '((emacs-lisp . t)))
(setq org-confirm-babel-evaluate nil)
(require 'org-install)
(require 'org)

;; Load neatly organized org file!
(org-babel-load-file "~/.emacs.d/config.org")
