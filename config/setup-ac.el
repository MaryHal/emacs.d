(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-clang)
;(require 'auto-complete-yasnippet)

;(require 'auto-complete-emacs-lisp)
;(require 'auto-complete-latex)
;(require 'ac-math)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")

;(ac-config-default)
(defcustom mycustom-system-include-paths
  '(
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/../../../../include/c++/4.7.1"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/../../../../include/c++/4.7.1/x86_64-unknown-linux-gnu"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/../../../../include/c++/4.7.1/backward"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/include"
    "/usr/local/include"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/include-fixed"
    "/usr/include"
    )
  "This is a list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
  )

(setq clang-completion-suppress-error 't)
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (append
               mycustom-system-include-paths
               )
              )
      )

;; C-common mode setup
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-dictionary
                             ac-source-filename
                             ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;;(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

(my-ac-config)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(ac-set-trigger-key "TAB")
;;(define-key ac-mode-map  [(control tab)] 'auto-complete)

;; Key mappings
(setq ac-use-menu-map t)

;(define-key ac-menu-map "\C-n" 'ac-next)
;(define-key ac-menu-map "\C-p" 'ac-previous)
;(define-key ac-menu-map (kbd "TAB") 'ac-next)
;(define-key ac-menu-map (kbd "M-TAB") 'ac-previous)
;(define-key ac-menu-map (kbd "<tab>") 'ac-next)
;(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)

;; Stuff to help in terminal emacs
(define-key ac-menu-map (kbd "ESC") 'ac-stop)
(define-key ac-menu-map (kbd "C-j") 'ac-next)
(define-key ac-menu-map (kbd "C-k") 'ac-previous)

(define-key ac-menu-map (kbd "TAB") nil)
;; (define-key ac-menu-map (kbd "RET") 'ac-complete)

;; Colors
;(set-face-background 'ac-candidate-face "lightgray")
;(set-face-underline 'ac-candidate-face "darkgray")
;(set-face-background 'ac-selection-face "steelblue")
(set-face-foreground 'ac-selection-face "black")

(provide 'setup-ac)
