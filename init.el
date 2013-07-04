;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Package Archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-list '(ac-math
                     ace-jump-mode
                     ag
                     auctex
                     auto-complete
                     auto-complete-clang
                     color-theme
                     diminish
                     evil
                     evil-leader
                     expand-region
                     haskell-mode
                     helm
                     lua-mode
                     ido-ubiquitous
                     markdown-mode
                     melpa
                     php-mode
                     popup
                     pretty-mode
                     smart-mode-line
                     smex
                     smooth-scrolling
                     surround
                     switch-window
                     undo-tree
                     web-mode
                     ))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/config.el")))

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))
          'append)
