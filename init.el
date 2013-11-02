;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Package Archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-list '(ace-jump-mode
                     ag
                     auctex
                     auto-complete
                     base16-theme
                     diminish
                     epl
                     evil-leader
                     evil
                     expand-region
                     haskell-mode
                     helm
                     flx
                     flx-ido
                     ido-ubiquitous
                     markdown-mode
                     melpa
                     multiple-cursors
                     pkg-info
                     dash
                     popup
                     popwin
                     s
                     smart-mode-line
                     smex
                     surround
                     undo-tree
                     workgroups2))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Custom Configuration
(setq custom-file "~/.emacs.d/custom.el")
(defun loadCustomFile()
  (unless (not (file-exists-p custom-file))
    (load custom-file)))
(add-hook 'after-init-hook 'loadCustomFile)

;; Personal Configuration
(setq config-file "~/.emacs.d/config.el")
(defun loadConfigFile()
  (load config-file))
(add-hook 'after-init-hook 'loadConfigFile)

;; Time Startup
(defun getUptime()
  (message "Time needed to load: %s seconds."
           (emacs-uptime "%s")))
(add-hook 'emacs-startup-hook 'getUptime 'append)
