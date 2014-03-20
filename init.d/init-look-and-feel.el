;; pretty lambda

(req-package pretty-lambdada
  :init
  (pretty-lambda-for-modes))

;; customizations

(req-package menu-bar
  :init
  (menu-bar-mode -1))

;; main line

(req-package smart-mode-line
  :require
  remember-theme
  :init
  (progn (setq sml/theme 'respectfull)
         (setq sml/shorten-modes t)
         (setq sml/shorten-directory t)
         (setq sml/name-width 20)
         (setq sml/mode-width 'full)
         (add-to-list 'sml/hidden-modes " Anzu")
         (add-to-list 'sml/hidden-modes " AC")
         (add-to-list 'sml/hidden-modes " yas")
         (add-to-list 'sml/hidden-modes " pair")
         (add-to-list 'sml/hidden-modes " 80col")
         (add-to-list 'sml/hidden-modes " FIC")
         (add-to-list 'sml/hidden-modes " Abbrev")
         (add-to-list 'sml/hidden-modes " ARev")
         (sml/setup)))

;; anzu

(req-package anzu
  :require
  smart-mode-line
  :init
  (global-anzu-mode 1))

;; mode line tweaks

(req-package simple
  :init
  (column-number-mode 1))

;; toolbar

(req-package tool-bar
  :init
  (tool-bar-mode -1))

;; scroll bar

(req-package scroll-bar
  :init
  (scroll-bar-mode -1))

;; expand region

(req-package expand-region
  :require
  sml-mode
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

;; turn off sctartup screen

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; show function name

(add-hook 'prog-mode-hook (lambda () (which-function-mode 1)))

;; miscaleous tweeks

(setq make-pointer-invisible nil)

;; smooth mouse scroll

(req-package mwheel
  :init
  (progn (setq mouse-wheel-scroll-amount '(2 ((shift) . 2)))
         (setq mouse-wheel-progressive-speed nil)
         (setq mouse-wheel-follow-mouse t)
         (setq scroll-step 1)
         (setq auto-window-vscroll nil)
         (setq scroll-preserve-screen-position t)
         (setq isearch-allow-scroll t)))

;; todo, fixme highlighting

(req-package fic-mode
  :init
  (add-hook 'prog-mode-hook (lambda () (fic-mode 1))))

;; highlight parenthesis

(req-package highlight-parentheses
  :init
  (add-hook 'prog-mode-hook (lambda () (highlight-parentheses-mode 1))))

;; hl sexps

(req-package hl-sexp
  :require
  hl-line
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (hl-sexp-mode 1))))

;; diff highlight

(req-package diff-hl
  :require
  (smartrep fringe)
  :init
  (global-diff-hl-mode 1))

;; highlight defined symbols

(req-package hl-defined
  :init
  (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode))

;; desc unbound keys

(req-package unbound)

;; helm themes

(req-package helm-themes
  :require
  helm)

;; xt mosue

(req-package xt-mouse
  :init (xterm-mouse-mode 1))

    ;; Theme

(req-package base16-theme
  :init
  (progn
    ;; Transparency after load theme...
    ;; On Linux, if in terminal, clear the background. If GUI, set background to black and set
    ;; frame transparency.
    (defadvice load-theme (after load-theme activate compile)
      (if (string= system-type "gnu/linux")
          (if (string= window-system "x")
              (progn (set-frame-parameter (selected-frame) 'alpha '(90 90))
                     (add-to-list 'default-frame-alist '(alpha 90 90))
                     (set-face-attribute 'default nil :background "black"))
            (progn (when (getenv "DISPLAY")
                     (set-face-attribute 'default nil :background "unspecified-bg")
                     ))
            )))

    (load-theme 'base16-default t)
    ))

;; Smooth Scrolling

(setq scroll-margin 8
      scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; Default window metrics
(setq default-frame-alist
      '((top   . 10) (left   . 2)
        (width . 80) (height . 30)
        (mouse-color  . "#CCCCCC")
        (cursor-color . "#CCCCCC")
        ))

;; Set font
(if (string= system-type "windows-nt")
    ;; If Windows
    (progn (setq myFrameFont "Consolas 10")
           (add-to-list 'default-frame-alist '(font . "Consolas 10")))
  ;; If not Windows
  (progn (setq myFrameFont "Inconsolata 10")
         (add-to-list 'default-frame-alist '(font . "Inconsolata 10")))
  )


(provide 'init-look-and-feel)
