;; Make the bars pretty
(require 'powerline)

;; Diminish modeline clutter
(require 'diminish)
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "El")))

;; Fonts + theme
;(require 'color-theme)
(color-theme-tomorrow-night)
(set-frame-font "Inconsolata 10")

(unless window-system
  (when (getenv "DISPLAY")
    (set-face-attribute 'default nil :background "unspecified-bg")

    (color-theme-tomorrow-night-bright)
    ;; Powerline settings
    (custom-set-faces
     '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil)))))
    ; '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
    ))

;; Thematic configuration
;(add-hook 'before-make-frame-hook 'turn-off-tool-bar)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

;; Boring startup screens
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(line-number-mode 1)   ; have line numbers and
(column-number-mode 1) ; column numbers in the mode line

(setq-default indent-tabs-mode nil) ; No tabs

; Don't add newlines when cursor goes past end of file
(setq next-line-add-newlines nil)

;; Don't Blink Cursor
(blink-cursor-mode -1)

;; Fringe
(set-fringe-mode 0)

(setq visible-bell nil
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Parenthesis matching
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#def")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

(provide 'appearance)

