
;; Preload Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things that should be set early just in case something bad happens.

;; Turn off backup files
(setq make-backup-files nil)

;; ad-handle-definition warnings are generated when functions are redefined with
;; defadvice. Let's disable these warnings for now.
(setq ad-redefinition-action 'accept)

;; Emacs will run garbage collection after `gc-cons-threshold' bytes of
;; consing. The default value is 800,000 bytes, or ~ 0.7 MiB. By increasing to
;; 40 MiB we reduce the number of pauses due to garbage collection.
(setq gc-cons-threshold (* 4 10 1024 1024))

;; Pre-init Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

(defconst user-custom-file (concat user-emacs-directory "custom.el"))
(defconst user-cache-directory (concat user-emacs-directory "cache/"))

(defconst user-shell "zsh")

;; Bootstrap use-package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file uses [[https://github.com/jwiegley/use-package][use-package]] to
;; install and configure third-party packages. Since use-package itself is a
;; third-party package, we need to handle the case for when we have a fresh
;; install.

(eval-when-compile (package-initialize))

;; ;; Bootstrap Function!
;; (defun require-package (package)
;;   "refresh package archives, check package presence and install
;; if it's not installed"
;;   (if (null (require package nil t))
;;       (progn (package-initialize)
;;              (let* ((ARCHIVES (if (null package-archive-contents)
;;                                   (progn (package-refresh-contents)
;;                                          package-archive-contents)
;;                                 package-archive-contents))
;;                     (AVAIL (assoc package ARCHIVES)))
;;                (if AVAIL
;;                    (package-install package)))
;;              (require package))))

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)
    (package-initialize)))

(require 'use-package)

;; Aquire use-package, the crux of our config file.
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0)

;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-loadpath (&rest dirs)
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my-window-killer ()
  "closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))

;; By default M-w is =kill-ring-save= -- this function simply copies a region.
;; However, if no region is selected, it doesn't really do anything.
;; =save-region-or-current-line= is =kill-ring-save-dwim=. If no region is
;; selected, copy to the end of the current line.

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

(defun comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above. If region is
active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    ;; (forward-line 1)
    (back-to-indentation)))

;; Very simple. Just open a terminal in the cwd using the $TERMINAL environment variable.
(defun open-terminal ()
  (interactive)
  (call-process-shell-command (concat "eval $TERMINAL -e " user-shell) nil 0))


;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When popping the mark, continue popping until the cursor actually moves

(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Balance windows after splitting.

;; ;; Rebalance windows after splitting right
;; (defadvice split-window-right
;;     (after rebalance-windows activate)
;;   (balance-windows))
;; (ad-activate 'split-window-right)

;; ;; Rebalance windows after splitting horizontally
;; (defadvice split-window-horizontally
;;     (after rebalance-windows activate)
;;   (balance-windows))
;; (ad-activate 'split-window-horizontally)

;; ;; Balance windows after window close
;; (defadvice delete-window
;;     (after rebalance-windows activate)
;;   (balance-windows))
;; (ad-activate 'delete-window)

;; Sane Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq epa-file-select-keys nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
;; (setq delete-by-moving-to-trash t)

(setq-default fill-column 80)

;; Easily navigate sillycased words
(global-subword-mode t)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Useful frame title, that show either a file or a buffer name (if the buffer
;; isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " : " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; Backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8))

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Window Rebalancing
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(use-package autorevert
  :commands (auto-revert-mode)
  :init (add-hook 'find-file-hook #'(lambda () (auto-revert-mode t))))

(use-package simple
  :config (progn (setq shift-select-mode nil)

                 ;; ;; Show active region
                 ;; (transient-mark-mode t)
                 ;; (make-variable-buffer-local 'transient-mark-mode)
                 ;; (put 'transient-mark-mode 'permanent-local t)
                 ;; (setq-default transient-mark-mode t)

                 ;; eval-expression-print-level needs to be set to 0 (turned
                 ;; off) so that you can always see what's happening.
                 (setq eval-expression-print-level nil)
                 ))

(use-package jka-cmpr-hook
  :config (auto-compression-mode t))

(use-package delsel
  :config (delete-selection-mode t))

(use-package tramp
  :defer t
  :config (setq tramp-default-method "ssh"))

(use-package tls
  :config (progn (setq tls-checktrust t)))

(use-package recentf
  :defer 10
  :commands (recentf-mode)
  :config (progn (setq recentf-save-file (concat user-cache-directory "recentf"))
                 (setq recentf-max-saved-items 100)
                 (setq recentf-max-menu-items 15)
                 ))

(use-package uniquify
  :defer t
  :disabled t
  :config (progn (setq uniquify-buffer-name-style 'forward
                       uniquify-separator "/"
                       uniquify-ignore-buffers-re "^\\*" ;; leave special buffers alone
                       uniquify-after-kill-buffer-p t)
                 ))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config (winner-mode t))

(use-package ediff
  :defer t
  :config (progn (setq ediff-diff-options "-w")
                 (setq ediff-split-window-function 'split-window-horizontally)
                 (setq ediff-window-setup-function 'ediff-setup-windows-plain)
                 ))

(use-package mouse
  :disabled t
  :config (progn (xterm-mouse-mode t)
                 (defun track-mouse (e))
                 (setq mouse-sel-mode t)
                 ))

;; Seed the random number generator
(random t)

;; ;; A lesser known fact is that sending the USR2 signal to an Emacs process makes it
;; ;; proceed as soon as possible to a debug window. USR1 is ignored however, so let’s
;; ;; bind it to an alternative desirable function that can be used on an Emacs
;; ;; instance that has locked up.
;; (defun my-quit-emacs-unconditionally ()
;;   (interactive)
;;   (my-quit-emacs '(4)))

;; (define-key special-event-map (kbd "<sigusr1>") 'my-quit-emacs-unconditionally)

;; Backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable backup
(setq backup-inhibited t)

;; Disable auto save
(auto-save-mode nil)
(setq auto-save-default nil)
(with-current-buffer (get-buffer "*scratch*")
  (auto-save-mode -1))

;; If `auto-save-list-file-prefix' is set to `nil', sessions are not recorded
;; for recovery.
;; (setq auto-save-list-file-prefix nil)
(setq auto-save-list-file-prefix (concat user-cache-directory "auto-save-list"))

;; Place Backup Files in a Specific Directory
(setq make-backup-files nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `((".*" . ,(expand-file-name
                  (concat user-cache-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

;; Helper Libraries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; String manipulation library
(use-package s
  :defer t
  :ensure t)

;; Modern list library
(use-package dash
  :defer t
  :ensure t)

;; Homeless Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybindings for functions that are not closely associated with a package
;; (like the built-in C functions) are located here.

;; ;; Poor-man's leader?
;; (defvar my-leader-key "M-SPC")
;; (global-unset-key (kbd "M-SPC"))

;; (defun leader-kbd (&rest keys)
;;   (kbd (mapconcat 'identity (cons my-leader-key keys) " ")))

;; ;; ;; Example Usage:
;; ;; (global-set-key (leader-kbd "m") 'magit-status)

;; Remove suspend-frame. Three times.
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
(put 'suspend-frame 'disabled t)

;; Unset some keys I never use
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x f"))

;; replace with [r]eally [q]uit
(bind-key "C-x r q" #'save-buffers-kill-terminal)
(bind-key "C-x C-c" (lambda ()
                      (interactive)
                      (message "Thou shall not quit!")))

;; Alter M-w so if there's no region, just grab 'till the end of the line.
(bind-key "M-w" #'save-region-or-current-line)

;; Join below
(bind-key "C-j" (lambda ()
                  (interactive)
                  (join-line -1)))

;; Join above
(bind-key "M-j" #'join-line)

;; Move windows
(windmove-default-keybindings 'meta)

;; Easier version of "C-x k" to kill buffer
(bind-key "C-x C-b"  #'buffer-menu)
(bind-key "C-x C-k"  #'kill-buffer)

;; Eval
(bind-key "C-c v"    #'eval-buffer)
(bind-key "C-c r"    #'eval-region)

(bind-key "C-c k"    #'open-terminal)

(bind-key "C-;"      #'comment-line-or-region)
(bind-key "M-i"      #'back-to-indentation)

;; (bind-key "C-."      #'hippie-expand)
(bind-key "C-."      #'dabbrev-expand)

;; Character-targeted movements
(use-package misc
  :bind (("M-z" . zap-up-to-char)))

(use-package jump-char
  :ensure t
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

(dotimes (n 10)
  (global-unset-key (kbd (format "M-%d" n)))
  (global-unset-key (kbd (format "C-%d" n))))

(bind-key "M-9"      #'backward-sexp)
(bind-key "M-0"      #'forward-sexp)

;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Frame Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-frame-alist
      '((top   . 10) (left   . 2)
        (width . 80) (height . 30)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (left-fringe . 0) (right-fringe . 0)
        ))

(use-package menu-bar
  :config (menu-bar-mode -1))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package tooltip
  :config (tooltip-mode -1))

(use-package scroll-bar
  :disabled t
  :config (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq visible-bell nil
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

;; Modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-mode-line
  :ensure t
  :config (progn (setq-default sml/line-number-format " %3l")
                 (setq-default sml/col-number-format  "%2c")

                 (line-number-mode t)   ;; have line numbers and
                 (column-number-mode t) ;; column numbers in the mode line

                 (setq sml/theme nil)
                 (sml/setup)
                 ))

;; I prefer hiding all minor modes by default.
(use-package rich-minority
  :ensure t
  :config (progn (setq rm-blacklist nil)
                 (setq rm-whitelist " Wrap")
                 ;; (rich-minority-mode t)
                 ))

;; Fringe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fringe
  :if (window-system)
  :config (progn
            ;; Disable margins
            (setq-default left-margin-width 0
                          right-margin-width 0)
            (set-window-buffer nil (current-buffer))

            ;; Don't show empty line markers in the fringe past the end of the document
            (setq-default indicate-empty-lines nil)

            ;; (define-fringe-bitmap 'empty-line
            ;;   [#b0010000
            ;;    #b0000000
            ;;    #b0010000
            ;;    #b0000000
            ;;    #b0010000
            ;;    #b0000000
            ;;    #b0010000
            ;;    #b0000000
            ;;    #b0010000])

            ;; (setq-default indicate-buffer-boundaries '((top . left)
            ;;                                            (bottom . left)))
            ;; (setq-default indicate-buffer-boundaries 'left)
            (setq-default indicate-buffer-boundaries 'nil)

            (define-fringe-bitmap 'right-arrow
              [#b0000000
               #b0000000
               #b0010000
               #b0011000
               #b0011100
               #b0011000
               #b0010000
               #b0000000
               #b0000000])
            (define-fringe-bitmap 'left-arrow
              [#b0000000
               #b0000000
               #b0001000
               #b0011000
               #b0111000
               #b0011000
               #b0001000
               #b0000000
               #b0000000])
            (define-fringe-bitmap 'exclamation-mark
              [#b0010000
               #b0111000
               #b0111000
               #b0010000
               #b0010000
               #b0010000
               #b0000000
               #b0010000
               #b0010000])
            (define-fringe-bitmap 'question-mark
              [#b0011000
               #b0100100
               #b0100100
               #b0001000
               #b0010000
               #b0010000
               #b0000000
               #b0010000
               #b0010000])

            (set-fringe-mode (cons 8 8))
            ))

;; Theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; Set transparency of emacs
(defun set-frame-alpha (arg &optional active)
  (interactive "nEnter alpha value (1-100): \np")
  (let* ((elt (assoc 'alpha default-frame-alist))
         (old (frame-parameter nil 'alpha))
         (new (cond ((atom old)     `(,arg ,arg))
                    ((eql 1 active) `(,arg ,(cadr old)))
                    (t              `(,(car old) ,arg)))))
    (if elt (setcdr elt new) (push `(alpha ,@new) default-frame-alist))
    (set-frame-parameter nil 'alpha new)))

(defun mhl/load-light-theme (theme)
  (load-theme theme t)
  (set-frame-alpha 90))

(defun mhl/load-dark-theme (theme)
  (load-theme theme t)

  ;; Set transparent background.
  (if (string= system-type "gnu/linux")
      (if (string= window-system "x")
          (progn
            (set-face-attribute 'default nil :background "black")
            (set-face-attribute 'fringe nil :background "black")
            (set-frame-alpha 90)
            )
        (progn (when (getenv "DISPLAY")
                 (set-face-attribute 'default nil :background "unspecified-bg")
                 ))
        )))

(use-package tao-theme
  :ensure t
  :disabled t
  :init (progn
          (defun tao-palette () (tao-theme-golden-grayscale-yang-palette))
          (mhl/load-dark-theme 'tao-yin)
          ;; (mhl/load-light-theme 'tao-yang)
          ))

(use-package base16-mod-theme
  :load-path "theme/base16-mod"
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/base16-mod/"))
          (mhl/load-dark-theme 'base16-mod-dark)
          ;; (mhl/load-dark-theme 'base16-ashes-dark)
          ))

(use-package leuven-mod
  :load-path "theme/leuven-mod"
  :disabled t
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/leuven-mod/"))
          (mhl/load-light-theme 'leuven-mod)
          ))

;; ;; We have some custom themes packaged with this config, so make sure =load-theme= can find 'em.
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/ashes/"))
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/minimal/"))
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/smyx/"))
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/ample-theme/"))

;; (mhl/load-light-theme 'base16-ashes-light)
;; (mhl/load-light-theme 'flatui)
;; (mhl/load-light-theme 'ample-light)

;; (mhl/load-dark-theme 'noctilux)
;; (mhl/load-dark-theme 'minimal)
;; (mhl/load-dark-theme 'ample-flat)

;; Disable the nagging when loading custom themes.
(setq custom-safe-themes t)

;; Tooltips can be themed as well.
(setq x-gtk-use-system-tooltips nil)

;; Font ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun font-candidate (&rest fonts)
  "Return the first existing font in FONTS."
  (find-if (lambda (f) (find-font (if (fontp f) f (font-spec :name f)))) fonts))

;; Only set font if outside terminal
(when (display-graphic-p)
  (set-frame-font (font-candidate (font-spec :family "PragmataPro"
                                             :size 12)
                                  (font-spec :family "Inconsolatazi4"
                                             :size 12)
                                  "Monospace 8"
                                  (font-spec :family "Consolas"
                                             :size 12)
                                  ) t t))

;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I prefer spaces over tabs. Note how this is not a mode, but a buffer-local
;; variable.
(setq-default indent-tabs-mode nil)

;; Don't add newlines when cursor goes past end of file
(setq next-line-add-newlines nil)
(setq require-final-newline nil)

;; Don't Blink Cursor
(blink-cursor-mode -1)
(setq visible-cursor nil)

;; Smoother Scrolling
(setq-default scroll-margin 10
              scroll-conservatively 101
              scroll-preserve-screen-position t
              auto-window-vscroll nil)

(use-package paren
  :config (progn (show-paren-mode t)
                 (setq show-paren-delay 0)
                 ))

(use-package highlight-parentheses
  :ensure t
  :config (progn
            (defun hl-parens-hook()
              (highlight-parentheses-mode t))
            (add-hook 'prog-mode-hook #'hl-parens-hook)
            ))

(use-package highlight-indent-guides
  :ensure t
  :disabled t
  :config (progn
            (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
            (set-face-background 'highlight-indent-guides-even-face "#101010")
            (set-face-background 'highlight-indent-guides-odd-face  "#0B0B0B")
            ))

;; (use-package elec-pair
;;   :config (electric-pair-mode t))

(use-package electric
  :config (electric-indent-mode t))

;; Trailing whitespace
(defun disable-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(defun disable-scroll-margin ()
  (setq-local scroll-margin 0))

(add-hook 'term-mode-hook #'disable-show-trailing-whitespace)
(add-hook 'term-mode-hook #'disable-scroll-margin)
(setq-default show-trailing-whitespace t)

(use-package imenu
  :config (progn
            ;; Add use-package / init file blocks to imenu
            (defun imenu-init-file-additions()
              (add-to-list 'imenu-generic-expression
                           '("Package" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
              (add-to-list 'imenu-generic-expression
                           '("Section" "^;; \\(.+\\) ;+$" 1))
              )
            (add-hook 'emacs-lisp-mode-hook #'imenu-init-file-additions)
            ))

(use-package avy
  :ensure t
  :commands (avy-goto-word-or-subword-1)
  :init (progn (setq avy-keys
                     '(?c ?a ?s ?d ?e ?f ?h ?w ?y ?j ?k ?l ?n ?m ?v ?r ?u ?p))
               ))

(use-package swiper
  :ensure t
  :defer t)

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :init (global-anzu-mode t))

(use-package aggressive-indent
  :ensure t
  :disabled t
  :config (global-aggressive-indent-mode t))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package key-chord
  :ensure t
  :disabled t
  :commands (key-chord-mode)
  :init (progn (key-chord-mode t))
  :config (progn (key-chord-define-global "VV" #'other-window)
                 (key-chord-define-global "qf" #'helm-find-files)
            ))

(use-package guide-key
  :ensure t
  :disabled t
  :defer 10
  :commands (guide-key-mode)
  :config (progn (setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "M-SPC"))
                 (setq guide-key/recursive-key-sequence-flag t)

                 ;; Alignment and extra spacing
                 (setq guide-key/align-command-by-space-flag t)

                 (guide-key-mode t)
                 ))

(use-package multiple-cursors
  :ensure t
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :init (progn (setq mc/list-file (concat user-cache-directory "mc-lists.el"))

               (setq mc/unsupported-minor-modes '(company-mode
                                                  auto-complete-mode
                                                  flyspell-mode
                                                  jedi-mode))

               (global-unset-key (kbd "M-<down-mouse-1>"))
               (bind-key "M-<mouse-1>" #'mc/add-cursor-on-click)
               ))

(use-package ag
  :ensure t
  :commands (ag ag-regexp)
  :init (progn
          (use-package helm-ag
            :ensure t
            :commands (helm-ag))
          ))

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

(use-package beacon
  :if (window-system)
  :ensure t
  :config (progn
            (add-to-list 'beacon-dont-blink-commands 'fzf)
            (add-to-list 'beacon-dont-blink-commands 'fzf-directory)

            (setq beacon-blink-when-window-scrolls t)
            (setq beacon-blink-when-window-changes t)
            ;; (setq beacon-blink-when-point-moves 2)

            (setq beacon-color 0.3)

            (beacon-mode t)))

;; Version Control ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine))

(use-package git-gutter
  :if (not window-system)
  :ensure t
  :defer t
  :init (progn (global-git-gutter-mode t))
  :config (progn
            (setq-default left-margin-width 1
                          right-margin-width 1)
            (set-window-buffer nil (current-buffer))

            (setq git-gutter:modified-sign "*")
            (setq git-gutter:added-sign "+")
            (setq git-gutter:deleted-sign "-")

            (set-face-foreground 'git-gutter:modified "purple")
            (set-face-foreground 'git-gutter:added    "green")
            (set-face-foreground 'git-gutter:deleted  "red")
            ))

(use-package git-gutter-fringe
  :if (window-system)
  :ensure t
  :init (progn (global-git-gutter-mode t))
  :config (progn
            (define-fringe-bitmap 'git-gutter-fr:added
              [#b0000000
               #b0010000
               #b0010000
               #b1111100
               #b0010000
               #b0010000
               #b0000000
               #b0000000])
            (define-fringe-bitmap 'git-gutter-fr:deleted
              [#b0000000
               #b0000000
               #b0000000
               #b1111100
               #b0000000
               #b0000000
               #b0000000
               #b0000000])
            (define-fringe-bitmap 'git-gutter-fr:modified
              [#b0000000
               #b0010000
               #b0111000
               #b1111100
               #b0111000
               #b0010000
               #b0000000
               #b0000000])))

;; Clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF7_STRING COMPOUND_TEXT TEXT STRING))

(defun xsel-cut-function (text &optional push)
  ;; Insert text to temp-buffer, and "send" content to xsel stdin
  (with-temp-buffer
    (insert text)
    ;; I prefer using the "clipboard" selection (the one the typically is used
    ;; by c-c/c-v) before the primary selection (that uses
    ;; mouse-select/middle-button-click)
    (call-process-region (point-min) (point-max)
                         "xsel"
                         nil 0
                         nil "--clipboard" "--input")))
;; Callback for when user pastes
(defun xsel-paste-function()
  ;; Find out what is current selection by xsel. If it is different from the
  ;; top of the kill-ring (car kill-ring), then return it. Else, nil is
  ;; returned, so whatever is in the top of the kill-ring will be used.
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))

;; If emacs is run in a terminal, the default clipboard functions have no
;; effect. Instead, we'll make use of xsel. See
;; [[http://www.vergenet.net/~conrad/software/xsel/][this]] -- "a command-line
;; program for getting and setting the contents of the X selection"
(when (not (display-graphic-p))
  ;; Callback for when user cuts
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function #'xsel-cut-function)
  (setq interprogram-paste-function #'xsel-paste-function)
  ;; Idea from http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
  )

;; Hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :init (progn
          (bind-key "<f1>" (defhydra hydra-help (:color blue)
                             "Help"
                             ("a" helm-apropos "Apropos")
                             ("c" describe-char "Describe Char")
                             ("F" find-function "Find Function")
                             ("f" describe-function "Describe Function")
                             ("k" describe-key "Describe Key")
                             ("K" find-function-on-key "Find Key")
                             ("m" describe-mode "Describe Modes")
                             ("V" find-variable "Find Variable")
                             ("v" describe-variable "Describe Variable")
                             ("b" helm-descbinds "Describe Bindings")))

          (bind-key "<f2>" (defhydra hydra-zoom ()
                             "Zoom"
                             ("i" text-scale-increase "in")
                             ("o" text-scale-decrease "out")))
          ))

;; Project Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :defer 5
  :commands (projectile-global-mode)
  :bind ("C-c a" . projectile-find-other-file)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (progn
          (setq projectile-cache-file (concat user-cache-directory "projectile.cache"))
          (setq projectile-known-projects-file (concat user-cache-directory "projectile-bookmarks.eld")))
  :config (progn (setq projectile-enable-caching t)

                 ;; (setq projectile-indexing-method 'native)
                 (add-to-list 'projectile-globally-ignored-directories "elpa")

                 (use-package helm-projectile
                   :ensure t
                   :config (progn (helm-projectile-on)
                                  (setq projectile-completion-system 'helm)

                                  (setq helm-projectile-fuzzy-match t)
                                  ))

                 ;; projectile modeline updates causes some slowdown, so let's
                 ;; change it to a static string.
                 ;; https://github.com/bbatsov/projectile/issues/657
                 (setq projectile-mode-line "Projectile")

                 (projectile-global-mode)
                 ))

;; [[https://github.com/pashinin/workgroups2][Workgroups2]] adds workspace and
;; session support to Emacs. I've found that over time, my use of helm-* to
;; switch buffers quickly has somewhat obsoleted the necessity of this feature,
;; so I've disabled it for now.
(use-package workgroups2
  :disabled t
  :config (progn (setq wg-default-session-file (concat user-cache-directory "workgroups2"))
                 (setq wg-use-default-session-file nil)

                 ;; Change prefix key (before activating WG)
                 (setq wg-prefix-key (kbd "C-c z"))

                 ;; What to do on Emacs exit / workgroups-mode exit?
                 (setq wg-emacs-exit-save-behavior nil)           ;; Options: 'save 'ask nil
                 (setq wg-workgroups-mode-exit-save-behavior nil) ;; Options: 'save 'ask nil

                 ;; Mode Line changes
                 ;; Display workgroups in Mode Line?
                 (setq wg-mode-line-display-on t) ;; Default: (not (featurep 'powerline))
                 (setq wg-flag-modified t)        ;; Display modified flags as well

                 (setq wg-mode-line-decor-left-brace  "["
                       wg-mode-line-decor-right-brace "]"
                       wg-mode-line-decor-divider     ":")

                 (workgroups-mode t)
                 ))

;; Helm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :commands (helm-mode)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-c C-f" . helm-find-files)

         ("C-x b" . helm-buffers-list)
         ("C-c u" . helm-buffers-list)

         ("C-c y" . helm-show-kill-ring))
  :config (progn
            (helm-mode t)

            (setq-default helm-mode-line-string "")

            ;; Scroll 4 lines other window using M-<next>/M-<prior>
            (setq helm-scroll-amount 4)

            ;; Do not display invisible candidates
            (setq helm-quick-update t)

            ;; (setq helm-ff-auto-update-initial-value nil)
            (setq helm-ff-smart-completion nil)

            ;; Be idle for this many seconds, before updating in delayed sources.
            (setq helm-idle-delay 0.01)

            ;; Be idle for this many seconds, before updating candidate buffer
            (setq helm-input-idle-delay 0.01)

            (setq helm-full-frame nil)
            (setq helm-split-window-default-side 'other)
            (setq helm-split-window-in-side-p t)         ;; open helm buffer inside current window, not occupy whole other window

            (setq helm-candidate-number-limit 200)

            ;; Don't loop helm sources.
            (setq helm-move-to-line-cycle-in-source nil)

            ;; Free up some visual space.
            (setq helm-display-header-line nil)

            (defun helm-cfg-use-header-line-instead-of-minibuffer ()
              ;; Enter search patterns in header line instead of minibuffer.
              (setq helm-echo-input-in-header-line t)
              (defun helm-hide-minibuffer-maybe ()
                (when (with-helm-buffer helm-echo-input-in-header-line)
                  (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
                    (overlay-put ov 'window (selected-window))
                    (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                            `(:background ,bg-color :foreground ,bg-color)))
                    (setq-local cursor-type nil))))
              (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))
            ;; (helm-cfg-use-header-line-instead-of-minibuffer)

            ;; ;; "Remove" source header text
            ;; (set-face-attribute 'helm-source-header nil :height 1.0)

            ;; ;; Save current position to mark ring when jumping to a different place
            ;; (add-hook 'helm-goto-line-before-hook #'helm-save-current-pos-to-mark-ring)

            (bind-key "C-z"   #'helm-select-action  helm-map)

            ;; Tab -> do persistent action
            (bind-key "<tab>" #'helm-execute-persistent-action helm-map)

            ;; Make Tab work in terminal. Cannot use "bind-key" since it
            ;; would detect that we already bound tab.
            (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)

            ;; (setq helm-mode-fuzzy-match t)
            ;; (setq helm-completion-in-region-fuzzy-match t)

            ;; When there is only a single directory candidate when
            ;; file-finding, don't automatically enter that directory.
            (setq helm-ff-auto-update-initial-value nil)

            (setq helm-ff-skip-boring-files t)

            ;; Fuzzy matching with flx!
            (use-package helm-flx
              :ensure t
              :init (progn (helm-flx-mode t)))

            (setq helm-apropos-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-imenu-fuzzy-match t
                  helm-recentf-fuzzy-match t
                  helm-locate-fuzzy-match nil
                  helm-M-x-fuzzy-match t
                  helm-semantic-fuzzy-match t)
            ))

;; Helm Additions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-descbinds
  :ensure t
  :commands (helm-descbinds)
  :init (progn
          (fset #'describe-bindings #'helm-descbinds)
          ))

(use-package helm-imenu
  :bind ("C-c o" . helm-imenu))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s" . helm-swoop))
  :init (progn (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map))
  :config (progn ;; disable pre-input
                 (setq helm-swoop-pre-input-function (lambda () ""))

                 (setq helm-swoop-speed-or-color t)

                 (setq helm-swoop-split-with-multiple-windows nil
                       helm-swoop-split-direction 'split-window-vertically
                       helm-swoop-split-window-function 'helm-default-display-buffer)
                 ))

;; Ido-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ido
  :ensure t
  :defer t
  :config (progn (ido-mode t)
                 (setq ido-enable-prefix nil
                       ido-enable-flex-matching t
                       ido-create-new-buffer 'always
                       ido-use-filename-at-point nil
                       ido-max-prospects 10)

                 (setq ido-save-directory-list-file (concat user-cache-directory "ido.last"))

                 ;; Always rescan buffer for imenu
                 (set-default 'imenu-auto-rescan t)

                 (add-to-list 'ido-ignore-directories "target")
                 (add-to-list 'ido-ignore-directories "node_modules")

                 ;; Use ido everywhere
                 (ido-everywhere t)

                 ;; Display ido results vertically, rather than horizontally
                 (setq ido-decorations (quote ("\n-> "
                                               ""
                                               "\n "
                                               "\n ..."
                                               "[" "]"
                                               " [No match]"
                                               " [Matched]"
                                               " [Not readable]"
                                               " [Too big]"
                                               " [Confirm]")))
                 ))

(use-package fzf
  :ensure t
  :commands (fzf fzf-directory)
  :init (progn
          (setq fzf/args "-x --color=no")
          ))

;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :preface (progn (setq evil-want-C-u-scroll t))
  :init (progn (setq evil-move-cursor-back nil)
               (setq evil-cross-lines t)
               (setq evil-intercept-esc 'always)

               (setq evil-auto-indent t)

               ;; Holy-mode (from [[https://github.com/syl20bnr/spacemacs][Spacemacs]]) for
               ;; when I want to use evil features (like evil-leader) while staying in the
               ;; emacs-state.
               (use-package holy-mode
                 :load-path "site-lisp/holy-mode"
                 :commands (holy-mode)
                 :bind ("<f12>" . holy-mode))

               ;; Hybrid-mode (from [[https://github.com/syl20bnr/spacemacs][Spacemacs]])
               (use-package hybrid-mode
                 :load-path "site-lisp/hybrid-mode"
                 :commands (hybrid-mode)
                 :config (hybrid-mode t))

               ;; (setq evil-emacs-state-cursor    '("DarkSeaGreen1"  box))
               ;; ;; (setq evil-normal-state-cursor   '("white"         box))
               ;; (setq evil-insert-state-cursor   '("DarkSeaGreen1" box))
               ;; (setq evil-visual-state-cursor   '("RoyalBlue"     box))
               ;; (setq evil-replace-state-cursor  '("red"           hollow))
               ;; (setq evil-operator-state-cursor '("CadetBlue"     box))
               )
  :config (progn (evil-mode t)

                 ;; Toggle evil-mode
                 (evil-set-toggle-key "C-\\")

                 (evil-set-initial-state 'erc-mode 'normal)
                 (evil-set-initial-state 'package-menu-mode 'normal)
                 (evil-set-initial-state 'term-mode 'emacs)

                 ;; Make ESC work more or less like it does in Vim
                 (defun init/minibuffer-keyboard-quit()
                   "Abort recursive edit.

In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
                   (interactive)
                   (if (and delete-selection-mode transient-mark-mode mark-active)
                       (setq deactivate-mark t)
                     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
                     (abort-recursive-edit)))

                 (bind-key [escape] #'init/minibuffer-keyboard-quit minibuffer-local-map)
                 (bind-key [escape] #'init/minibuffer-keyboard-quit minibuffer-local-ns-map)
                 (bind-key [escape] #'init/minibuffer-keyboard-quit minibuffer-local-completion-map)
                 (bind-key [escape] #'init/minibuffer-keyboard-quit minibuffer-local-must-match-map)
                 (bind-key [escape] #'init/minibuffer-keyboard-quit minibuffer-local-isearch-map)

                 ;; Being Emacs-y
                 (defun mhl/evil-be-emacsy ()
                   (bind-key "C-a" #'evil-beginning-of-line  evil-insert-state-map)
                   (bind-key "C-a" #'evil-beginning-of-line  evil-motion-state-map)

                   (bind-key "C-b" #'evil-backward-char      evil-insert-state-map)
                   (bind-key "C-d" #'evil-delete-char        evil-insert-state-map)

                   (bind-key "C-e" #'evil-end-of-line        evil-insert-state-map)
                   (bind-key "C-e" #'evil-end-of-line        evil-motion-state-map)

                   (bind-key "C-f" #'evil-forward-char       evil-insert-state-map)

                   (bind-key "C-k" #'evil-delete-line          evil-insert-state-map)
                   (bind-key "C-k" #'evil-delete-line          evil-motion-state-map)

                   ;; Delete forward like Emacs.
                   (bind-key "C-d" #'evil-delete-char evil-insert-state-map)

                   ;; Make end-of-line work in insert
                   (bind-key "C-e" #'end-of-line evil-insert-state-map))
                 ;; (mhl/evil-be-emacsy)

                 ;; Extra text objects
                 (defmacro define-and-bind-text-object (key start-regex end-regex)
                   (let ((inner-name (make-symbol "inner-name"))
                         (outer-name (make-symbol "outer-name")))
                     `(progn
                        (evil-define-text-object ,inner-name (count &optional beg end type)
                          (evil-select-paren ,start-regex ,end-regex beg end type count nil))
                        (evil-define-text-object ,outer-name (count &optional beg end type)
                          (evil-select-paren ,start-regex ,end-regex beg end type count t))
                        (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
                        (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

                 ;; create "il"/"al" (inside/around) line text objects:
                 (define-and-bind-text-object "l" "^\\s-*" "\\s-*$")

                 ;; create "ie"/"ae" (inside/around) entire buffer text objects:
                 (define-and-bind-text-object "e" "\\`\\s-*" "\\s-*\\'")

                 ;; Swap j,k with gj, gk
                 (bind-key "j" #'evil-next-visual-line     evil-normal-state-map)
                 (bind-key "k" #'evil-previous-visual-line evil-normal-state-map)
                 (bind-key "g j" #'evil-next-line          evil-normal-state-map)
                 (bind-key "g k" #'evil-previous-line      evil-normal-state-map)

                 ;; Other evil keybindings
                 (evil-define-operator evil-join-previous-line (beg end)
                   "Join the previous line with the current line."
                   :motion evil-line
                   (evil-previous-visual-line)
                   (evil-join beg end))

                 ;; Let K match J
                 (bind-key "K" #'evil-join-previous-line evil-normal-state-map)

                 ;; Make Y work like D
                 (bind-key "Y" (lambda ()
                                 (interactive)
                                 (evil-yank (point) (line-end-position)))
                           evil-normal-state-map)

                 ;; Kill buffer if only window with buffer open, otherwise just close
                 ;; the window.
                 (bind-key "Q" #'my-window-killer evil-normal-state-map)

                 ;; Visual indentation now reselects visual selection.
                 (bind-key ">" (lambda ()
                                 (interactive)
                                 ;; ensure mark is less than point
                                 (when (> (mark) (point))
                                   (exchange-point-and-mark)
                                   )
                                 (evil-normal-state)
                                 (evil-shift-right (mark) (point))
                                 ;; re-select last visual-mode selection
                                 (evil-visual-restore))
                           evil-visual-state-map)

                 (bind-key "<" (lambda ()
                                 (interactive)
                                 ;; ensure mark is less than point
                                 (when (> (mark) (point))
                                   (exchange-point-and-mark)
                                   )
                                 (evil-normal-state)
                                 (evil-shift-left (mark) (point))
                                 ;; re-select last visual-mode selection
                                 (evil-visual-restore))
                           evil-visual-state-map)

                 ;; Commentin'
                 (bind-key "g c c" #'comment-line-or-region evil-normal-state-map)
                 (bind-key "g c"   #'comment-line-or-region evil-visual-state-map)

                 ;; Don't quit!
                 (defadvice evil-quit (around advice-for-evil-quit activate)
                   (message "Thou shall not quit!"))
                 (defadvice evil-quit-all (around advice-for-evil-quit-all activate)
                   (message "Thou shall not quit!"))
                 ))

;; Evil Additions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-leader
  :ensure t
  :commands (evil-leader-mode)
  :init (global-evil-leader-mode t)
  :config (progn (setq evil-leader/in-all-states t
                       evil-leader/leader "SPC"
                       evil-leader/non-normal-prefix "s-")

                 (define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
                 (define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
                 (define-key evil-emacs-state-map  (kbd "M-SPC") evil-leader--default-map)
                 (define-key evil-insert-state-map (kbd "M-SPC") evil-leader--default-map)

                 (evil-leader/set-key "!" #'shell-command)

                 (evil-leader/set-key "a" #'projectile-find-other-file)

                 ;; Eval
                 (evil-leader/set-key "eb" #'eval-buffer)
                 (evil-leader/set-key "er" #'eval-region)

                 ;; Errors
                 (evil-leader/set-key "en" #'next-error)
                 (evil-leader/set-key "ep" #'previous-error)

                 ;; Files
                 (evil-leader/set-key "f" #'helm-find-files)
                 (evil-leader/set-key "z" #'fzf)           ;; Project(ile) fzf
                 (evil-leader/set-key "g" #'fzf-directory) ;; Directory fzf

                 ;; Buffers
                 (evil-leader/set-key "b" #'buffer-menu)
                 (evil-leader/set-key "k" #'ido-kill-buffer)
                 (evil-leader/set-key "u" #'helm-buffers-list)

                 (evil-leader/set-key "o" #'helm-imenu)
                 (evil-leader/set-key "x" #'helm-M-x)

                 ;; Rings
                 (evil-leader/set-key "y"  #'helm-show-kill-ring)
                 (evil-leader/set-key "rm" #'helm-mark-ring)

                 ;; Git
                 (evil-leader/set-key "m" #'magit-status)

                 ;; Projectile
                 (evil-leader/set-key "p" #'projectile-command-map)

                 ;; Swiper/Swoop
                 (evil-leader/set-key "s" #'helm-swoop)
                 (evil-leader/set-key "w" #'swiper)

                 ;; Avy integration
                 (evil-leader/set-key "SPC" #'avy-goto-word-or-subword-1)
                 (evil-leader/set-key "l"   #'avy-goto-line)

                 ;; Narrowing
                 (put 'narrow-to-region 'disabled nil)
                 (evil-leader/set-key "n r" #'narrow-to-region)
                 (evil-leader/set-key "n d" #'narrow-to-defun)
                 (evil-leader/set-key "n p" #'narrow-to-page)
                 (evil-leader/set-key "n w" #'widen)

                 ;; Expand region
                 (evil-leader/set-key "v" #'er/expand-region)

                 ;; Terminal
                 (evil-leader/set-key "t" #'open-terminal)

                 ;; Help!
                 (evil-leader/set-key "h" #'hydra-help/body)))

(use-package evil-surround
  :ensure t
  :disabled t
  :defer t
  :config (global-evil-surround-mode t))

(use-package evil-args
  :ensure t
  :init (progn
          ;; bind evil-args text objects
          (bind-key "a" #'evil-inner-arg evil-inner-text-objects-map)
          (bind-key "a" #'evil-outer-arg evil-outer-text-objects-map)

          ;; bind evil-forward/backward-args
          (bind-key "gl" #'evil-forward-arg  evil-normal-state-map)
          (bind-key "gh" #'evil-backward-arg evil-normal-state-map)
          (bind-key "gl" #'evil-forward-arg  evil-motion-state-map)
          (bind-key "gh" #'evil-backward-arg evil-motion-state-map)

          ;; bind evil-jump-out-args
          ;; (bind-key "gm" 'evil-jump-out-args evil-normal-state-map)
          ))

(use-package evil-numbers
  :ensure t
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init (progn
          ;; Instead of C-a and C-x like in Vim, let's use + and -.
          (bind-key "-" #'evil-numbers/dec-at-pt evil-normal-state-map)
          (bind-key "+" #'evil-numbers/inc-at-pt evil-normal-state-map)
          ))

(use-package evil-mc
  :ensure t
  :init (progn (global-evil-mc-mode t)))

;; Special Buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With either of these packages we can force certain buffers to open in a
;; certain location in a frame. I mostly use this for helm and compilation
;; buffers.

(use-package popwin
  :ensure t
  :defer t
  :disabled t
  :config (progn (push '("\\`\\*helm.*?\\*\\'" :regexp t :height 16) popwin:special-display-config)
                 (push '("magit" :regexp t :height 16) popwin:special-display-config)
                 (push '(".*Shell Command Output\*" :regexp t :height 16) popwin:special-display-config)
                 (push '(compilation-mode :height 16) popwin:special-display-config)

                 (popwin-mode t)
                 ))

(use-package shackle
  :ensure t
  :init (progn (shackle-mode t))
  :config (progn (setq shackle-rules
                     '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)
                       (compilation-mode :align t :ratio 0.4)
                       (t :select t)))
               ))

;; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :commands (dired)
  :config (setq dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso"))

(use-package ranger
  :ensure t
  :disabled t
  :commands (ranger)
  :config (progn
            ;; When disabling the mode you can choose to kill the buffers that
            ;; were opened while browsing the directories.
            (setq ranger-cleanup-on-disable t)

            ;; Or you can choose to kill the buffer just after you move to
            ;; another entry in the dired buffer.
            (setq ranger-cleanup-eagerly t)

            (setq ranger-show-dotfiles t)
            ))

;; Language Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sh-script
  :config (progn
            (defun disable-elec-here-doc-mode ()
              (sh-electric-here-document-mode -1))

            (add-hook 'sh-mode-hook #'disable-elec-here-doc-mode)))

(use-package cc-mode
  :config (progn (setq-default c-default-style "bsd")
                 (setq-default c-basic-offset 4)

                 (defun c-mode-common-custom ()
                   (c-set-offset 'access-label '-)
                   (c-set-offset 'inclass '++)
                   (c-set-offset 'substatement-open 0)
                   ;; (c-set-offset 'inclass 'my-c-lineup-inclass)
                   )

                 (add-hook 'c-mode-common-hook #'c-mode-common-custom)
                 ))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" . markdown-mode)
  :config (progn (defun my-markdown-mode-hook()
                   (defvar markdown-imenu-generic-expression
                     '(("title" "^\\(.*\\)[\n]=+$" 1)
                       ("h2-" "^\\(.*\\)[\n]-+$" 1)
                       ("h1" "^# \\(.*\\)$" 1)
                       ("h2" "^## \\(.*\\)$" 1)
                       ("h3" "^### \\(.*\\)$" 1)
                       ("h4" "^#### \\(.*\\)$" 1)
                       ("h5" "^##### \\(.*\\)$" 1)
                       ("h6" "^###### \\(.*\\)$" 1)
                       ("fn" "^\\[\\^\\(.*\\)\\]" 1)
                       ))
                   (setq imenu-generic-expression markdown-imenu-generic-expression))

                 (add-hook 'markdown-mode-hook #'my-markdown-mode-hook)

                 (use-package markdown-toc
                   :ensure t
                   :disabled t)
                 ))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package php-mode
  :ensure t
  :mode ("\\.php$" . php-mode))

(use-package sgml-mode
  :ensure t
  :mode ("\\.html\\'" . html-mode))

(use-package writegood-mode
  :ensure t
  :commands (writegood-mode))

;; Yasnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :init (progn (setq yas-snippet-dirs (concat user-emacs-directory "snippets")))
  :config (progn (yas-reload-all)
                 (add-hook 'prog-mode-hook #'yas-minor-mode)
                 (add-hook 'markdown-mode-hook #'yas-minor-mode)
                 ))

;; Auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package irony
  :ensure t)

(use-package company-irony
  :ensure t)

(use-package company
  :ensure t
  :bind (("C-<tab>" . company-dabbrev)
         ("M-<tab>" . company-complete)
         ("C-c C-y" . company-yasnippet))
  :config (progn
            (bind-key "C-n" #'company-select-next     company-active-map)
            (bind-key "C-p" #'company-select-previous company-active-map)

            (setq company-idle-delay 0
                  company-minimum-prefix-length 2
                  company-show-numbers nil
                  company-require-match 'never
                  company-selection-wrap-around t)

            (add-hook 'c++-mode-hook #'irony-mode)
            (add-hook 'c-mode-hook #'irony-mode)
            (add-hook 'objc-mode-hook #'irony-mode)

            ;; replace the `completion-at-point' and `complete-symbol' bindings in
            ;; irony-mode's buffers by irony-mode's function
            (defun my-irony-mode-hook ()
              (define-key irony-mode-map [remap completion-at-point]
                'irony-completion-at-point-async)
              (define-key irony-mode-map [remap complete-symbol]
                'irony-completion-at-point-async))
            (add-hook 'irony-mode-hook #'my-irony-mode-hook)
            (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

            ;; (optional) adds CC special commands to `company-begin-commands' in order to
            ;; trigger completion at interesting places, such as after scope operator
            ;; std::|
            (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)

            ;; "Iterating through back-ends that don’t apply to the current buffer is pretty fast."
            (setq company-backends (quote (company-files
                                           company-irony
                                           company-elisp
                                           company-css
                                           ;; company-eclim
                                           ;; company-clang
                                           company-capf
                                           ;; (company-dabbrev-code company-keywords)
                                           company-keywords
                                           ;; company-dabbrev
                                           )))

            ;; Add yasnippet support for all company backends
            ;; https://github.com/syl20bnr/spacemacs/pull/179
            (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

            (defun company-mode/backend-with-yas (backend)
              (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
                  backend
                (append (if (consp backend) backend (list backend))
                        '(:with company-yasnippet))))

            (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

            (use-package company-flx
              :ensure t
              :config (progn (company-flx-mode t)))

            (global-company-mode t)
            ))

;; Flycheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :defer 5
  :init (progn
          ;; Remove newline checks, since they would trigger an immediate check
          ;; when we want the idle-change-delay to be in effect while editing.
          (setq flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

          ;; Flycheck doesn't use =load-path= when checking emacs-lisp files.
          ;; Instead, it uses =flycheck-emacs-lisp-load-path=, which is empty by
          ;; default. Let's have flycheck use =load-path=!
          (setq-default flycheck-emacs-lisp-load-path 'inherit)

          (global-flycheck-mode t)
          ))

(use-package flycheck-irony
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package htmlize
  :ensure t
  :defer t)

(use-package org
  :defer t
  :config (progn (setq org-startup-indented t)
                 (setq org-replace-disputed-keys t)

                 ;; Fontify org-mode code blocks
                 (setq org-src-fontify-natively t)

                 ;; Ellipses blend in too well. Make them more distict!
                 (setq org-ellipsis " […]")

                 ;; Create "Table of Contents" without exporting (useful for
                 ;; github README.org files, for example)
                 (use-package toc-org
                   :ensure t
                   :disabled t
                   :config (progn
                             (add-hook 'org-mode-hook #'toc-org-enable)))
                 ))

;; Miscellaneous Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pretty package listings.
(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :config (progn (setq paradox-execute-asynchronously t)))

;; Emacs by default leaves a "watermark" when you leave a channel (The default
;; part reason declares to the universe that you're using Emacs/ERC). Let's make
;; that a bit more generic.
(use-package erc
  :defer t
  :commands (erc)
  :config (progn (setq erc-part-reason 'erc-part-reason-various)
                 (setq erc-part-reason-various-alist
                       '(("^$" "Goodbye.")))

                 (setq erc-quit-reason 'erc-quit-reason-various)
                 (setq erc-quit-reason-various-alist
                       '(("^$" "Goodbye.")))
                 ))

(use-package znc
  :defer t
  :disabled t
  :ensure t)

(use-package twittering-mode
  :defer t
  :ensure t
  :commands (twittering-mode)
  :init (progn (setq twittering-use-master-password t)
               (setq twittering-icon-mode t)
               (setq twittering-allow-insecure-server-cert t)

               (add-hook 'twittering-mode-hook #'disable-show-trailing-whitespace)
               ))

;; Finishing Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load custom init file at the end.
(setq custom-file user-custom-file)
(load user-custom-file t)

;; Garbage collection of larger blocks of data can be slow, causes pauses during
;; operation. Let's reduce the size of the threshold to a smaller value after
;; most of our init is complete.
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold (* 10 1024 1024))
                              ))

;; Make sure emacs is daemonized.
(use-package server
  :config (unless (server-running-p)
            (server-start)))

;; Print out some timing data.
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]" ,load-file-name
                          elapsed))) t))
