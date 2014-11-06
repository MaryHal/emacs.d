
;;; Code:

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(defconst user-custom-file (concat user-emacs-directory "custom.el"))
(defconst user-cache-directory (concat user-emacs-directory "cache/"))

;; Customize Configuration
(setq custom-file user-custom-file)
(load user-custom-file t)



;; Preload Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things that should be set early just in case something bad happens

;; Turn off backup files
(setq make-backup-files nil)



;; Package Management (req-package) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (package-initialize))
(require 'cl)

;; this code block is just a bootstrapping helper
;; for clean emacs installation with copy of this configs repo.
;; i noticed, that clean emacs installation has empty
;; package-archive-contents variable.
;; it happening because you have not any packages descriptions into
;; ~/.emacs.d/elpa/archives directory. so there is no information
;; about req-package at first emacs launch.
;; that's why i check package-archive-contents and fetch descriptions
;; in case this variable is empty and then i'm tring to install it
;; using package-install function.
(defun package-try-install (package)
  "installs package if not installed"
  (let* ((ARCHIVES (if (null package-archive-contents)
                       (progn (package-refresh-contents)
                              package-archive-contents)
                     package-archive-contents))
         (AVAIL (some (lambda (elem)
                        (eq (car elem) package))
                      ARCHIVES)))
    (if AVAIL
        (package-install package))))

(if (null (require 'req-package "req-package" t))
    ;; requre failed, it might be first start.
    ;; try to fetch archives and install req-package.
    ;; then require again.
    (progn (package-try-install 'req-package)
           (require 'req-package)))



;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Set transparency of emacs
(defun set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Switch to previously selected buffer.
(defun backward-buffer ()
  (interactive)
  "Switch to previously selected buffer."
  (let* ((list (cdr (buffer-list)))
         (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
        (setq list (cdr list))
        (setq buffer (car list))))
    (bury-buffer)
    (switch-to-buffer buffer)))

;; Opposite of backward-buffer.
(defun forward-buffer ()
  (interactive)
  "Opposite of backward-buffer."
  (let* ((list (reverse (buffer-list)))
         (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
        (setq list (cdr list))
        (setq buffer (car list))))
    (switch-to-buffer buffer)))

;; Split functions
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; from https://gist.github.com/3402786
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (set-register '_ (list (current-window-configuration)))
           (delete-other-windows))))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

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



;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Transparency after load theme...
;; On Linux, if in terminal, clear the background. If GUI, set background to black and set
;; frame transparency.
(defadvice load-theme (after load-theme activate compile)
  (if (string= system-type "gnu/linux")
      (if (string= window-system "x")
          (progn (set-frame-parameter (selected-frame) 'alpha '(90 90))
                 (add-to-list 'default-frame-alist '(alpha 90 90))
                 (set-face-attribute 'default nil :background "black")
                 (set-face-attribute 'fringe nil :background "black")
                 )
        (progn (when (getenv "DISPLAY")
                 (set-face-attribute 'default nil :background "unspecified-bg")
                 ))
        )))

;; Rebalance windows after splitting right
(defadvice split-window-right
    (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-right)

;; Rebalance windows after splitting horizontally
(defadvice split-window-horizontally
    (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-horizontally)

;; Balance windows after window close
(defadvice delete-window
    (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'delete-window)



;; Sane Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs will run garbage collection after `gc-cons-threshold' bytes of consing.
;; The default value is 800,000 bytes, or ~ 0.7 MiB.
;; By increasing to 10 MiB we reduce the number of pauses due to garbage collection.
(setq gc-cons-threshold (* 10 1024 1024))

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
;; (setq delete-by-moving-to-trash t)

;; UTF-8 please
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Easily navigate sillycased words
(global-subword-mode t)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Useful frame title, that show either a file or a buffer name (if the buffer isn't visiting a file)
;; (setq frame-title-format
;;       '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
;;                                                     (abbreviate-file-name (buffer-file-name))
;;                                                   "%b"))))

;; backwards compatibility as default-buffer-file-coding-system
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

(req-package autorevert
  :config (progn (setq global-auto-revert-non-file-buffers t)
                 (setq auto-revert-verbose nil)

                 (global-auto-revert-mode t)
                 ))

(req-package simple
  :config (progn (setq shift-select-mode nil)

                 ;; ;; Show active region
                 ;; (transient-mark-mode t)
                 ;; (make-variable-buffer-local 'transient-mark-mode)
                 ;; (put 'transient-mark-mode 'permanent-local t)
                 ;; (setq-default transient-mark-mode t)

                 ;; Nic says eval-expression-print-level needs to be set to 0 (turned off) so
                 ;; that you can always see what's happening.
                 (setq eval-expression-print-level nil)
                 ))

(req-package jka-compr-hook
  :config (auto-compression-mode))

(req-package delsel
  :config (delete-selection-mode t))

(req-package tramp
  :init (setq tramp-default-method "ssh"))

(req-package recentf
  :config (recentf-mode t)
  :init (progn (setq recentf-save-file (concat user-cache-directory "recentf"))
               (setq recentf-max-saved-items 100)
               (setq recentf-max-menu-items 50)
               ))

(req-package uniquify
  :init (progn (setq uniquify-buffer-name-style 'forward
                     uniquify-separator "/"
                     uniquify-ignore-buffers-re "^\\*" ;; leave special buffers alone
                     uniquify-after-kill-buffer-p t)
               ))

(req-package ediff
  :init (progn (setq ediff-diff-options "-w")
               (setq ediff-split-window-function 'split-window-horizontally)
               (setq ediff-window-setup-function 'ediff-setup-windows-plain)
               ))

;; (req-package mouse
;;              :config (progn (xterm-mouse-mode t)
;;                             (defun track-mouse (e))
;;                             (setq mouse-sel-mode t)
;;                             ))

;; Seed the random number generator
(random t)



;; Backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable backup
(setq backup-inhibited t)

;; Disable auto save
(auto-save-mode nil)
(setq auto-save-default nil)
(with-current-buffer (get-buffer "*scratch*")
  (auto-save-mode -1))

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



;; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package dired
  :commands dired
  :config (setq dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")
  )



;; Special Buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package popwin
  :config (progn (push '("helm" :regexp t :height 16) popwin:special-display-config)
                 (push "*Shell Command Output*" popwin:special-display-config)
                 (push '(compilation-mode :noselect t) popwin:special-display-config)

                 (popwin-mode t)
                 ))



;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default window metrics
(setq default-frame-alist
      '((top   . 10) (left   . 2)
        (width . 80) (height . 30)
        ))

;; Set font
(if (string= system-type "windows-nt")
    ;; If Windows
    (progn (add-to-list 'default-frame-alist '(font . "Consolas 10")))
  ;; If not Windows
  (progn (add-to-list 'default-frame-alist '(font . "Inconsolata 10")))
  )

;; Load custom theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/"))
;; (load-theme 'enox t)
(load-theme 'smyx t)



(req-package smart-mode-line
  :config (progn (setq-default sml/line-number-format " %3l")
                 (setq-default sml/col-number-format  "%2c")

                 (line-number-mode t)   ;; have line numbers and
                 (column-number-mode t) ;; column numbers in the mode line

                 (sml/setup)
                 ))

(req-package rich-minority
  :config (progn (setq rm-blacklist nil)
                 (setq rm-whitelist " Wrap")
                 ;; (rich-minority-mode t)
                 ))

(req-package menu-bar
  :config
  (menu-bar-mode -1))

(req-package tool-bar
  :config
  (tool-bar-mode -1))

(req-package tooltip
  :config
  (tooltip-mode -1))

(req-package scroll-bar
  :config
  (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


(setq visible-bell nil
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil)



;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No Tabs, just spaces
(setq-default indent-tabs-mode nil)

;; Don't add newlines when cursor goes past end of file
(setq next-line-add-newlines nil)
(setq require-final-newline nil)

;; Don't Blink Cursor
(blink-cursor-mode -1)

;; Smoother Scrolling
(setq scroll-margin 8
      scroll-conservatively 9999
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; (req-package elec-pair
;;              :config (electric-pair-mode t))

;; (req-package electric
;;              :config (electric-indent-mode t))

(req-package fringe
  :config (progn (set-fringe-mode (cons 0 0))

                 ;; Empty line indicators in the fringe
                 (setq-default indicate-empty-lines nil)
                 ))

;; Set margins to 1
(setq-default left-margin-width 1
              right-margin-width 1)
(set-window-buffer nil (current-buffer))

(req-package git-gutter
  :config (global-git-gutter-mode t))

(req-package paren
  :config (progn (show-paren-mode t)
                 (setq show-paren-delay 0)
                 ))

(req-package highlight-parentheses
  :config (progn (defun hl-parens-hook()
                   (highlight-parentheses-mode 1))
                 (add-hook 'prog-mode-hook 'hl-parens-hook)
                 ))

;; Whitespace-style
(setq-default show-trailing-whitespace t)

(req-package anzu
  :config (global-anzu-mode t))

(req-package aggressive-indent
  :config (global-aggressive-indent-mode t))



;; Clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; I prefer using the "clipboard" selection (the one the
        ;; typically is used by c-c/c-v) before the primary selection
        ;; (that uses mouse-select/middle-button-click)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Callback for when user pastes
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))



;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package evil
  :pre-load (progn (setq evil-want-C-u-scroll t)
                   (setq evil-move-cursor-back nil)
                   (setq evil-cross-lines t)
                   (setq evil-intercept-esc 'always)

                   (setq evil-auto-indent t)
                   )
  :config (progn (evil-mode t)

                 ;; Toggle evil-mode
                 (evil-set-toggle-key "C-\\")

                 ;; List of modes that should start up in Evil state.
                 (defvar dotemacs-evil-state-modes
                   '(fundamental-mode
                     text-mode
                     prog-mode
                     sws-mode
                     dired-mode
                     comint-mode
                     log-edit-mode
                     compilation-mode))

                 (defun my-enable-evil-mode ()
                   (if (apply 'derived-mode-p dotemacs-evil-state-modes)
                       (turn-on-evil-mode)))
                 (add-hook 'after-change-major-mode-hook 'my-enable-evil-mode)

                 (evil-set-initial-state 'package-menu-mode 'normal)

                 (add-hook 'compilation-mode-hook '(lambda ()
                                                     (local-unset-key "g")
                                                     (local-unset-key "h")
                                                     (evil-define-key 'motion compilation-mode-map "r" 'recompile)
                                                     (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char)))

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

                 (define-key minibuffer-local-map [escape] 'init/minibuffer-keyboard-quit)
                 (define-key minibuffer-local-ns-map [escape] 'init/minibuffer-keyboard-quit)
                 (define-key minibuffer-local-completion-map [escape] 'init/minibuffer-keyboard-quit)
                 (define-key minibuffer-local-must-match-map [escape] 'init/minibuffer-keyboard-quit)
                 (define-key minibuffer-local-isearch-map [escape] 'init/minibuffer-keyboard-quit)

                 ;; insert one or several line below without changing current evil state
                 (defun evil-insert-line-below (count)
                   "Insert one of several lines below the current point's line without changing
the current state and point position."
                   (interactive "p")
                   (save-excursion
                     (evil-save-state (evil-open-below count))))

                 ;; insert one or several line above without changing current evil state
                 (defun evil-insert-line-above (count)
                   "Insert one of several lines above the current point's line without changing
the current state and point position."
                   (interactive "p")
                   (save-excursion
                     (evil-save-state (evil-open-above count))))

                 ;; Make end-of-line work in insert
                 (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

                 ;; Other evil keybindings
                 (evil-define-operator evil-join-previous-line (beg end)
                   "Join the previous line with the current line."
                   :motion evil-line
                   (evil-previous-visual-line)
                   (evil-join beg end))

                 ;; gj gk by default
                 (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
                 (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

                 ;; Let K match J
                 (define-key evil-normal-state-map (kbd "K") 'evil-join-previous-line)

                 ;; Make Y work like D
                 (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

                 ;; Kill buffer if only window with buffer open, otherwise just close the window.
                 (define-key evil-normal-state-map (kbd "Q") 'my-window-killer)

                 ;; Visual indentation now reselects visual selection.
                 (define-key evil-visual-state-map ">" (lambda ()
                                                         (interactive)
                                                         ;; ensure mark is less than point
                                                         (when (> (mark) (point))
                                                           (exchange-point-and-mark)
                                                           )
                                                         (evil-normal-state)
                                                         (evil-shift-right (mark) (point))
                                                         ;; re-select last visual-mode selection
                                                         (evil-visual-restore)))

                 (define-key evil-visual-state-map "<" (lambda ()
                                                         (interactive)
                                                         ;; ensure mark is less than point
                                                         (when (> (mark) (point))
                                                           (exchange-point-and-mark)
                                                           )
                                                         (evil-normal-state)
                                                         (evil-shift-left (mark) (point))
                                                         ;; re-select last visual-mode selection
                                                         (evil-visual-restore)))

                 ;; "Unimpaired"
                 (define-key evil-normal-state-map (kbd "[ SPC") 'evil-insert-line-above)
                 (define-key evil-normal-state-map (kbd "] SPC") 'evil-insert-line-below)
                 (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
                 (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
                 (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
                 (define-key evil-normal-state-map (kbd "] q") 'next-error)

                 ;; Bubble Text up and down. Works with regions.
                 (define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
                 (define-key evil-normal-state-map (kbd "] e") 'move-text-down)

                 ;; Commentin'
                 (define-key evil-normal-state-map (kbd "g c c") '(lambda ()
                                                                    (interactive)
                                                                    (comment-or-uncomment-region
                                                                     (line-beginning-position)
                                                                     (line-end-position))
                                                                    ))
                 (define-key evil-visual-state-map (kbd "g c") 'comment-or-uncomment-region)
                 ))

(req-package evil-leader
  :require (evil helm helm-projectile helm-swoop magit projectile)
  :config (progn (setq evil-leader/in-all-states t
                       evil-leader/leader "SPC"
                       evil-leader/non-normal-prefix "s-")

                 (global-evil-leader-mode t)

                 ;; make leader available in visual mode
                 (define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
                 (define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
                 (define-key evil-emacs-state-map (kbd "SPC") evil-leader--default-map)

                 (evil-leader/set-key "a" 'projectile-find-other-file)

                 (evil-leader/set-key "c" '(lambda()
                                             (interactive)
                                             (file-name-directory (or load-file-name buffer-file-name))))

                 ;; Eval
                 (evil-leader/set-key "eb" 'eval-buffer)
                 (evil-leader/set-key "er" 'eval-region)

                 ;; Files
                 (evil-leader/set-key "f" 'helm-find-files)

                 ;; Buffers
                 (evil-leader/set-key "b" 'buffer-menu)
                 (evil-leader/set-key "k" 'ido-kill-buffer)
                 (evil-leader/set-key "u" 'helm-buffers-list)

                 (evil-leader/set-key "o" 'helm-imenu)
                 (evil-leader/set-key "x" 'helm-M-x)

                 ;; Git
                 (evil-leader/set-key "m" 'magit-status)

                 ;; Projectile
                 (evil-leader/set-key "p" 'helm-projectile)

                 ;; Swoop
                 (evil-leader/set-key "s" 'helm-swoop)

                 ;; Terminal
                 (evil-leader/set-key "t"  '(lambda()
                                              (interactive)
                                              (shell-command "$TERMINAL")))
                 ))

(req-package evil-surround
  :require evil
  :config (global-evil-surround-mode t))

(req-package evil-args
  :require evil
  :config (progn
            ;; bind evil-args text objects
            (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
            (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

            ;; bind evil-forward/backward-args
            (define-key evil-normal-state-map "gl" 'evil-forward-arg)
            (define-key evil-normal-state-map "gh" 'evil-backward-arg)
            (define-key evil-motion-state-map "gl" 'evil-forward-arg)
            (define-key evil-motion-state-map "gh" 'evil-backward-arg)

            ;; bind evil-jump-out-args
            ;; (define-key evil-normal-state-map "gm" 'evil-jump-out-args)
            ))



;; Helm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package helm
  :bind (("M-x" . helm-M-x)
         )
  :config (progn (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to do persistent action
                 (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ;; make TAB works in terminal
                 (define-key helm-map (kbd "C-z")  'helm-select-action)              ;; list actions using C-z
                 (define-key helm-map (kbd "C-w") 'backward-kill-word)

                 (setq helm-scroll-amount 4             ;; scroll 4 lines other window using M-<next>/M-<prior>
                       helm-quick-update t              ;; do not display invisible candidates
                       helm-idle-delay 0.01             ;; be idle for this many seconds, before updating in delayed sources.
                       helm-input-idle-delay 0.01       ;; be idle for this many seconds, before updating candidate buffer
                       helm-ff-search-library-in-sexp t ;; search for library in `require' and `declare-function' sexp.

                       helm-full-frame nil
                       ;; helm-split-window-default-side 'other ;; open helm buffer in another window
                       ;; helm-split-window-in-side-p t         ;; open helm buffer inside current window, not occupy whole other window
                       ;; helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                       ;;                                     '(picture-mode artist-mode))
                       helm-candidate-number-limit 200         ;; limit the number of displayed canidates
                       helm-M-x-requires-pattern 0             ;; show all candidates when set to 0
                       helm-ff-file-name-history-use-recentf t
                       ;; helm-move-to-line-cycle-in-source t     ;; move to end or beginning of source
                       ;;                                         ;; when reaching top or bottom of source.

                       ;; ido-use-virtual-buffers t      ;; Needed in helm-buffers-list
                       helm-buffers-fuzzy-matching t     ;; fuzzy matching buffer names when non--nil
                       ;; useful in helm-mini that lists buffers
                       )

                 ;; Save current position to mark ring when jumping to a different place
                 (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

                 (helm-mode t)
                 ))

(req-package helm-swoop
  :require helm
  :config (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch))



;; Ido-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package ido
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
                 (ido-everywhere 1)

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



;; Projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package projectile
  :config (progn
            (setq projectile-cache-file (concat user-cache-directory "projectile.cache"))
            (setq projectile-known-projects-file (concat user-cache-directory "projectile-bookmarks.eld"))

            (setq projectile-enable-caching t)

            ;; (setq projectile-indexing-method 'native)

            (add-to-list 'projectile-globally-ignored-directories "elpa")
            (add-to-list 'projectile-globally-ignored-directories ".cache")

            (setq projectile-completion-system 'helm)

            (projectile-global-mode t)
            ))



;; Language Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package cc-mode
  :config (progn
            (setq-default c-default-style "bsd")
            (setq-default c-basic-offset 4)

            (defun c-mode-common-custom ()
              (c-set-offset 'access-label '-)
              (c-set-offset 'inclass '++)
              (c-set-offset 'substatement-open 0)
              ;; (c-set-offset 'inclass 'my-c-lineup-inclass)
              )

            (add-hook 'c-mode-common-hook 'c-mode-common-custom)
            ))

(req-package markdown-mode
  :config (progn
            (defun my-markdown-mode-hook()
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

            (add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
            ))

;; (req-package js2-mode
;;              :config (js2-highlight-level 3))

(req-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  )

(req-package sgml-mode
  :mode (("\\.html\\'" . html-mode)))



;; Auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package company
  :require (irony company-irony)
  :config (progn (setq-default company-idle-delay 0)
                 (setq-default company-minimum-prefix-length 1)
                 ;; (setq-default company-show-numbers t)

                 (add-hook 'c++-mode-hook 'irony-mode)
                 (add-hook 'c-mode-hook 'irony-mode)
                 (add-hook 'objc-mode-hook 'irony-mode)

                 ;; replace the `completion-at-point' and `complete-symbol' bindings in
                 ;; irony-mode's buffers by irony-mode's function
                 (defun my-irony-mode-hook ()
                   (define-key irony-mode-map [remap completion-at-point]
                     'irony-completion-at-point-async)
                   (define-key irony-mode-map [remap complete-symbol]
                     'irony-completion-at-point-async))
                 (add-hook 'irony-mode-hook 'my-irony-mode-hook)

                 (setq-default company-backends (quote (company-files
                                                        company-irony
                                                        company-elisp
                                                        ;; company-yasnippet
                                                        ;; company-css
                                                        ;; company-eclim
                                                        ;; company-clang
                                                        ;; company-capf
                                                        (company-dabbrev-code company-keywords)
                                                        company-dabbrev
                                                        )))

                 ;; (optional) adds CC special commands to `company-begin-commands' in order to
                 ;; trigger completion at interesting places, such as after scope operator
                 ;; std::|
                 (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

                 (global-company-mode t)

                 (define-key company-active-map (kbd "C-n") 'company-select-next)
                 (define-key company-active-map (kbd "C-p") 'company-select-previous)
                 ))



;; Yasnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package yasnippet
  :config (progn (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
                 (yas-global-mode t)
                 ))



;; Org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package org
  :config (progn (setq org-replace-disputed-keys t)

                 ;; Fontify org-mode code blocks
                 (setq org-src-fontify-natively t)
                 ))

;; Extra Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove suspend-frame. Three times.
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
(put 'suspend-frame 'disabled t)

;; Easier version of "C-x k" to kill buffer
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-k") 'kill-buffer)



;; Finishing Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(req-package-finish)
