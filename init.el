
;;; Code:

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

;; Bootstrap use-package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default use-package-enable-imenu-support t)

;; This file uses [[https://github.com/jwiegley/use-package][use-package]] to
;; install and configure third-party packages. Since use-package itself is a
;; third-party package, we need to handle the case for when we have a fresh
;; install.

(when (>= emacs-major-version 24)
  (setq package-enable-at-startup nil)
  (package-initialize)
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0)

;; Very important, init-file-encompassing packages
(use-package general
  :ensure t
  :config (progn
            (setq general-default-keymaps '(evil-normal-state-map
                                            evil-visual-state-map
                                            evil-operator-state-map
                                            evil-insert-state-map
                                            evil-emacs-state-map))

            (setq general-default-prefix "SPC")
            (setq general-default-non-normal-prefix "M-SPC")))

;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-loadpath (&rest dirs)
  "Add DIRS to loadpath."
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(defun sudo-edit (&optional arg)
  "Open file (ARG) with root permissions."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my-window-killer ()
  "Closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))

(defun copy-to-end-of-line ()
  "Copy to the end of the line, even if no region is selected.
Meant to replace 'kill-ring-save'."
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy ARG lines in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy ARG lines into the kill ring."
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  "Yank region or until the end of the line ARG."
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun create-scratch-buffer ()
  "Create a new scratch buffer to work in.  Could be named *scratch* -> *scratchX*."
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

(defun open-terminal ()
  "Just open a terminal in the cwd using the $TERMINAL environment variable."
  (interactive)
  (call-process-shell-command (concat "eval $TERMINAL") nil 0))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun do-not-quit ()
  "Alternate function to point quit keybinds and stuff to."
  (interactive)
  (message "Thou shall not quit!"))

(defun get-active-minor-modes ()
  "Get a list of active minor-mode symbols."
  (cl-remove-if (lambda (m) (and (boundp m) (symbol-value m)))
                minor-mode-list))

(defun what-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Minor mode: "
                          (get-active-minor-modes))))
  (describe-minor-mode-from-symbol (intern mode)))

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
(setq echo-keystrokes 0.02)

;; Move files to trash when deleting
;; (setq delete-by-moving-to-trash t)

(setq-default fill-column 80)

;; ;; Easily navigate sillyCased words
;; (global-subword-mode t)

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
  (setq buffer-file-coding-system 'utf-8-unix))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)
(setq font-lock-maximum-decoration nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Window Rebalancing
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; In this init file we also set gc-cons-threshold to a very high value then
;; reduce it once we're done loading.
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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
  :init (progn (setq tramp-ssh-controlmaster-options ""))
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
  :config (progn
            (winner-mode t)))

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

(use-package esup
  :ensure t
  :commands esup)

;; Keep .emacs.d clean
(use-package no-littering
  :disabled t
  :ensure t)

;; Backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable backup
(setq backup-inhibited t)

;; Disable auto save
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
(global-unset-key (kbd "ESC ESC ESC"))

;; Auto-indent on RET
(bind-key (kbd "RET") #'newline-and-indent)

;; replace with [r]eally [q]uit
(bind-key "C-x r q" #'save-buffers-kill-terminal)
(bind-key "C-x C-c" #'do-not-quit)

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
(bind-key "C-c u"    #'switch-to-buffer)
(bind-key "C-x k"    #'my-window-killer)
(bind-key "C-x C-k"  #'my-window-killer)

;; Eval
(bind-key "C-c v"    #'eval-buffer)
(bind-key "C-c r"    #'eval-region)

(bind-key "C-x C-e"  #'eval-and-replace)

(bind-key "C-c C-t"  #'open-terminal)

;; (bind-key "C-;"      #'comment-line-or-region)
(bind-key "C-;"      #'comment-line)
(bind-key "M-i"      #'back-to-indentation)

(bind-key "C-."      #'hippie-expand)
;; (bind-key "C-."      #'dabbrev-expand)

;; Character-targeted movements
(use-package misc
  :bind (("M-z" . zap-up-to-char)))

(use-package jump-char
  :ensure t
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

;; (dotimes (n 10)
;;   (global-unset-key (kbd (format "M-%d" n)))
;;   (global-unset-key (kbd (format "C-%d" n))))

;; (bind-key "M-9"      #'backward-sexp)
;; (bind-key "M-0"      #'forward-sexp)

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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package paren
  :config (progn (show-paren-mode t)

                 ;; Show matched parens even in selections
                 (setq show-paren-priority -50)

                 ;; Immediately match parens
                 (setq show-paren-delay 0)

                 (setq show-paren-style 'parenthesis)
                 ))

(use-package highlight-parentheses
  :ensure t
  :config (progn
            (defun hl-parens-hook ()
              (highlight-parentheses-mode t))
            (add-hook 'prog-mode-hook #'hl-parens-hook)
            ))

(use-package highlight-leading-spaces
  :ensure t
  :disabled t
  :defer t
  :init (progn (add-hook 'prog-mode-hook 'highlight-leading-spaces-mode)))

(use-package highlight-indent-guides
  :ensure t
  :disabled t
  :config (progn
            (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
            (set-face-background 'highlight-indent-guides-even-face "#101010")
            (set-face-background 'highlight-indent-guides-odd-face  "#0B0B0B")
            ))

(use-package term
  :defer t
  :config (progn
            (defun disable-scroll-margin ()
              (setq-local scroll-margin 0))
            (add-hook 'term-mode-hook #'disable-scroll-margin)
            ))

(use-package whitespace
  :defer t
  :init (progn
          ;; Only show trailing whitespace in programming modes
          (defun enable-show-trailing-whitespace ()
            (setq show-trailing-whitespace t))
          (add-hook 'prog-mode-hook #'enable-show-trailing-whitespace)
          ))

(use-package imenu
  :config (progn
            ;; Always rescan buffer for imenu
            (set-default 'imenu-auto-rescan t)
            ))

(use-package avy
  :ensure t
  :commands (avy-goto-word-or-subword-1)
  :config (progn (setq avy-keys
                       '(?c ?a ?s ?d ?e ?f ?h ?w ?y ?j ?k ?l ?n ?m ?v ?r ?u ?p))
                 ))

(use-package swiper
  :ensure t
  :defer t
  :init (progn
            (setq ivy-re-builders-alist
                  '((t . ivy--regex-fuzzy)))

            ;; (setq ivy-re-builders-alist
            ;;       '((t . ivy--regex-plus)))

            (setq ivy-height 20)
            (setq ivy-format-function 'ivy-format-function-arrow)
            (setq ivy-count-format "%d/%d ")

            ;; (setq ivy-display-style 'fancy)

            (ivy-mode t)

            (setq projectile-completion-system 'ivy)
            (setq magit-completing-read-function 'ivy-completing-read)

            (defun mhl/swiper-recenter (&rest args)
              "recenter display after swiper"
              (recenter))
            (advice-add 'swiper--cleanup :after #'mhl/swiper-recenter)

            (use-package smex
              :ensure t
              :init (progn (setq smex-save-file (concat user-cache-directory "smex-items"))))

            (use-package counsel
              :ensure t
              :defer t
              :init (progn (counsel-mode t)
                           (bind-key (kbd "M-x")   #'counsel-M-x)
                           (bind-key (kbd "C-c x") #'counsel-M-x)
                           (bind-key (kbd "C-c o") #'counsel-imenu)
                           (bind-key (kbd "C-c l") #'ivy-resume)
                           (bind-key (kbd "C-c y") #'counsel-yank-pop)
                           (bind-key (kbd "C-c s") #'swiper))
              :config (progn
                        (advice-add 'counsel-imenu :after #'mhl/swiper-recenter)
                        (setq counsel-yank-pop-separator (concat "\n\n" (make-string 70 ?-) "\n\n"))
                        ))

            ;; (use-package counsel-projectile
            ;;   :ensure t)
            )
  :config (progn
            (defun ivy-insert-action (x)
              (with-ivy-window
                (insert x)))

            (ivy-set-actions
             t
             '(("I" ivy-insert-action "insert")
               ("W" kill-new "save to kill ring")))
  ))

(use-package anzu
  :ensure t
  :config (progn
            (setq anzu-cons-mode-line-p nil)
            (global-anzu-mode t)

            (general-define-key "M-%"   #'anzu-query-replace)
            (general-define-key "C-M-%" #'anzu-query-replace-regexp)))

(use-package electric
  :config (electric-indent-mode t))

(use-package aggressive-indent
  :ensure t
  :disabled t
  :config (global-aggressive-indent-mode t))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package viking-mode
  :ensure t
  :disabled t
  :config (progn
            (setq viking-use-expand-region-when-loaded t)
            (global-viking-mode)))

(use-package embrace
  :ensure t
  :defer t
  :bind (("C-," . embrace-commander)))

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

(use-package which-key
  :ensure t
  :config (progn
            (which-key-mode t)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config (progn (setq mc/list-file (concat user-cache-directory "mc-lists.el"))

                 (setq mc/unsupported-minor-modes '(company-mode
                                                    auto-complete-mode
                                                    flyspell-mode
                                                    jedi-mode))

                 ;; (global-unset-key (kbd "M-<down-mouse-1>"))
                 ;; (bind-key "M-<mouse-1>" #'mc/add-cursor-on-click)
                 ))

(use-package ag
  :ensure t
  :commands (ag ag-regexp)
  :init (progn
          (use-package helm-ag
            :ensure t
            :disabled t
            :commands (helm-ag))
          ))

(use-package ripgrep
  :ensure t
  :init (progn
          (use-package projectile-ripgrep
            :ensure t
            :config (progn
                      (bind-key (kbd "s r") 'projectile-ripgrep projectile-command-map)
                      ))
          ))

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go)
  :init (progn
          (general-define-key "j" #'dumb-jump-go))
  :config (progn
            (setq dumb-jump-prefer-searcher 'rg)
            (setq dumb-jump-selector 'ivy)))

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

(use-package beacon
  :if (window-system)
  :ensure t
  :disabled t
  :config (progn
            ;; (add-to-list 'beacon-dont-blink-commands 'fzf)
            ;; (add-to-list 'beacon-dont-blink-commands 'fzf-directory)

            (setq beacon-blink-when-window-scrolls t)
            (setq beacon-blink-when-window-changes t)
            ;; (setq beacon-blink-when-point-moves 2)

            (setq beacon-color 0.8)

            (beacon-mode t)))

(use-package string-edit
  :ensure t
  :defer t)

(use-package elec-pair
  :disabled t
  :config (electric-pair-mode t))

(use-package smartparens
  :ensure t
  :disabled t
  :init (progn
          (use-package smartparens-config)
          (bind-key (kbd "C-<left>") 'sp-backward-slurp-sexp smartparens-mode-map)
          (bind-key (kbd "C-<right>") 'sp-forward-slurp-sexp smartparens-mode-map)
          (bind-key (kbd "C-M-<left>") 'sp-backward-barf-sexp smartparens-mode-map)
          (bind-key (kbd "C-M-<right>") 'sp-forward-barf-sexp smartparens-mode-map)

          (add-hook 'prog-mode-hook #'smartparens-mode)
          ))

(use-package region-state
  :ensure t
  :disabled t
  :init (progn
          (region-state-mode t)
          ))

(use-package vlf
  :disabled t
  :ensure t)

(use-package persistent-scratch
  :ensure t
  :init (progn
          (persistent-scratch-setup-default)
          ))

(use-package color-identifiers-mode
  :ensure t
  :disabled t
  :config (progn
            (global-color-identifiers-mode)
            ))

;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Frame Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set margins in terminal
(when (not (display-graphic-p))
  (setq-default left-margin-width 1
                right-margin-width 1))

(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (left-fringe . 0) (right-fringe . 0)
        ))

;; Remove GUI elements
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))

;; tooltips in echo-area
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))
(unless (eq window-system 'mac)
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1)))

;; No splash screen please
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq visible-bell nil
      truncate-partial-width-windows nil)

;; Modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-mode-line
  :ensure t
  :config (progn
            ;; I prefer hiding all minor modes by default.
            (use-package rich-minority
              :ensure t
              :config (progn (setq rm-blacklist nil)
                             (setq rm-whitelist " Wrap")

                             ;; "You don't need to activate rich-minority-mode if you're using smart-mode-line"
                             (rich-minority-mode t)
                             ))
            (setq-default sml/line-number-format " %3l")
            (setq-default sml/col-number-format  "%2c")

            (line-number-mode t)   ;; have line numbers and
            (column-number-mode t) ;; column numbers in the mode line

            (setq sml/theme nil)
            (sml/setup)
            ))


(use-package telephone-line
  :ensure t
  :disabled t
  :config (progn
            ;; Need to create custom segments
            (use-package telephone-line-utils)

            ;; Set default separators: choose either of them
            (setq telephone-line-primary-left-separator 'telephone-line-identity-left)
            (setq telephone-line-primary-right-separator 'telephone-line-identity-right)
            ;; OR
            ;; (setq telephone-line-primary-left-separator 'telephone-line-cubed-left)
            ;; (setq telephone-line-primary-right-separator 'telephone-line-cubed-right)

            ;; ;; Set subseparator
            ;; (if window-system
            ;;     (progn
            ;;       (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)
            ;;       (setq telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)))

            ;;;; Custom segments

            ;; Example of color string segment
            ;; (telephone-line-defsegment* my-color-segment
            ;;   (propertize "some-string" 'face `(:foreground "green")))

            ;; TODO: Rewrite using assoc and defvar
            ;; Display major mode
            (telephone-line-defsegment* my-major-mode-segment
              (let ((mode (cond
                           ((string= mode-name "Fundamental") "Text")
                           ((string= mode-name "Emacs-Lisp") "Elisp")
                           ((string= mode-name "Javascript-IDE") "Javascript")
                           (t mode-name))))
                (propertize mode 'face `(:foreground "#F0F0EF"))
                ))

            ;; Display evil state
            (telephone-line-defsegment* my-evil-segment
              (if (telephone-line-selected-window-active)
                  (let ((tag (cond
                              ((string= evil-state "normal") ":")
                              ((string= evil-state "insert") ">")
                              ((string= evil-state "replace") "r")
                              ((string= evil-state "visual") "v")
                              ((string= evil-state "operator") "=")
                              ((string= evil-state "motion") "m")
                              ((string= evil-state "emacs") "Emacs")
                              ((string= evil-state "multiedit") "Multi")
                              (t "-"))))
                    (concat " " tag))))

            ;; Display buffer name
            (telephone-line-defsegment* my-buffer-segment
              `(""
                ,(telephone-line-raw mode-line-buffer-identification t)))


            ;; Display current position in a buffer
            (telephone-line-defsegment* my-position-segment
              (if (telephone-line-selected-window-active)
                  (if (eq major-mode 'paradox-menu-mode)
                      (telephone-line-trim (format-mode-line mode-line-front-space))
                    '(" %3l,%2c "))))

            ;; Ignore some buffers in modeline
            (defvar modeline-ignored-modes nil
              "List of major modes to ignore in modeline")

            (setq modeline-ignored-modes '("Dashboard"
                                           "Warnings"
                                           "Compilation"
                                           "EShell"
                                           "REPL"
                                           "Messages"))

            ;; Display modified status
            (telephone-line-defsegment* my-modified-status-segment
              (if (and (buffer-modified-p) (not (member mode-name modeline-ignored-modes)))
                  (propertize "*" 'face `(:foreground "#FFFF57"))
                ""))

            ;; Display encoding system
            (telephone-line-defsegment* my-coding-segment
              (if (telephone-line-selected-window-active)
                  (let* ((code (symbol-name buffer-file-coding-system))
                         (eol-type (coding-system-eol-type buffer-file-coding-system))
                         (eol (cond
                               ((eq 0 eol-type) "unix")
                               ((eq 1 eol-type) "dos")
                               ((eq 2 eol-type) "mac")
                               (t ""))))
                    (concat eol " "))))

            ;; Left edge
            (setq telephone-line-lhs
                  '((evil   . (my-evil-segment))
                    (nil    . (my-buffer-segment))
                    (nil    . (my-modified-status-segment))))

            ;; Right edge
            (setq telephone-line-rhs
                  '((nil     . ((telephone-line-vc-segment :active)))
                    (nil     . (telephone-line-misc-info-segment))
                    (accent  . (my-position-segment))
                    (nil     . (my-major-mode-segment))
                    (nil     . (my-coding-segment))))

            (setq telephone-line-height 20)

            (telephone-line-mode 1)))

(use-package spaceline
  :ensure t
  :disabled t
  :config (progn
          (require 'spaceline-config)

          (setq powerline-height 18)
          (setq-default powerline-default-separator 'bar)

          (spaceline-emacs-theme)

          ;; (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

          (spaceline-toggle-minor-modes-off)

          (spaceline-toggle-flycheck-error-off)
          (spaceline-toggle-flycheck-warning-off)
          (spaceline-toggle-flycheck-info-off)

          (spaceline-toggle-evil-state-on)

          (spaceline-toggle-point-position-off)
          (spaceline-toggle-version-control-off)

          (spaceline-toggle-buffer-size-off)
          (spaceline-toggle-buffer-id-on)
          (spaceline-toggle-buffer-position-on)

          (spaceline-toggle-window-number-off)

          (spaceline-toggle-hud-off)
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

;; (defadvice load-theme (before theme-dont-propagate activate)
;;   (mapc #'disable-theme custom-enabled-themes))

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
  "Set THEME with background transparency."
  (load-theme theme :no-confirm)
  (set-frame-alpha 90))

(defun mhl/load-dark-theme (theme)
  "Set THEME with background transparency. If Emacs is running in
a terminal, just try to remove default the background color."
  (load-theme theme :no-confirm)

  ;; Set transparent background.
  (if (string= system-type "gnu/linux")
      (if (string= window-system "x")
          (progn
            ;; (set-face-attribute 'default nil :background "black")
            ;; (set-face-attribute 'fringe nil :background "black")
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
          ;; (defun tao-palette () (tao-theme-golden-grayscale-yang-palette))
          (load-theme 'tao-yin 'y)
          ;; (load-theme 'tao-yang 'y)
          ;; (mhl/load-dark-theme 'tao-yin)
          ;; (mhl/load-light-theme 'tao-yang)
          ))

(use-package eziam-theme
  :ensure t
  :disabled t
  :config (progn
            (load-theme 'eziam-light-theme y)))

(use-package base16-mod-theme
  :load-path "theme/base16-mod"
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/base16-mod/"))
          (mhl/load-dark-theme 'base16-mod-dark)
          ;; (mhl/load-dark-theme 'base16-ashes-dark)
          ;; (mhl/load-light-theme 'base16-ashes-light)
          ))

(use-package apprentice-theme
  :load-path "theme/apprentice-theme"
  :disabled t
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/apprentice-theme/"))
          (mhl/load-dark-theme 'apprentice)
          ))

(use-package moe-theme
  :ensure t
  :disabled t
  :config (progn
            (moe-theme-set-color 'purple)
            (setq moe-theme-highlight-buffer-id nil)
            (moe-light)
            ))

(use-package yoshi-theme
  :ensure t
  :disabled t
  :init (progn
          ;; (add-to-list 'custom-theme-load-path (expand-file-name "~/yoshi-theme"))
          (load-theme 'yoshi :no-confirm)
          ))

(use-package zerodark-theme
  :ensure t
  :disabled t
  :config (progn
            ;; (setq zerodark-use-high-contrast-in-mode-line t)
            (zerodark-setup-modeline-format)
            (zerodark-setup-modeline-format-alt)
            ))

(use-package minimal-theme
  :load-path "theme/minimal"
  :disabled t
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/minimal/"))
          (mhl/load-dark-theme 'minimal)
          ))

(use-package essense-theme
  :load-path "theme/essense"
  :disabled t
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/essense-theme/"))
          (mhl/load-dark-theme 'essense)
          ))

(use-package leuven-mod
  :load-path "theme/leuven-mod"
  :disabled t
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/leuven-mod/"))
          ;; (mhl/load-light-theme 'leuven-mod)
          (load-theme 'leuven-mod t)
          ))

(use-package apropospriate-theme
  :ensure t
  :disabled t
  :config (progn
            ;; (mhl/load-dark-theme 'apropospriate-dark)
            (mhl/load-light-theme 'apropospriate-light)
            ))

(use-package kaolin-theme
  :load-path "theme/kaolin-theme"
  :disabled t
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/kaolin-theme/"))
          (mhl/load-dark-theme 'kaolin)
          ))

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
  (let ((my-font (font-candidate (font-spec :family "PragmataPro"
                                                             :size 12)
                                                  (font-spec :family "Inconsolatazi4"
                                                             :size 12)
                                                  "Monospace 8"
                                                  (font-spec :family "Consolas"
                                                             :size 12)
                                                  )))
    (set-frame-font my-font t t)

    (set-fontset-font t 'hangul my-font)

    ;; (set-face-font 'Info-quoted my-font)
    ;; (set-face-attribute 'Info-quoted nil :weight 'extra-bold)
    ))

;; Version Control ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :commands (magit-blame
             magit-commit
             magit-commit-popup
             magit-diff-popup
             magit-diff-unstaged
             magit-fetch-popup
             magit-init
             magit-log-popup
             magit-pull-popup
             magit-push-popup
             magit-revert
             magit-stage-file
             magit-status
             magit-unstage-file
             magit-blame-mode)
  :bind ("C-c m" . magit-status)
  :config (progn
            (if (string= system-type "windows-nt")
                (setenv "SSH_ASKPASS" "git-gui--askpass"))
            ))

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine))

(use-package git-gutter
  :disabled t
  :if (not window-system)
  :ensure t
  :defer t
  :init (progn (global-git-gutter-mode t))
  :config (progn
            (set-window-buffer nil (current-buffer))

            (setq git-gutter:modified-sign "|")
            (setq git-gutter:added-sign "|")
            (setq git-gutter:deleted-sign "|")
            ))

(use-package git-gutter-fringe
  :if (window-system)
  :ensure t
  :init (progn (global-git-gutter-mode t))
  :config (progn
            (define-fringe-bitmap 'git-gutter-fr:added
              [#b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000])
            (define-fringe-bitmap 'git-gutter-fr:deleted
              [#b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000])
            (define-fringe-bitmap 'git-gutter-fr:modified
              [#b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000
               #b0011000])))

;; Clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clipboard-setup ()
  "Do clipboard setup."
  (setq select-enable-clipboard t)
  (setq select-enable-primary t)
  (setq save-interprogram-paste-before-kill t)

  ;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  (defun xsel-cut-function (text &optional push)
    """Insert TEXT to temp-buffer and send content to xsel stdin."""
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
    ))

;; Hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :defer t
  :init (progn
          (bind-key "<f1>" (defhydra hydra-help (:color blue)
                             "Help"
                             ("a" apropos "Apropos")
                             ("b" counsel-descbinds "Describe Keybindings")
                             ("c" describe-char "Describe Char")
                             ("F" find-function "Find Function")
                             ("f" describe-function "Describe Function")
                             ("k" describe-key "Describe Key")
                             ("K" find-function-on-key "Find Key")
                             ("m" describe-mode "Describe Modes")
                             ("V" find-variable "Find Variable")
                             ("v" describe-variable "Describe Variable")))

          (bind-key "<f2>" (defhydra hydra-zoom ()
                             "Zoom"
                             ("i" text-scale-increase "in")
                             ("o" text-scale-decrease "out")
                             ("r" (lambda ()
                                    (interactive)
                                    (text-scale-set 0)) "reset")))

          ;; Hydra - Multiple cursors
          (defhydra hydra-multiple-cursors (:columns 3
                                                     :idle 1.0)
            "Multiple cursors"
            ("l" mc/edit-lines "Edit lines in region" :exit t)
            ("b" mc/edit-beginnings-of-lines "Edit beginnings of lines in region" :exit t)
            ("e" mc/edit-ends-of-lines "Edit ends of lines in region" :exit t)
            ("a" mc/mark-all-dwim "Mark all dwim" :exit t)
            ("S" mc/mark-all-symbols-like-this "Mark all symbols likes this" :exit t)
            ("w" mc/mark-all-words-like-this "Mark all words like this" :exit t)
            ("r" mc/mark-all-in-region "Mark all in region" :exit t)
            ("R" mc/mark-all-in-region-regexp "Mark all in region (regexp)" :exit t)
            ("d" mc/mark-all-like-this-in-defun "Mark all like this in defun" :exit t)
            ("s" mc/mark-all-symbols-like-this-in-defun "Mark all symbols like this in defun" :exit t)
            ("W" mc/mark-all-words-like-this-in-defun "Mark all words like this in defun" :exit t)
            ("i" mc/insert-numbers "Insert numbers" :exit t)
            ("n" mc/mark-next-like-this "Mark next like this")
            ("N" mc/skip-to-next-like-this "Skip to next like this")
            ("M-n" mc/unmark-next-like-this "Unmark next like this")
            ("p" mc/mark-previous-like-this "Mark previous like this")
            ("P" mc/skip-to-previous-like-this "Skip to previous like this")
            ("M-p" mc/unmark-previous-like-this "Unmark previous like this")
            ("q" nil "Quit" :exit t))

          (defhydra hydra-lisp-eval (:exit t
                                           :columns 2
                                           :idle 1.0)
            "Lisp eval"
            ("r" eval-region "Region")
            ("b" eval-buffer "Buffer")
            ("e" eval-expression "S-expression")
            ("l" eval-last-sexp "Last s-expression")
            ("L" eval-print-last-sexp "Last s-expression and print value")
            ("i" eval-and-replace "Eval and replace in buffer")
            ("d" eval-defun "Defun / Function")
            ("f" eval-defun "Defun / Function"))

          (defhydra hydra-yank-pop ()
            "yank"
            ("C-y" yank nil)
            ("M-y" yank-pop nil)
            ("y" (yank-pop 1) "next")
            ("Y" (yank-pop -1) "prev")
            ("l" helm-show-kill-ring "list" :color blue)))
  :config (progn
            (use-package ivy-hydra
              :ensure t
              :defer t)
            ))

;; Project Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :defer 1
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (progn
          (setq projectile-enable-caching t
                projectile-cache-file (concat user-cache-directory
                                              "projectile.cache")
                projectile-known-projects-file (concat user-cache-directory
                                                       "projectile-bookmarks.eld"))

          ;; projectile modeline updates causes some slowdown, so let's
          ;; change it to a static string.
          ;; https://github.com/bbatsov/projectile/issues/657
          (setq projectile-mode-line "Projectile"))
  :config (progn
            ;; (setq projectile-indexing-method 'native)
            (add-to-list 'projectile-globally-ignored-directories "elpa")

            (projectile-mode)
            ))

(use-package helm-projectile
  :ensure t
  :disabled t
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project)
  :init (progn
          (helm-projectile-on)

          (setq projectile-completion-system 'helm)
          (setq helm-projectile-fuzzy-match t)
          (setq projectile-switch-project-action 'helm-projectile)))

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
  :disabled t
  :defer 1
  :commands (helm-mode)
  ;; :init (progn
  ;;         (bind-key (kbd "C-x C-f") #'helm-find-files)
  ;;         (bind-key (kbd "M-x")   #'helm-M-x)
  ;;         (bind-key (kbd "C-c x") #'helm-M-x)
  ;;         (bind-key (kbd "C-c o") #'helm-imenu)
  ;;         (bind-key (kbd "C-c y") #'helm-show-kill-ring))
  :config (progn
            (setq helm-apropos-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-file-cache-fuzzy-match t
                  helm-imenu-fuzzy-match t
                  helm-lisp-fuzzy-completion t
                  helm-locate-fuzzy-match nil
                  helm-recentf-fuzzy-match t
                  helm-M-x-fuzzy-match t
                  helm-semantic-fuzzy-match t)

            ;; (helm-mode t)

            (setq-default helm-mode-line-string "")

            ;; Scroll 4 lines other window using M-<next>/M-<prior>
            (setq helm-scroll-amount 4)

            ;; Do not display invisible candidates
            (setq helm-quick-update t)

            ;; (setq helm-ff-auto-update-initial-value nil)
            ;; (setq helm-ff-smart-completion nil)

            ;; Be idle for this many seconds, before updating in delayed sources.
            (setq helm-idle-delay 0)

            ;; Be idle for this many seconds, before updating candidate buffer
            (setq helm-input-idle-delay 0.01)

            (setq helm-full-frame nil)
            (setq helm-split-window-default-side 'other)
            (setq helm-split-window-in-side-p t)         ;; open helm buffer inside current window, not occupy whole other window

            (setq helm-candidate-number-limit 1000)

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
            ))

;; Helm Additions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-descbinds
  :ensure t
  :disabled t
  :defer t
  :commands (helm-descbinds)
  :init (progn
          (fset #'describe-bindings #'helm-descbinds)
          (setq helm-descbinds-window-style 'split)
          ))

(use-package helm-imenu
  :disabled t
  :bind ("C-c o" . helm-imenu))

(use-package helm-swoop
  :ensure t
  :disabled t
  :bind (("C-c s" . helm-swoop))
  :init (progn (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map))
  :config (progn ;; disable pre-input
                 (setq helm-swoop-pre-input-function (lambda () ""))

                 (setq helm-swoop-split-with-multiple-windows nil
                       helm-swoop-split-direction 'split-window-vertically
                       helm-swoop-speed-or-color t
                       helm-swoop-split-window-function 'helm-default-display-buffer)
                 ))

;; Ido-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ido
  :ensure t
  :defer t
  :config (progn (setq ido-enable-prefix nil
                       ido-enable-flex-matching t
                       ido-create-new-buffer 'always
                       ido-use-filename-at-point nil
                       ido-max-prospects 10)

                 ;; (ido-mode t)

                 (setq ido-save-directory-list-file (concat user-cache-directory "ido.last"))

                 (add-to-list 'ido-ignore-directories "target")
                 (add-to-list 'ido-ignore-directories "node_modules")

                 ;; ;; Use ido everywhere
                 ;; (ido-everywhere t)

                 ;; ;; Display ido results vertically, rather than horizontally
                 ;; (setq ido-decorations (quote ("\n-> "
                 ;;                               ""
                 ;;                               "\n "
                 ;;                               "\n ..."
                 ;;                               "[" "]"
                 ;;                               " [No match]"
                 ;;                               " [Matched]"
                 ;;                               " [Not readable]"
                 ;;                               " [Too big]"
                 ;;                               " [Confirm]")))

                 (use-package ido-vertical-mode
                   :ensure t
                   :disabled t
                   :init (progn
                           (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
                           (ido-vertical-mode t)))

                 (use-package flx-ido
                   :ensure t
                   :disabled t
                   :init (flx-ido-mode t))
                 ))

(use-package flx
  :ensure t)

(use-package fzf
  :ensure t
  :commands (fzf fzf-directory)
  :disabled t
  :init (progn
          (general-define-key "z" #'fzf))
  :config (progn
            (setq fzf/args "-x --sort 10000 --color=16,bg+:-1,hl:4,hl+:4")

            (defadvice fzf/start (after normalize-fzf-mode-line activate)
              "Hide the modeline so FZF will render properly."
              (setq mode-line-format nil))
            ))

;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :preface (progn (setq evil-want-C-u-scroll t))
  :init (progn (setq evil-move-cursor-back nil)
               (setq evil-cross-lines t)
               (setq evil-intercept-esc 'always)

               (setq evil-want-fine-undo t)
               (setq evil-symbol-word-search t)

               ;; (setq evil-disable-insert-state-bindings t)

               (setq evil-auto-indent t)

               ;; Holy-mode (from [[https://github.com/syl20bnr/spacemacs][Spacemacs]]) for
               ;; when I want to use evil features (like evil-leader) while staying in the
               ;; emacs-state.
               (use-package holy-mode
                 :load-path "site-lisp/holy-mode"
                 :disabled t
                 :commands (holy-mode)
                 :bind ("<f12>" . holy-mode))

               ;; Hybrid-mode (from [[https://github.com/syl20bnr/spacemacs][Spacemacs]])
               (use-package hybrid-mode
                 :load-path "site-lisp/hybrid-mode"
                 :disabled t
                 :commands (hybrid-mode)
                 :init (hybrid-mode t))

               ;; (setq evil-normal-state-tag   " N ")
               ;; (setq evil-visual-state-tag   " V ")
               ;; (setq evil-emacs-state-tag    " E ")
               ;; (setq evil-insert-state-tag   " I ")
               ;; (setq evil-replace-state-tag  " R ")
               ;; (setq evil-operator-state-tag " O ")

               ;; (setq evil-emacs-state-cursor    '("#8abeb7" box))
               ;; (setq evil-normal-state-cursor   '("#e0e0e0" box))
               ;; (setq evil-insert-state-cursor   '("#f0c674" box))
               ;; (setq evil-visual-state-cursor   '("#de935f" box))
               ;; (setq evil-replace-state-cursor  '("#a3685a" box))
               ;; (setq evil-operator-state-cursor '("#81a2be" box))

               (evil-mode t))
  :config (progn (evil-set-toggle-key "C-\\")
                 ;; (bind-key "\\" 'evil-execute-in-evil-state evil-normal-state-map)
                 (bind-key "C-z" 'evil-execute-in-normal-state)

                 ;; (evil-set-initial-state 'erc-mode 'normal)
                 ;; (evil-set-initial-state 'package-menu-mode 'normal)
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

                   (bind-key "C-k" #'evil-delete-line        evil-insert-state-map)
                   (bind-key "C-k" #'evil-delete-line        evil-motion-state-map)

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

                 (define-and-bind-text-object "*" "*" "*")
                 (define-and-bind-text-object "/" "/" "/")

                 ;; Swap j,k with gj, gk
                 (bind-key "j"   #'evil-next-visual-line     evil-normal-state-map)
                 (bind-key "k"   #'evil-previous-visual-line evil-normal-state-map)
                 (bind-key "g j" #'evil-next-line            evil-normal-state-map)
                 (bind-key "g k" #'evil-previous-line        evil-normal-state-map)

                 ;; Other evil keybindings
                 (evil-define-operator evil-join-previous-line (beg end)
                   "Join the previous line with the current line."
                   :motion evil-line
                   ;; (evil-previous-visual-line)
                   ;; (evil-join beg end)
                   (join-line))

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
                   (do-not-quit))
                 (defadvice evil-quit-all (around advice-for-evil-quit-all activate)
                   (do-not-quit))

                 (use-package evil-surround
                   :ensure t
                   :defer t
                   :init (global-evil-surround-mode t))

                 (use-package evil-embrace
                   :ensure t
                   :defer t
                   :init (progn
                             (evil-embrace-enable-evil-surround-integration)
                             ))

                 (use-package evil-matchit
                   :ensure t
                   :defer t
                   :config (progn
                             (global-evil-matchit-mode t)))

                 (use-package evil-textobj-anyblock
                   :ensure t
                   :defer t
                   :init (progn
                           (bind-key "b" 'evil-textobj-anyblock-inner-block evil-inner-text-objects-map)
                           (bind-key "b" 'evil-textobj-anyblock-a-block     evil-outer-text-objects-map)
                           ))

                 (use-package evil-args
                   :ensure t
                   :defer t
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

                 (use-package evil-exchange
                   :ensure t
                   :defer t
                   :init (progn (evil-exchange-install)))

                 (use-package evil-numbers
                   :ensure t
                   :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
                   :init (progn
                           ;; Instead of C-a and C-x like in Vim, let's use + and -.
                           (bind-key "-" #'evil-numbers/dec-at-pt evil-normal-state-map)
                           (bind-key "+" #'evil-numbers/inc-at-pt evil-normal-state-map)
                           ))

                 (use-package evil-quickscope
                   :ensure t
                   :defer t
                   :disabled t
                   :init (progn (setq evil-quickscope-cross-lines t)))

                 (use-package evil-mc
                   :ensure t
                   :disabled t
                   :init (progn (global-evil-mc-mode t)))

                 ;; Make Anzu work with evil-search
                 (use-package evil-anzu
                   :ensure t)

                 (use-package evil-lion
                   :ensure t
                   :config (progn
                             (evil-lion-mode)))

                 (use-package evil-goggles
                   :ensure t
                   :disabled t
                   :config (progn
                             (evil-goggles-mode)
                             ;; optionally use diff-mode's faces; as a result, deleted text
                             ;; will be highlighed with `diff-removed` face which is typically
                             ;; some red color (as defined by the color theme)
                             ;; other faces such as `diff-added` will be used for other actions
                             (evil-goggles-use-diff-faces))
                   )))

;; Evil Additions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key "!" 'shell-command
                    "a" 'projectile-find-other-file

                    ;; Eval
                    "e" 'hydra-lisp-eval/body

                    ;; ;; Errors
                    ;; "e n" 'next-error
                    ;; "e p" 'previous-error

                    ;; Files
                    "f" 'find-file
                    ;; (evil-leader/set-key "f" #'helm-find-files)
                    "g" 'counsel-git

                    ;; Buffers
                    "b" 'buffer-menu
                    "k" 'kill-buffer
                    "u" 'switch-to-buffer
                    ;; (evil-leader/set-key "u" #'helm-buffers-list)

                    "o" 'imenu
                    "x" 'execute-extended-command
                    ;; "o" #'helm-imenu
                    ;; "x" #'helm-M-x

                    "l" 'ivy-resume

                    ;; Rings
                    "y"  'counsel-yank-pop
                    ;; "y"  #'helm-show-kill-ring
                    ;; "rm" #'helm-mark-ring
                    ;; "rr" #'helm-register

                    ;; Git
                    "m" 'magit-status

                    ;; Projectile
                    "p" 'projectile-command-map

                    ;; Swiper/Swoop
                    "s" 'swiper
                    ;; "s" #'helm-swoop

                    "j" 'dumb-jump-go

                    ;; Avy integration
                    "SPC" 'avy-goto-word-or-subword-1

                    ;; Narrowing
                    "n r" 'narrow-to-region
                    "n d" 'narrow-to-defun
                    "n p" 'narrow-to-page
                    "n w" 'widen

                    ;; Expand region
                    "v" 'er/expand-region

                    ;; Terminal
                    "t" 'open-terminal

                    ;; Help!
                    "h" 'hydra-help/body

                    ;; multiple cursors
                    "i" 'hydra-multiple-cursors/body)

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
  :defer t
  :config (progn
            (defun disable-elec-here-doc-mode ()
              (sh-electric-here-document-mode -1))

            (add-hook 'sh-mode-hook #'disable-elec-here-doc-mode)))

(use-package cc-mode
  :defer t
  :config (progn (setq-default c-default-style "bsd")
                 (setq-default c-basic-offset 4)

                 (defun c-mode-common-custom ()
                   (c-set-offset 'access-label '+)
                   ;; (c-set-offset 'inclass '++)
                   (c-set-offset 'substatement-open 0)
                   ;; (c-set-offset 'inclass 'my-c-lineup-inclass)
                   )

                 (add-hook 'c-mode-common-hook #'c-mode-common-custom)
                 ))

(use-package csharp-mode
  :ensure t
  :defer t
  :mode ("\\.cs$" . csharp-mode))

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
                   :commands (markdown-toc-generate-toc))
                 ))

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs$" . haskell-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode))

(use-package powershell
  :ensure t
  :mode ("\\.ps1" . powershell-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package php-mode
  :ensure t
  :mode ("\\.php$" . php-mode))

(use-package web-mode
  :ensure t
  :disabled t
  :mode ("\\.php$" . web-mode))

(use-package sql-mode
  :defer t
  :init (progn
            ;; Set prompt for mariadb
            (setq sql-mysql-options '("--prompt=mysql> "))
            ))

(use-package sgml-mode
  :ensure t
  :mode ("\\.html\\'" . html-mode))

(use-package ahk-mode
  :ensure t
  :mode ("\\.ahk$" . ahk-mode))

(use-package writegood-mode
  :ensure t
  :commands (writegood-mode))

;; Yasnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :defer 3
  :init (progn
          (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))

          ;; We don't want undefined variable errors
          (defvar yas-global-mode nil))
  :config (progn (yas-reload-all)
                 (add-hook 'prog-mode-hook #'yas-minor-mode)
                 (add-hook 'markdown-mode-hook #'yas-minor-mode)
                 ))

;; Auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config (progn
            (bind-key "C-n" #'company-select-next     company-active-map)
            (bind-key "C-p" #'company-select-previous company-active-map)

            (bind-key "C-<tab>" #'company-dabbrev)
            (bind-key "M-<tab>" #'company-complete)
            (bind-key "C-c C-y" #'company-yasnippet)

            (setq company-idle-delay 0
                  company-minimum-prefix-length 2
                  company-show-numbers nil
                  company-require-match 'never
                  company-selection-wrap-around t)

            (use-package irony
              :ensure t
              :defer t)

            (use-package company-irony
              :ensure t
              :defer t)

            (defun irony-mode-enable ()
              (when (member major-mode irony-supported-major-modes)
                (irony-mode t)))

            (add-hook 'c++-mode-hook  #'irony-mode-enable)
            (add-hook 'c-mode-hook    #'irony-mode-enable)
            ;; (add-hook 'objc-mode-hook #'irony-mode-enable)

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

            (setq company-backends (mapc #'company-mode/backend-with-yas company-backends))

            (global-company-mode t)
            ))

(use-package company-flx
  :ensure t
  :defer t
  :config (progn (company-flx-mode t)))

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

            (setq flycheck-perlcritic-severity 4)

            ;; Flycheck doesn't use =load-path= when checking emacs-lisp files.
            ;; Instead, it uses =flycheck-emacs-lisp-load-path=, which is empty by
            ;; default. Let's have flycheck use =load-path=!
            (setq-default flycheck-emacs-lisp-load-path 'inherit)

            ;; (flycheck-define-checker proselint
            ;;   "A linter for prose."
            ;;   :command ("proselint" source-inplace)
            ;;   :error-patterns
            ;;   ((warning line-start (file-name) ":" line ":" column ": "
            ;;             (id (one-or-more (not (any " "))))
            ;;             (message (one-or-more not-newline)
            ;;                      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            ;;             line-end))
            ;;   :modes (text-mode org-mode markdown-mode gfm-mode))
            ;; (add-to-list 'flycheck-checkers 'proselint)

            (global-flycheck-mode t)
            ))

(use-package flycheck-irony
  :ensure t
  :defer t
  :config (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package ispell
  :defer t
  :config (progn
            ;; find aspell and hunspell automatically
            (cond
             ;; try hunspell at first
             ;; if hunspell does NOT exist, use aspell
             ((executable-find "hunspell")
              (setq ispell-program-name "hunspell")
              (setq ispell-local-dictionary "en_US")
              (setq ispell-local-dictionary-alist
                    ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
                    ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
                    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
                      )))

             ((executable-find "aspell")
              (setq ispell-program-name "aspell")
              ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
              (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
            ))

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

                 ;; ;; Cooler to-do states
                 ;; (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                 ;;                           (sequence "⚑ WAITING(w)" "|")
                 ;;                           (sequence "|" "✘ CANCELED(c)")))

                 ;; Create "Table of Contents" without exporting (useful for
                 ;; github README.org files, for example)
                 (use-package toc-org
                   :ensure t
                   :config (progn
                             (add-hook 'org-mode-hook #'toc-org-enable)))

                 (use-package org-bullets
                   :ensure t
                   :disabled t
                   :init (progn (setq org-bullets-bullet-list
                                      '("◉" "○" "►" "◇")))
                   :config (progn (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))
                 ))

;; Miscellaneous Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hex color modification
(use-package kurecolor
  :ensure t
  :defer t)

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
  :config (progn (setq twittering-use-master-password t)
                 (setq twittering-icon-mode t)
                 (setq twittering-allow-insecure-server-cert t)
                 ))

;; Finishing Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load custom init file at the end.
(setq custom-file user-custom-file)
(load user-custom-file t)

;; Garbage collection of larger blocks of data can be slow, causes pauses during
;; operation. Let's reduce the size of the threshold to a smaller value (the
;; default) after most of our init is complete.
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 800000)
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

(provide 'init)
;;; init.el ends here
