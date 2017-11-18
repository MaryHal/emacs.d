
;;; Code:

;; Preload Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(defconst user-custom-file (concat user-emacs-directory "custom.el"))
(defconst user-cache-directory (concat user-emacs-directory "cache/"))
(defconst user-package-directory (concat user-emacs-directory "packages/"))

(defvar mhl/post-init-hook nil)

(defconst is-linux    (eq system-type 'gnu/linux))
(defconst is-mac      (eq system-type 'darwin))
(defconst is-windows  (eq system-type 'windows-nt))

(defconst in-x        (eq window-system 'x))

;; Properly verify outgoing ssl connections.
(setq gnutls-verify-error t
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
      nsm-settings-file (expand-file-name "network-security.data" user-cache-directory))

;; We’re going to increase the gc-cons-threshold to a very high number to
;; decrease the load time. We’re going to add a hook to reset this value after
;; initialization
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(add-hook 'mhl|post-init-hook #'(lambda () (setq gc-cons-threshold 16777216
                                                 gc-cons-percentage 0.1)))

;; Bootstrap use-package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq use-package-verbose t
      use-package-minimum-reported-time 0
      use-package-enable-imenu-support t)

(setq package-user-dir (expand-file-name user-package-directory))
(setq package-check-signature nil)

(setq load-prefer-newer noninteractive
      package--init-file-ensured t
      package-enable-at-startup nil)

(require 'package)

(when (>= emacs-major-version 24)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu"   . "https://elpa.gnu.org/packages/")))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

;; Very important, init-file-encompassing packages
(use-package general
  :ensure t
  :demand t
  :config (progn
            ;; (setq general-default-keymaps '(evil-normal-state-map
            ;;                                 evil-visual-state-map
            ;;                                 evil-operator-state-map
            ;;                                 evil-insert-state-map
            ;;                                 evil-emacs-state-map))

            ;; (setq general-default-prefix "SPC")
            ;; (setq general-default-non-normal-prefix "M-SPC")

            (general-create-definer
             leader-bind
             :prefix "SPC"
             :non-normal-prefix "M-SPC"
             :keymaps '(normal visual operator insert emacs)
            )))

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
  (if is-linux
      (call-process-shell-command "eval $TERMINAL" nil 0)
      (call-process-shell-command "start powershell.exe" nil 0)))

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

(defun output-to-minibuffer (n)
  (interactive "p")
  (let ((message-log-max nil))
    (if (use-region-p)
        (message (buffer-substring-no-properties (region-beginning) (region-end)))
      (message (buffer-substring-no-properties (line-beginning-position) (goto-char (line-end-position n)))))))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

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

(setq-default apropos-do-all t
              compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t
              confirm-nonexistent-file-or-buffer t
              idle-update-delay 2) ;; Update UI less often

;; Utf-8 please. Pretty please.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Window Rebalancing
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(use-package emacs-lock-mode
  :init (progn
            ;; Protect the scratch buffer!
            (with-current-buffer "*scratch*"
              (emacs-lock-mode 'kill))))

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
  :demand t 
  :commands (recentf-mode)
  :config (progn
            (setq recentf-save-file (concat user-cache-directory "recentf")
                  recentf-max-saved-items 100
                  recentf-max-menu-items 0)
            (recentf-mode)))

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
  :commands (winner-undo winnder-redo)
  :general ("M-N" #'winner-redo
            "M-P" #'winner-undo)
  :config (progn
            (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
            (add-hook 'mhl/post-init-hook #'winner-mode)))

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
(setq-default auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

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

;; From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html:
(setq savehist-file (concat user-cache-directory "savehist")
      history-length 500
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat user-cache-directory "saveplace"))

(savehist-mode +1)
(save-place-mode +1)

;; Helper Libraries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; String manipulation library
(use-package s
  :ensure t
  :demand t)

;; Modern list library
(use-package dash
  :ensure t
  :demand t)

(require 'cl-lib)

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
(general-define-key "C-x C-z" nil)
(general-define-key "C-z" nil)
(put 'suspend-frame 'disabled t)

;; Unset some keys I never use
(general-define-key "C-x m" nil)
(general-define-key "C-x f" nil)

(general-define-key [escape] #'isearch-abort :keymaps 'isearch-mode-map)
(general-define-key "\e"     #'isearch-abort :keymaps 'isearch-mode-map)
(general-define-key [escape] #'keyboard-escape-quit)

;; Auto-indent on RET
(general-define-key "RET" #'newline-and-indent)

;; replace with [r]eally [q]uit
(general-define-key "C-x r q" #'save-buffers-kill-terminal)
(general-define-key "C-x C-c" #'do-not-quit)

;; Alter M-w so if there's no region, just grab 'till the end of the line.
(general-define-key "M-w" #'save-region-or-current-line)

;; Join below
(general-define-key "C-j" (lambda ()
                  (interactive)
                  (join-line 1)))

;; Join above
(general-define-key "M-j" #'join-line)

;; Move windows
(windmove-default-keybindings 'meta)

;; Easier version of "C-x k" to kill buffer
(general-define-key "C-x C-b"  #'buffer-menu)
(general-define-key "C-c u"    #'switch-to-buffer)
(general-define-key "C-x k"    #'my-window-killer)
(general-define-key "C-x C-k"  #'my-window-killer)

;; Eval
(general-define-key "C-c v"    #'eval-buffer)
(general-define-key "C-c r"    #'eval-region)

(general-define-key "C-x C-e"  #'eval-and-replace)

(general-define-key "C-c C-t"  #'open-terminal)

(general-define-key "C-;"      #'comment-line-or-region)
(general-define-key "M-i"      #'back-to-indentation)

(general-define-key "C-."      #'hippie-expand)
;; (general-define-key "C-."      #'dabbrev-expand)

;; Character-targeted movements
(use-package misc
  :general ("M-z" #'zap-up-to-char))

(use-package jump-char
  :ensure t
  :general ("M-m" #'jump-char-forward
            "M-M" #'jump-char-backward))

;; (dotimes (n 10)
;;   (general-define-key (format "M-%d" n)) nil
;;   (general-define-key (format "C-%d" n))) nil

;; (general-define-key "M-9"      #'backward-sexp)
;; (general-define-key "M-0"      #'forward-sexp)

;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show keystrokes in progress
(setq echo-keystrokes 0.02)

(setq-default fill-column 80)

;; ;; Easily navigate sillyCased words
;; (global-subword-mode t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Useful frame title, that show either a file or a buffer name (if the buffer
;; isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " : " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

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

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Whitespace settings
(setq-default indent-tabs-mode nil
              require-final-newline t
              tab-always-indent t
              tab-width 4
              tabify-regexp "^\t* [ \t]+")

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(show-paren-mode)

(use-package highlight-parentheses
  :ensure t
  :disabled t
  :config (progn
            (defun hl-parens-hook ()
              (highlight-parentheses-mode t))
            (add-hook 'prog-mode-hook #'hl-parens-hook)
            ))

(use-package rainbow-delimiters
  :ensure t
  :disabled t
  :config (progn
            (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

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
  :init (progn
            ;; Always rescan buffer for imenu
            (set-default 'imenu-auto-rescan t)
            ))

(use-package avy
  :ensure t
  :commands (avy-goto-char-2 avy-goto-line avy-go-word-or-subword-1 avy-goto-char-timer)
  :config (progn
            (setq avy-keys
                  '(?c ?a ?s ?d ?e ?f ?h ?w ?y ?j ?k ?l ?n ?m ?v ?r ?u ?p)
                  avy-background t
                  avy-timeout-seconds 0.3)))

(use-package amx
  :ensure t
  :init (progn
          (setq amx-save-file (concat user-cache-directory "amx-items"))))

(use-package ivy
  :demand t
  :init (progn
          (add-hook 'mhl/post-init-hook #'ivy-mode))
  :config (progn
            (setq ivy-height 20
                  ivy-format-function 'ivy-format-function-arrow
                  ivy-count-format "%d/%d ")

            (setq projectile-completion-system 'ivy)
            (setq magit-completing-read-function 'ivy-completing-read)

            ;; (setq ivy-minibuffer-faces '(region region region region))

            (setq ivy-re-builders-alist
                  '((t . ivy--regex-fuzzy)))

            (defun ivy-insert-action (x)
              (with-ivy-window
                (insert x)))

            (ivy-set-actions
             t
             '(("I" ivy-insert-action "insert")
               ("W" kill-new "save to kill ring")))

            ;; Don't sort perp mode related lists
            (nconc ivy-sort-functions-alist
                   '((persp-kill-buffer   . nil)
                     (persp-remove-buffer . nil)
                     (persp-add-buffer    . nil)
                     (persp-switch        . nil)
                     (persp-window-switch . nil)
                     (persp-frame-switch  . nil)))
            ))

(use-package counsel
  :ensure t
  :demand t
  :general ("M-x"   #'counsel-M-x
            "C-c x" #'counsel-M-x
            "C-c o" #'counsel-imenu
            "C-c l" #'ivy-resume
            "C-c y" #'counsel-yank-pop
            "C-c s" #'swiper)
  :config (progn
            (counsel-mode t)

            (advice-add 'counsel-imenu :after #'mhl/swiper-recenter)
            (setq counsel-yank-pop-separator (concat "\n\n" (make-string 70 ?-) "\n\n"))
            ))

(use-package swiper
  :ensure t
  :commands (swiper swiper-all)
  :init (progn
            (setq swiper-faces '(region region region region))))

(use-package anzu
  :ensure t
  :config (progn
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
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word)
  :general ("C-=" #'er/expand-region))

(use-package viking-mode
  :ensure t
  :disabled t
  :config (progn
            (setq viking-use-expand-region-when-loaded t)
            (global-viking-mode)))

(use-package embrace
  :ensure t
  :defer t
  :general ("C-," #'embrace-commander))

(use-package key-chord
  :ensure t
  :disabled t
  :commands (key-chord-mode)
  :init (progn (key-chord-mode t))
  :config (progn (key-chord-define-global "VV" #'other-window)
                 (key-chord-define-global "qf" #'helm-find-files)
            ))

(use-package which-key
  :ensure t
  :demand t
  :config (progn
            (setq which-key-sort-order #'which-key-prefix-then-key-order
                  which-key-sort-uppercase-first nil
                  which-key-add-column-padding 1
                  which-key-max-display-columns nil
                  which-key-min-display-lines 5)

            (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
            (which-key-setup-side-window-bottom)

            (which-key-mode t)
            ))

(use-package multiple-cursors
  :ensure t
  :general ("C->"     #'mc/mark-next-like-this
            "C-<"     #'mc/mark-previous-like-this
            "C-c C-<" #'mc/mark-all-like-this)
  :config (progn (setq mc/list-file (concat user-cache-directory "mc-lists.el"))

                 (setq mc/unsupported-minor-modes '(company-mode
                                                    auto-complete-mode
                                                    flyspell-mode
                                                    jedi-mode))

                 ;; (general-define-key "M-<down-mouse-1>" nil)
                 ;; (general-define-key "M-<mouse-1>" #'mc/add-cursor-on-click)
                 ))

(use-package ag
  :ensure t
  :commands (ag ag-regexp))

(use-package ripgrep
  :ensure t
  :init (progn
          (use-package projectile-ripgrep
            :ensure t
            :general ("s r" #'projectile-ripgrep :keymaps 'projectile-command-map)
                      )))

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go)
  :config (progn
            (setq dumb-jump-prefer-searcher 'rg)
            (setq dumb-jump-selector 'ivy)))

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

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
          (general-define-key :keymaps 'smartparens-mode-map
                              "C-<left>"    #'sp-backward-slurp-sexp
                              "C-<right>"   #'sp-forward-slurp-sexp
                              "C-M-<left>"  #'sp-backward-barf-sexp
                              "C-M-<right>" #'sp-forward-barf-sexp)

          (add-hook 'prog-mode-hook #'smartparens-mode)
          ))

(use-package region-state
  :ensure t
  :disabled t
  :init (progn
          (region-state-mode t)))

(use-package vlf
  :disabled t
  :ensure t)

(use-package persistent-scratch
  :ensure t
  :init (progn
          (persistent-scratch-setup-default)
          (persistent-scratch-autosave-mode t)

          (let ((persistdir (concat user-cache-directory "persist")))
            (mkdir persistdir t)
            (setq persistent-scratch-backup-directory persistdir))

          ;; keep the newest 10 backups
          (setq persistent-scratch-backup-filter
                (persistent-scratch-keep-n-newest-backups 10))

          ;; keep backups not older than a month
          (setq persistent-scratch-backup-filter
                (persistent-scratch-keep-backups-not-older-than
                 (days-to-time 30)))
          ))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode +1)
  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat user-cache-directory "undo-tree-hist/")))))

;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set margins in terminal
(when (not (display-graphic-p))
  (setq-default left-margin-width 1
                right-margin-width 1))

(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (left-fringe . 0) (right-fringe . 0)
        (inhibit-double-buffering . t)))

;; Remove GUI elements
(tooltip-mode -1)
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default bidi-display-reordering nil
              blink-matching-paren nil
              cursor-in-non-selected-windows nil
              ;; remove continuation arrow on right fringe
              fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                           fringe-indicator-alist)
              highlight-nonselected-windows nil
              max-mini-window-height 0.3
              mode-line-default-help-echo nil ; disable mode-line mouseovers
              mouse-yank-at-point t           ; middle-click paste at point, not at click
              resize-mini-windows 'grow-only  ; Minibuffer resizing
              show-help-function nil          ; hide :help-echo text
              split-width-threshold 160       ; favor horizontal splits
              uniquify-buffer-name-style 'forward
              use-dialog-box nil              ; always avoid GUI
              visible-cursor nil
              x-stretch-cursor nil
              ;; defer jit font locking slightly to [try to] improve Emacs performance
              jit-lock-defer-time nil
              ;; BMACS - improve cpu usage
              jit-lock-stealth-nice 0.5
              jit-lock-stealth-time 1
              jit-lock-stealth-verbose nil
              ;; `pos-tip' defaults
              pos-tip-internal-border-width 6
              pos-tip-border-width 1
              ;; no beeping or blinking please
              ring-bell-function #'ignore
              visible-bell nil)

;; No splash screen please
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

(if is-linux
  (setq x-gtk-use-system-tooltips nil))

;; Modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-mode-line
  :ensure t
  :disabled t
  :config (progn
            (setq-default sml/line-number-format " %3l")
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

                 ;; "You don't need to activate rich-minority-mode if you're using smart-mode-line"
                 (rich-minority-mode t)
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
  (if is-linux
      (if in-x
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
          (defun tao-palette () (tao-theme-golden-grayscale-yang-palette))
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
  :disabled t
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/base16-mod/"))
          (mhl/load-dark-theme 'base16-mod-dark)
          ;; (mhl/load-dark-theme 'base16-ashes-dark)
          ;; (mhl/load-light-theme 'base16-ashes-light)
          ))

(use-package apprentice-theme
  :load-path "theme/apprentice-theme"
  :init (progn
          (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/apprentice-theme/"))
          (mhl/load-dark-theme 'apprentice)
          ))

(use-package doom-themes
  :ensure t
  :disabled t)

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
  (let ((my-font (font-candidate
                  (font-spec :family "Inziu Iosevka Slab J"
                             :size 12)
                  (font-spec :family "PragmataPro"
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
  :general ("C-c m" #'magit-status)
  :config (progn
            (if is-windows
                (setenv "SSH_ASKPASS" "git-gui--askpass"))

            (setq-default magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
            ))

(use-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(use-package gitignore-mode
  :mode "/\\.gitignore$")

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine git-timemachine-toggle))

(use-package git-gutter
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

(use-package smerge-mode
  :ensure t
  :init (progn
          (defun mhl/enable-smerge-mode-maybe ()
            "Auto-enable `smerge-mode' when merge conflict is detected."
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^<<<<<<< " nil :noerror)
                (smerge-mode 1))))

          (add-hook 'find-file-hook #'mhl/enable-smerge-mode-maybe))
  :config (progn
            (when (version< emacs-version "26")
              (defalias #'smerge-keep-upper #'smerge-keep-mine)
              (defalias #'smerge-keep-lower #'smerge-keep-other)
              (defalias #'smerge-diff-base-upper #'smerge-diff-base-mine)
              (defalias #'smerge-diff-upper-lower #'smerge-diff-mine-other)
              (defalias #'smerge-diff-base-lower #'smerge-diff-base-other))))

;; Clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      select-enable-clipboard t
      select-enable-primary t)

(after! evil
  (advice-add #'evil-visual-update-x-selection :override #'ignore))

;; Hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :demand t
  :init (progn
          (setq lv-use-separator t)

          (defhydra hydra-help (:exit t)
            "Help"
            ("a" counsel-apropos "Apropos")
            ("b" counsel-descbinds "Describe Keybindings")
            ("c" describe-char "Describe Char")
            ("F" find-function "Find Function")
            ("f" describe-function "Describe Function")
            ("k" describe-key "Describe Key")
            ("K" find-function-on-key "Find Key")
            ("m" describe-mode "Describe Modes")
            ("M" what-minor-mode "Describe Minor Modes")
            ("V" find-variable "Find Variable")
            ("v" describe-variable "Describe Variable"))

          (defhydra hydra-zoom ()
            "Zoom"
            ("i" text-scale-increase "in")
            ("o" text-scale-decrease "out")
            ("r" (lambda ()
                   (interactive)
                   (text-scale-set 0)) "reset"))

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
            ("l" counsel-yank-pop "list" :exit t))))

(use-package ivy-hydra
  :after ivy
  :commands (+ivy@coo/body ivy-dispatching-done-hydra)
  :general ("C-o" #'+ivy@coo/body
            "M-o" #'ivy-dispatching-done-hydra
            :keymaps 'ivy-minibuffer-map)
  :config
  (defhydra +ivy@coo (:hint nil :color pink)
    "
   Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
  ----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
   _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
   ^ ^ _h_ ^+^ _l_ ^ ^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
   _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
  "
    ;; arrows
    ("j" ivy-next-line)
    ("k" ivy-previous-line)
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("C-o" nil)
    ("i" nil)
    ("TAB" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ;; ("d" ivy-done :exit t)
    ("RET" ivy-done :exit t)
    ("C-m" ivy-done :exit t)
    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("w" ivy-prev-action)
    ("s" ivy-next-action)
    ("a" ivy-read-action)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("C" ivy-toggle-case-fold)
    ("o" ivy-occur :exit t)))

;; Project Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :demand t
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
  :general ("C-c p" '(:keymap projectile-command-map))
  :init (progn
          (setq projectile-enable-caching t
                projectile-indexing-method 'alien
                projectile-require-project-root nil
                projectile-cache-file (concat user-cache-directory "projectile.cache")
                projectile-known-projects-file (concat user-cache-directory
                                                       "projectile-bookmarks.eld")

                projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
                projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class"))

          ;; projectile modeline updates causes some slowdown, so let's
          ;; change it to a static string.
          ;; https://github.com/bbatsov/projectile/issues/657
          (setq projectile-mode-line "Projectile")

          (add-hook 'mhl/post-init-hook #'projectile-mode))
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories "elpa")
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

(use-package persp-mode
  :ensure t
  :demand t
  :config (progn
            (setq wg-morph-on nil
                  persp-autokill-buffer-on-remove 'kill-weak
                  persp-nil-name "nil"
                  persp-nil-hidden t
                  persp-auto-save-fname "autosave"
                  persp-auto-resume-time 1
                  persp-auto-save-opt 1
                  persp-save-dir (concat user-cache-directory "workspaces/"))))

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
  :disabled t
  :commands (fzf fzf-directory)
  :general (leader-bind "z" #'fzf)
  :config (progn
            (setq fzf/args "-x --sort 10000 --color=16,bg+:-1,hl:4,hl+:4")

            (defadvice fzf/start (after normalize-fzf-mode-line activate)
              "Hide the modeline so FZF will render properly."
              (setq mode-line-format nil))
            ))

;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :demand t
  :init (progn
          (setq evil-want-C-u-scroll t
                evil-want-visual-char-semi-exclusive t
                evil-want-Y-yank-to-eol t
                evil-magic t
                evil-echo-state t
                evil-auto-indent t
                evil-indent-convert-tabs t
                evil-ex-search-vim-style-regexp t
                evil-ex-visual-char-range t
                evil-insert-skip-empty-lines t
                evil-mode-line-format 'nil
                ;; more vim-like behavior
                evil-symbol-word-search t
                ;; don't activate mark on shift-click
                shift-select-mode nil))
  :config (progn

            (defun mhl/evil-set-cursor-colors ()
              (setq evil-emacs-state-cursor    '("#8abeb7" box))
              (setq evil-normal-state-cursor   '("#e0e0e0" box))
              (setq evil-insert-state-cursor   '("#f0c674" box))
              ;; (setq evil-visual-state-cursor   '("#de935f" box))
              (setq evil-replace-state-cursor  '("#a3685a" box))
              (setq evil-operator-state-cursor '("#81a2be" box)))

            (advice-add #'load-theme :after #'mhl/evil-set-cursor-colors)

            ;; make `try-expand-dabbrev' from `hippie-expand' work in minibuffer
            ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
            (defun minibuffer-inactive-mode-hook-setup ()
              (set-syntax-table (let* ((table (make-syntax-table)))
                                  (modify-syntax-entry ?/ "." table)
                                  table)))
            (add-hook 'minibuffer-inactive-mode-hook #'minibuffer-inactive-mode-hook-setup)

            (evil-set-toggle-key "C-\\")
            (general-define-key "C-z" 'evil-execute-in-normal-state)

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

            (general-define-key [escape] #'init/minibuffer-keyboard-quit :keymaps 'minibuffer-local-map)
            (general-define-key [escape] #'init/minibuffer-keyboard-quit :keymaps 'minibuffer-local-ns-map)
            (general-define-key [escape] #'init/minibuffer-keyboard-quit :keymaps 'minibuffer-local-completion-map)
            (general-define-key [escape] #'init/minibuffer-keyboard-quit :keymaps 'minibuffer-local-must-match-map)
            (general-define-key [escape] #'init/minibuffer-keyboard-quit :keymaps 'minibuffer-local-isearch-map)

            (defun mhl/evil-be-emacsy ()
              (general-define-key "C-a" #'evil-beginning-of-line    :keymaps '(insert motion))
              (general-define-key "C-e" #'evil-end-of-line          :keymaps '(insert motion))

              (general-define-key "C-b" #'evil-backward-char        :keymaps '(insert))
              (general-define-key "C-d" #'evil-delete-char          :keymaps '(insert))
              (general-define-key "C-f" #'evil-forward-char         :keymaps '(insert))
              (general-define-key "C-k" #'evil-delete-line          :keymaps '(insert motion))

              (general-define-key "C-p" #'evil-previous-visual-line :keymaps '(insert))
              (general-define-key "C-n" #'evil-next-visual-line     :keymaps '(insert)))

            (mhl/evil-be-emacsy)

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
            (general-define-key "j"   #'evil-next-visual-line     :keymaps '(normal))
            (general-define-key "k"   #'evil-previous-visual-line :keymaps '(normal))
            (general-define-key "g j" #'evil-next-line            :keymaps '(normal))
            (general-define-key "g k" #'evil-previous-line        :keymaps '(normal))

            ;; Other evil keybindings
            (evil-define-operator evil-join-previous-line (beg end)
              "Join the previous line with the current line."
              :motion evil-line
              ;; (evil-previous-visual-line)
              ;; (evil-join beg end)
              (join-line))

            ;; Let K match J
            (general-define-key "K" #'evil-join-previous-line :keymaps '(normal))

            (defun mhl/evil-visual-indent ()
              (interactive)
              (evil-shift-right (region-beginning) (region-end))
              (evil-normal-state)
              (evil-visual-restore))

            (defun mhl/evil-visual-dedent ()
              (interactive)
              (evil-shift-left (region-beginning) (region-end))
              (evil-normal-state)
              (evil-visual-restore))

            (general-define-key ">" #'mhl/evil-visual-indent)
            (general-define-key "<" #'mhl/evil-visual-dedent)

            ;; Commentin'
            (general-define-key "g c c" #'comment-line-or-region :keymaps '(normal))
            (general-define-key "g c"   #'comment-line-or-region :keymaps '(visual))

            ;; Don't quit!
            (defadvice evil-quit (around advice-for-evil-quit activate)
              (do-not-quit))
            (defadvice evil-quit-all (around advice-for-evil-quit-all activate)
              (do-not-quit))

            (evil-mode t)
            ))

(evil-define-operator evil-delete-char-without-register (beg end type reg)
  "delete character without yanking unless in visual mode"
  :motion evil-forward-char
  (interactive "<R><y>")
  (if (evil-visual-state-p)
    (evil-delete beg end type reg)
    (evil-delete beg end type ?_)))

(evil-define-operator evil-delete-backward-char-without-register (beg end type _)
  "delete backward character without yanking"
  :motion evil-backward-char
  (interactive "<R><y>")
  (evil-delete beg end type ?_))

(evil-define-operator evil-delete-without-register (beg end type _ _2)
  (interactive "<R><y>")
  (evil-delete beg end type ?_))

(evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
  (interactive "<R><y>")
  (let ((text (replace-regexp-in-string "\n" "" (filter-buffer-substring beg end))))
    (if (string-match-p "^\\s-*$" text)
      (evil-delete beg end type ?_)
      (evil-delete beg end type reg yank-handler))))

(evil-define-operator evil-delete-line-without-register (beg end type _ yank-handler)
    (interactive "<R><y>")
    (evil-delete-line beg end type ?_ yank-handler))

(evil-define-operator evil-change-without-register (beg end type _ yank-handler)
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler))

(evil-define-operator evil-change-line-without-register (beg end type _ yank-handler)
  "Change to end of line without yanking."
  :motion evil-end-of-line
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler #'evil-delete-line))

(evil-define-command evil-paste-after-without-register (count &optional register yank-handler)
  "evil paste before without yanking"
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste-without-register count register)
      (evil-paste-after count register yank-handler)))

(evil-define-command evil-paste-before-without-register (count &optional register yank-handler)
  "evil paste before without yanking"
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste-without-register count register)
      (evil-paste-before count register yank-handler)))

(evil-define-command evil-visual-paste-without-register (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         new-kill
         paste-eob)
    (evil-with-undo
      (let* ((kill-ring (list (current-kill 0)))
             (kill-ring-yank-pointer kill-ring))
        (when (evil-visual-state-p)
          (evil-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `evil-paste-after' because `evil-delete'
          ;; will move point to the line above
          (when (and (= evil-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (evil-delete-without-register evil-visual-beginning evil-visual-end
                       (evil-visual-type))
          (when (and (eq yank-handler #'evil-yank-line-handler)
                     (not (eq (evil-visual-type) 'line))
                     (not (= evil-visual-end (point-max))))
            (insert "\n"))
          (evil-normal-state)
          (setq new-kill (current-kill 0))
          (current-kill 1))
        (if paste-eob
            (evil-paste-after count register)
          (evil-paste-before count register)))
      (kill-new new-kill)
      ;; mark the last paste as visual-paste
      (setq evil-last-paste
            (list (nth 0 evil-last-paste)
                  (nth 1 evil-last-paste)
                  (nth 2 evil-last-paste)
                  (nth 3 evil-last-paste)
                  (nth 4 evil-last-paste)
                  t)))))

;; Evil Additions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-surround
  :ensure t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :init (global-evil-surround-mode t))

(use-package evil-embrace
  :ensure t
  :defer t
  :after evil-surround
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
  :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block)
  :init (progn
          (general-define-key "b" #'evil-textobj-anyblock-inner-block :keymaps 'evil-inner-text-objects-map)
          (general-define-key "b" #'evil-textobj-anyblock-a-block     :keymaps 'evil-outer-text-objects-map)
          ))

(use-package evil-args
  :ensure t
  :commands (evil-inner-arg
             evil-outer-arg
             evil-forward-arg
             evil-backward-arg
             evil-jump-out-args)
  :init (progn
          ;; bind evil-args text objects
          (general-define-key "a" #'evil-inner-arg :keymaps 'evil-inner-text-objects-map)
          (general-define-key "a" #'evil-outer-arg :keymaps 'evil-outer-text-objects-map)

          ;; bind evil-forward/backward-args
          (general-define-key "gl" #'evil-forward-arg  :keymaps '(normal motion))
          (general-define-key "gh" #'evil-backward-arg :keymaps '(normal motion))

          ;; bind evil-jump-out-args
          ;; (general-define-key "gm" 'evil-jump-out-args :keymaps '(normal))
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
          (general-define-key "-" #'evil-numbers/dec-at-pt :keymaps '(normal))
          (general-define-key "+" #'evil-numbers/inc-at-pt :keymaps '(normal))
          ))

;; Make Anzu work with evil-search
(use-package evil-anzu
  :when (featurep 'evil)
  :ensure t)

(use-package evil-lion
  :ensure t
  :config (progn
            (evil-lion-mode)))

(use-package evil-goggles
  :ensure t
  :config (progn
            (setq evil-goggles-duration 0.2)
            (add-hook 'mhl/post-init-hook #'evil-goggles-mode t))

            ;; optionally use diff-mode's faces; as a result, deleted text
            ;; will be highlighed with `diff-removed` face which is typically
            ;; some red color (as defined by the color theme)
            ;; other faces such as `diff-added` will be used for other actions
            (evil-goggles-use-diff-faces))

;; Leader ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leader-bind "!" #'shell-command
             "a" #'projectile-find-other-file

             ;; Eval
             "e" #'hydra-lisp-eval/body

             ;; ;; Errors
             ;; "e n" #'next-error
             ;; "e p" #'previous-error

             ;; Files
             "f" #'find-file
             "g" #'counsel-git

             ;; Buffers
             "b" #'buffer-menu
             "k" #'kill-buffer
             "u" #'switch-to-buffer

             "o" #'imenu
             "x" #'execute-extended-command

             "l" #'ivy-resume

             ;; Rings
             "y"  #'counsel-yank-pop
             "r m" #'counsel-mark-ring

             ;; Git
             "m" #'magit-status

             ;; Projectile
             "p" #'projectile-command-map

             ;; Swiper/Swoop
             "s" #'swiper
             ;; "s" #'helm-swoop

             "j" #'dumb-jump-go

             ;; Avy integration
             "SPC" #'avy-goto-word-or-subword-1

             ;; Narrowing
             "n r" #'narrow-to-region
             "n d" #'narrow-to-defun
             "n p" #'narrow-to-page
             "n w" #'widen

             ;; Expand region
             "v" #'er/expand-region

             ;; Terminal
             "t" #'open-terminal

             ;; Help!
             "h" #'hydra-help/body

             ;; multiple cursors
             "i" #'hydra-multiple-cursors/body)

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
  :config (progn
            (setq shackle-rules
                  '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
                    ;; ("^\\*magit" :regexp t :ignore t)
                    ;; ("^\\*magit[^*]+$" :regexp t :same t :noselect t)
                    ;; ("COMMIT_EDITMSG" :align t :popup t)
                    ;; ("^\\*magit.*popup\\*$"  :regexp t :align t :size 0.4)
                    ("*Backtrace*" :size 20  :align t :noselect t)
                    ("*Warnings*"  :size 8   :align t :noselect t)
                    ("*Messages*"  :size 12  :align t :noselect t)
                    ("*Help*"      :size 0.3 :align t)
                    ("^\\*.*Shell Command.*\\*$" :regexp t :align t :size 20 :noselect t)
                    (apropos-mode :size 0.3 :align t)
                    (compilation-mode :align t :size 0.4)))
            ;; (setq shackle-default-rule '(:select t))
            ))

;; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :commands (dired)
  :config (progn
            (use-package dired-details
              :ensure t
              :config (progn
                        (setq-default dired-details-hidden-string "--- ")
                        (dired-details-install)))

            (setq dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")))

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

;; Programming Languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :mode "\\.m\\(d\\|arkdown\\)$"
  :mode ("/README\\.md$" . gfm-mode)
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
                 ))

(use-package markdown-toc
  :ensure t
  :commands (markdown-toc-generate-toc))

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
  :interpreter ("lua" . lua-mode)
  :init (progn
          (add-hook 'lua-mode-hook #'flycheck-mode))
  :config (progn
            ;; sp's lua-specific rules are obnoxious, so we disable them
            (setq sp-pairs (delete (assq 'lua-mode sp-pairs) sp-pairs))))

(use-package company-lua
  :after lua-mode)

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

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode))

(use-package ahk-mode
  :ensure t
  :mode ("\\.ahk$" . ahk-mode))

(use-package writegood-mode
  :ensure t
  :commands (writegood-mode))

(use-package disaster
  :ensure t
  :commands disaster)

(use-package toml-mode
  :ensure t
  :mode "\\.toml$")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml$")

(use-package json-mode
  :ensure t
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$")

;; TODO: racer, company-racer, flycheck-rust
(use-package rust-mode
  :ensure t
  :mode "\\.rs$")

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
  :commands (company-mode global-company-mode company-complete
                          company-complete-common company-manual-begin company-grab-line)
  :config (progn
            (setq company-idle-delay 0.1
                  company-tooltip-limit 10
                  company-tooltip-align-annotations t
                  company-minimum-prefix-length 2
                  company-show-numbers nil
                  company-require-match 'never
                  company-selection-wrap-around t)

            (setq company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
                  company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
                  company-backends '(company-capf company-dabbrev-code company-keywords company-files company-dabbrev)
                  company-transformers '(company-sort-by-occurrence))

            (after! yasnippet
              (nconc company-backends '(company-yasnippet)))

            (defun mhl/company-complete ()
              "Bring up the completion popup. If only one result, complete it."
              (interactive)
              (require 'company)
              (when (and (company-manual-begin)
                         (= company-candidates-length 1))
                (company-complete-common)))

            (general-define-key "C-n" #'company-select-next     :keymaps 'company-active-map)
            (general-define-key "C-p" #'company-select-previous :keymaps 'company-active-map)

            (general-define-key "C-<tab>" #'company-dabbrev)
            (general-define-key "M-<tab>" #'mhl/company-complete)
            (general-define-key "C-c C-y" #'company-yasnippet)

            (global-company-mode t)
            ))

(use-package company-flx
  :ensure t
  :defer t
  :after company
  :config (progn (company-flx-mode t)))

(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))

;; Flycheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :init (progn
            ;; Remove newline checks, since they would trigger an immediate check
            ;; when we want the idle-change-delay to be in effect while editing.
            (setq flycheck-check-syntax-automatically '(save
                                                        idle-change
                                                        mode-enabled))

            ;; Flycheck doesn't use =load-path= when checking emacs-lisp files.
            ;; Instead, it uses =flycheck-emacs-lisp-load-path=, which is empty by
            ;; default. Let's have flycheck use =load-path=!
            (setq-default flycheck-emacs-lisp-load-path 'inherit))
  :config (progn
            (global-flycheck-mode t)
            ))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode t))

(use-package ispell
  :defer t
  :config (progn
            ;; find aspell and hunspell automatically
            (cond
             ;; try hunspell at first
             ;; if hunspell does NOT exist, use aspell
             ((executable-find "hunspell")
              (setq-default ispell-program-name "hunspell")
              (setq-default ispell-local-dictionary "en_US")
              (setq-default ispell-local-dictionary-alist
                    ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
                    ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
                    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
                      )))

             ((executable-find "aspell")
              (setq-default ispell-program-name "aspell")
              ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
              (setq-default ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

            ;; On windows, append .exe suffix
            (when (and is-windows
                       (not (s-suffix? ".exe" ispell-program-name)))
              (setq-default ispell-program-name (concat ispell-program-name ".exe")))
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

;; Finishing Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-hooks 'mhl/post-init-hook)

;; Load custom init file at the end.
(setq custom-file user-custom-file)
(load user-custom-file t)

;; Make sure emacs is daemonized.
(use-package server
  :config (progn
            ;; Suppress error "directory
            ;; ~/.emacs.d/server is unsafe"
            ;; on windows.
            (when (and (>= emacs-major-version 23)
                       (equal window-system 'w32))
              (defun server-ensure-safe-dir (dir) "Noop" t))

            (unless (server-running-p)
              (server-start))))


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
