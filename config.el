;; Sane defaults

;; Emacs will run garbage collection after `gc-cons-threshold' bytes of consing.
;; The default value is 800,000 bytes, or ~ 0.7 MiB.
;; By increasing to 10 MiB we reduce the number of pauses due to garbage collection.
(setq gc-cons-threshold (* 10 1024 1024))

;; UTF-8 please
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Fringe and window margins
(set-fringe-mode 0)

;; batch mode

(req-package batch-mode
  :init (add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode)))

;; completion with clang

(req-package auto-complete-clang
  :require
  (auto-complete cc-mode)
  :init
  (progn (add-hook 'c++-mode-hook 'cc-mode-clang-hook)
         (add-hook 'c-mode-hook 'cc-mode-clang-hook)
         (setq-default c-basic-offset 4)
         (setq-default c-default-style "bsd")))

;; headers completion

(req-package auto-complete-c-headers
  :require
  auto-complete-clang)

;; detect mode for .h file

(req-package dummy-h-mode
  :require
  cc-mode
  :init
  (progn (add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
         (add-hook 'dummy-h-mode-hook
                   (lambda ()
                     (setq dummy-h-mode-default-major-mode 'c++-mode)))
         (add-hook 'dummy-h-mode-hook
                   (lambda ()
                     (setq dummy-h-mode-search-limit 60000)))))

;; gdb

(req-package gdb-mi
  :require
  cc-mode
  :init
  (progn (setq gdb-many-windows t)
         (setq gdb-show-main t)))

;; snippets using helm

;; (req-package helm-c-yasnippet
;;   :require
;;   (helm yasnippet cc-mode auto-complete auto-complete-clang)
;;   :init
;;   (define-key global-map (kbd "C-M-y") 'helm-c-yas-complete))

;; rtags

;; (req-package rtags)

;; some utils

(defun find-makefile-dir (cur)
  (if (file-exists-p (concat cur "Makefile"))
      cur
    (if (string-equal (expand-file-name cur) "/")
        nil
      (find-makefile-dir (expand-file-name (concat cur "../"))))))

(defun expand-include-flag (a)
  (if (string-prefix-p "-I" a)
      (concat "-I" (expand-file-name (concat (find-makefile-dir "./") (substring a 2))))
    a))

(defun cc-mode-clang-hook ()
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-clang)
  (add-to-list 'ac-sources 'ac-source-c-headers)

  (setq cc-search-directories (split-string (shell-command-to-string "bash ~/.emacs.d/clang-include-paths.sh")))

  (setq ac-clang-flags
        (mapcar (lambda (item) (concat "-I" item))
                (split-string (shell-command-to-string "bash ~/.emacs.d/clang-include-paths.sh"))))

  (setq ac-clang-flags (append ac-clang-flags
                               (mapcar 'expand-include-flag
                                       (split-string (shell-command-to-string (concat (concat "make -C " (find-makefile-dir "./")) " -s print-cflags")))))))

;; Mac support

(if (eq system-type 'darwin)
	(progn   (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

			 (req-package exec-path-from-shell
               :init
               (exec-path-from-shell-initialize))))

;; single dired

(req-package dired-single
  :require
  (dired helm-swoop autorevert)
  :init
  (progn (define-key dired-mode-map (kbd "f")
           'dired-single-buffer)

         (define-key dired-mode-map (kbd "<RET>")
           'dired-single-buffer)

         (define-key dired-mode-map (kbd "^")
           (function (lambda ()
                       (interactive)
                       (dired-single-buffer ".."))))

         (define-key dired-mode-map (kbd "M-i")
           'helm-swoop)

         (add-hook 'dired-mode-hook (lambda ()
                                      (auto-revert-mode 1)))))

;; sunrise commander

(req-package sunrise-commander
  :require
  (dired dired-single))

;; dired rainbow

(req-package dired-rainbow
  :require
  dired)

;; dired open

(req-package dired-open
  :require
  dired)

;; Undo Tree

(req-package undo-tree
  :diminish undo-tree-mode)

;; Evil Mode

(setq evil-want-C-u-scroll t)
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil)
(setq evil-cross-lines t)
(setq evil-intercept-esc 'always)

(setq evil-auto-indent t)

(req-package evil
  :require undo-tree
  :init
  (progn (evil-mode t)
         (evil-set-toggle-key "C-\\")

         ;; Stop evil from overwriting cursor color
         (setq evil-default-cursor t)
         ;; (setq evil-insert-state-cursor '("#aa0000" hbar))

         ;; Make end-of-line work in insert
         (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

         ;; Redefine ESC for Evil (By default it's meta)
         ;; (define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
         ;; (define-key evil-visual-state-map (kbd "ESC") 'evil-normal-state)
         ;; (define-key evil-replace-state-map (kbd "ESC") 'evil-normal-state)
         ;; (define-key evil-operator-state-map (kbd "ESC") 'evil-normal-state)
         ;; (define-key evil-motion-state-map (kbd "ESC") 'evil-normal-state)

         ;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
         ;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
         ;; (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
         ;; (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
         ;; (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
         ;; (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
         ;; (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
         
         ;; Other evil keybindings
         (evil-define-operator evil-join-previous-line (beg end)
           "Join the previous line with the current line."
           :motion evil-line
           (evil-previous-visual-line)
           (evil-join beg end))

         ;; Package list: don't need to switch to evil mode if I have these two keys!
         (define-key package-menu-mode-map "j" 'next-line)
         (define-key package-menu-mode-map "k" 'previous-line)

         (define-key evil-normal-state-map (kbd "z z") (lambda ()
                                                         (interactive)
                                                         (evil-scroll-line-to-center (line-number-at-pos))))

         ;; gj gk by default
         (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
         (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

         ;; Let K match J
         (define-key evil-normal-state-map (kbd "K") 'evil-join-previous-line)

         ;; Make Y work like D
         (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

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
         ))


;; evil-leader
(setq evil-leader/in-all-states t
      evil-leader/leader "SPC"
      evil-leader/non-normal-prefix "s-")

(req-package evil-leader
  :init (progn
          (global-evil-leader-mode)

          (evil-leader/set-key "eb" 'eval-buffer)
          (evil-leader/set-key "er" 'eval-region)

          (evil-leader/set-key "ff" 'ido-find-file)
          (evil-leader/set-key "fd" 'ido-dired)
          (evil-leader/set-key "fs" (lambda()
                                      (interactive)
                                      (split-window-below)
                                      (evil-window-down 1)
                                      (ido-find-file)))
          (evil-leader/set-key "fv" (lambda()
                                      (interactive)
                                      (split-window-right)
                                      (evil-window-right 1)
                                      (ido-find-file)))

          ;; Terminal
          (evil-leader/set-key "t"  '(lambda()
                                       (interactive)
                                       (shell-command "urxvtc")))
          ))

;; Projectile
(req-package projectile
  :require evil-leader
  :init (progn
          (setq projectile-enable-caching t)

          (defvar projectile-cache-file (concat user-emacs-directory "cache/projectile.cache"))
          (defvar projectile-known-projects-file (concat user-emacs-directory "cache/projectile-bookmarks.eld"))

          (setq projectile-indexing-method 'native)
          (add-to-list 'projectile-globally-ignored-directories "elpa")
          (add-to-list 'projectile-globally-ignored-directories ".cache")

          (projectile-global-mode t)

          (evil-leader/set-key "p" 'projectile-find-file)
          )) 

;; save package list

(req-package save-packages
  :init
  (progn (setq save-packages-file "~/.emacs.d/saved-packages")
         (add-hook 'kill-emacs-hook 'save-packages)))

;; swoop

;; (req-package swoop)

;; sudo support

(req-package sudo-ext)

;; shell command

(req-package shell-command)

;; indentation

(setq-default tab-width 4)
(add-hook 'find-file-hook (lambda () (setq indent-tabs-mode nil)))
(electric-indent-mode 1)

;; mark ring tweaks

(setq set-mark-command-repeat-pop t)

;; save bookmarks on emacs exit

;; (setq bookmark-save-flag 1)

;; Disable backup
;; (setq backup-inhibited t)

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
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; do not use dialog boxes

(setq use-dialog-box nil)

;; enable upcase and downcase region commands

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable narrowing

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; default mode for text editing

(setq-default major-mode 'text-mode)

;; disable defining variables in files

(setq enable-local-variables nil)

;; ace jump mode

(req-package ace-jump-mode
  :init
  (progn (define-key global-map (kbd "M-/") 'ace-jump-word-mode)
         (define-key global-map (kbd "s-c") 'ace-jump-char-mode)
         (define-key global-map (kbd "M-g M-g") 'ace-jump-line-mode)))

;; ace jump buffer

(req-package ace-jump-buffer
  :require (shell ace-jump-mode)
  :init (progn (define-key shell-mode-map (kbd "M-?") 'ace-jump-buffer)
               (define-key global-map (kbd "M-?") 'ace-jump-buffer)))

;; move text

(req-package move-text
  :init (progn (global-set-key (kbd "M-n") 'move-text-down)
               (global-set-key (kbd "M-p") 'move-text-up)))

;; auto pair

;; (req-package autopair
;;   :init (add-hook 'prog-mode-hook (lambda () (autopair-mode 1))))

;; auto complete

(req-package auto-complete
  :init (progn (require 'auto-complete-config)
               (global-auto-complete-mode t)
               (setq ac-auto-start t)
               (setq ac-quick-help-delay 0.1)
               (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

               ;; Fuzzy matching
               (setq ac-use-fuzzy t)
               ;; Set history file location
               (setq ac-comphist-file (expand-file-name (concat user-emacs-directory "cache/ac-comphist.dat")))
               ))

;; yasnippet

(req-package yasnippet
  :init (yas-global-mode 1))

;; glsl

(req-package glsl-mode
  :init (progn (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
               (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
               (add-to-list 'auto-mode-alist '("\\.gs\\'" . glsl-mode))
               (setq glsl-other-file-alist '(("\\.fs$" (".vs"))
                                             ("\\.vs$" (".fs"))))))

;; helm

(req-package helm
  :require evil-leader
  :init
  (progn (require 'helm-config)

         ;; Helm keybindings
         (define-key helm-map (kbd "C-k") 'helm-previous-line)
         (define-key helm-map (kbd "C-j") 'helm-next-line)
         (define-key helm-map (kbd "C-h") 'helm-previous-source)
         (define-key helm-map (kbd "C-l") 'helm-next-source)

         ;; Helm evil-leader keys
         (evil-leader/set-key "k" 'helm-show-kill-ring)
         (evil-leader/set-key "o" 'helm-imenu)
         (evil-leader/set-key "r" 'helm-register)
         (evil-leader/set-key "u" 'helm-buffers-list)
         (evil-leader/set-key "x" 'helm-M-x)
         ))

;; helm ac

(req-package ac-helm
  :require
  (helm auto-complete)
  :init
  (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm))

;; helm helm commands

(req-package helm-helm-commands
  :require helm)

;; helm swoop

(req-package helm-swoop
  :require
  helm
  :init
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch))

;; helm desc binds

(req-package helm-descbinds
  :require
  helm)

;; helm package

(req-package helm-package
  :require
  helm)

;; helm make

(req-package helm-make
  :require
  helm
  :init
  (global-set-key (kbd "s-B") 'helm-make))

;; helm wgrep

(req-package wgrep-helm
  :require
  (helm wgrep grep))

;; Ido Mode

(req-package ido
  :init
  (progn
    (ido-mode t)
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-max-prospects 10)

    (setq ido-save-directory-list-file (concat user-emacs-directory "cache/ido.last"))

    (defun my-ido-define-keys()
      (define-key ido-file-completion-map (kbd "C-u") 'ido-delete-backward-updir)
      (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
      (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
        )

    (add-hook 'ido-setup-hook 'my-ido-define-keys)

    ;; Always rescan buffer for imenu
    (set-default 'imenu-auto-rescan t)

    (add-to-list 'ido-ignore-directories "target")
    (add-to-list 'ido-ignore-directories "node_modules")
    ))

(req-package ido-ubiquitous
  :require ido
  :config
  (progn
    (ido-ubiquitous-mode t)

    ;; Fix ido-ubiquitous for newer packages
    (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
      `(eval-after-load ,package
         '(defadvice ,cmd (around ido-ubiquitous-new activate)
            (let ((ido-ubiquitous-enable-compatibility nil))
              ad-do-it))))

    (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
    ;;(ido-ubiquitous-use-new-completing-read webjump 'webjump)
    ;;(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)
    ))

(req-package flx-ido
  :require (flx ido)
  :init
  (progn
    (flx-ido-mode t)

    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)

    ;; Disable flx highlights
    ;; (setq flx-ido-use-faces nil)
    ))

(req-package smex
  :require ido
  :bind
  (("M-x" . smex)
   ("C-x C-m" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)
   )
  :init
  (progn
    (smex-initialize)

    (setq smex-key-advice-ignore-menu-bar t)
    (setq smex-save-file (concat user-emacs-directory "cache/smex-items"))
    ))

;; js2 mode

(req-package js2-mode
  :init (progn
          (setq js2-highlight-level 3)
          ;; (setq-default js2-basic-offset 2)

          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
          ))

;; json reformatter

(req-package json-reformat)

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
  :init
  (progn (setq sml/theme 'respectfull)
         (setq sml/shorten-modes t)
         (setq sml/shorten-directory t)
         (setq sml/name-width 20)
         (setq sml/mode-width 'full)
         (add-to-list 'sml/hidden-modes " Anzu")
         (add-to-list 'sml/hidden-modes " AC")
         (add-to-list 'sml/hidden-modes " yas")
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

;; (req-package hl-sexp
;;   :require
;;   hl-line
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (hl-sexp-mode 1))))

;; diff highlight

;; (req-package diff-hl
;;   :require
;;   (smartrep fringe)
;;   :init
;;   (global-diff-hl-mode 1))

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

;; (req-package base16-theme
;;   :init
;;   (progn
;;     ;; Transparency after load theme...
;;     ;; On Linux, if in terminal, clear the background. If GUI, set background to black and set
;;     ;; frame transparency.
;;     (defadvice load-theme (after load-theme activate compile)
;;       (if (string= system-type "gnu/linux")
;;           (if (string= window-system "x")
;;               (progn (set-frame-parameter (selected-frame) 'alpha '(90 90))
;;                      (add-to-list 'default-frame-alist '(alpha 90 90))
;;                      (set-face-attribute 'default nil :background "black"))
;;             (progn (when (getenv "DISPLAY")
;;                      (set-face-attribute 'default nil :background "unspecified-bg")
;;                      ))
;;             )))

;;     (load-theme 'base16-default t)
;;     ))

;; (req-package base16-theme)

(req-package hemisu-theme
  :init
  (progn
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

    (load-theme 'hemisu-dark t)
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

;; Org Mode

(req-package org
  :init
  (progn (global-set-key "\C-cl" 'org-store-link)
         ;; (global-set-key "\C-cc" 'org-capture)
         (global-set-key "\C-ca" 'org-agenda)
         (global-set-key "\C-cb" 'org-iswitchb)))

;; pkgbuild mode

(req-package pkgbuild-mode
  :mode
  "\\PKGBUILD\\'")

;; use igrep

(req-package igrep)

;; use wgrep

(req-package wgrep
  :require
  grep)

;; magit

(req-package magit)

;; helm ls git

(req-package helm-ls-git
  :require
  helm
  :init (global-set-key (kbd "M-+") 'helm-ls-git-ls))

;; git config mode

(req-package gitconfig-mode)

;; git ignore mode

(req-package gitignore-mode)

;; Markdown-mode

(req-package markdown-mode
  :init (progn
          (defun my-markdown-mode-hook()
            (setq markdown-imenu-generic-expression
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

;; Java + Eclim

(req-package emacs-eclim
             :init (progn
                     (require 'eclim)
                     (global-eclim-mode)

                     (require 'ac-emacs-eclim-source)
                     (ac-emacs-eclim-config)
                     ))

;; More Keybindings

;; Easier version of "C-x k" to kill buffer
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Evaluate Buffer
(global-set-key (kbd "C-c C-v") 'eval-buffer)
(global-set-key (kbd "C-c C-r") 'eval-region)

;; Commentin'
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
