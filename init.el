
;;; Code:

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

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



;; Package Management (use-package) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (package-initialize))

;; Bootstrap!
(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (package-initialize)
             (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

;; ;; el-get
;; (require-package 'el-get)
;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;; (el-get 'sync)

;; req-package
(require-package 'use-package)

(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :config (progn (setq paradox-execute-asynchronously t)))



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

;; (defun rotate-windows ()
;;   "Rotate your windows"
;;   (interactive)
;;   (cond ((not (> (count-windows)1))
;;          (message "You can't rotate a single window!"))
;;         (t
;;          (setq i 1)
;;          (setq numWindows (count-windows))
;;          (while (< i numWindows)
;;            (let* (
;;                   (w1 (elt (window-list) i))
;;                   (w2 (elt (window-list) (+ (% i numWindows) 1)))

;;                   (b1 (window-buffer w1))
;;                   (b2 (window-buffer w2))

;;                   (s1 (window-start w1))
;;                   (s2 (window-start w2))
;;                   )
;;              (set-window-buffer w1 b2)
;;              (set-window-buffer w2 b1)
;;              (set-window-start w1 s2)
;;              (set-window-start w2 s1)
;;              (setq i (1+ i)))))))

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
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
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
  (shell-command "eval $TERMINAL"))



;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; On Linux, if in terminal, clear the background. If GUI, set background to black and set frame
;; transparency.
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



;; Sane Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq epa-file-select-keys nil)

;; Emacs will run garbage collection after `gc-cons-threshold' bytes
;; of consing. The default value is 800,000 bytes, or ~ 0.7 MiB. By
;; increasing to 10 MiB we reduce the number of pauses due to garbage
;; collection.
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

(setq-default fill-column 100)

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

(use-package autorevert
  :config (progn (setq global-auto-revert-non-file-buffers t)
                 (setq auto-revert-verbose nil)

                 (global-auto-revert-mode t)
                 ))

(use-package simple
  :config (progn (setq shift-select-mode nil)

                 ;; ;; Show active region
                 ;; (transient-mark-mode t)
                 ;; (make-variable-buffer-local 'transient-mark-mode)
                 ;; (put 'transient-mark-mode 'permanent-local t)
                 ;; (setq-default transient-mark-mode t)

                 ;; eval-expression-print-level needs to be set to 0 (turned off) so that you can
                 ;; always see what's happening.
                 (setq eval-expression-print-level nil)
                 ))

(use-package jka-cmpr-hook
  :config (auto-compression-mode))

(use-package delsel
  :config (delete-selection-mode t))

(use-package tramp
  :defer t
  :config (setq tramp-default-method "ssh"))

(use-package recentf
  :defer t
  :config (progn (setq recentf-save-file (concat user-cache-directory "recentf"))
                 (setq recentf-max-saved-items 100)
                 (setq recentf-max-menu-items 15)
                 (recentf-mode t)
                 ))

(use-package uniquify
  :defer t
  :config (progn (setq uniquify-buffer-name-style 'forward
                       uniquify-separator "/"
                       uniquify-ignore-buffers-re "^\\*" ;; leave special buffers alone
                       uniquify-after-kill-buffer-p t)
                 ))

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



;; Other Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; String manipulation library
(use-package s
  :defer t
  :ensure t)

;; Modern list library
(use-package dash
  :defer t
  :ensure t)


;; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :commands dired
  :config (setq dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")
  )



;; Special Buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package popwin
  :ensure t
  :defer t
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
        (vertical-scroll-bars . nil)
        (left-fringe . 0) (right-fringe . 0)
        ))

;; Set font
(if (string= system-type "windows-nt")
    ;; If Windows
    (progn (add-to-list 'default-frame-alist '(font . "Consolas 10")))
  ;; If not Windows
  (progn (add-to-list 'default-frame-alist '(font . "Inconsolatazi4 10")))
  )

;; Load custom theme

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/smyx/"))
(load-theme 'smyx t)

(use-package smart-mode-line
  :ensure t
  :config (progn (setq-default sml/line-number-format " %3l")
                 (setq-default sml/col-number-format  "%2c")

                 (line-number-mode t)   ;; have line numbers and
                 (column-number-mode t) ;; column numbers in the mode line

                 (sml/setup)
                 ))

(use-package rich-minority
  :ensure t
  :config (progn (setq rm-blacklist nil)
                 (setq rm-whitelist " Wrap")
                 ;; (rich-minority-mode t)
                 ))

(use-package menu-bar
  :config (menu-bar-mode -1))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package tooltip
  :config (tooltip-mode -1))

(use-package scroll-bar
  :config (scroll-bar-mode -1))

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
(setq visible-cursor nil)

;; Smoother Scrolling
(setq scroll-margin 2
      scroll-conservatively 9999
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(use-package fringe
  :config (progn (set-fringe-mode (cons 0 0))

                 ;; Empty line indicators in the fringe
                 (setq-default indicate-empty-lines nil)
                 ))

;; Set margins to 1
(setq-default left-margin-width 1
              right-margin-width 1)
(set-window-buffer nil (current-buffer))

(use-package paren
  :config (progn (show-paren-mode t)
                 (setq show-paren-delay 0)
                 ))

(use-package highlight-parentheses
  :ensure t
  :config (progn
            (defun hl-parens-hook()
              (highlight-parentheses-mode 1))
            (add-hook 'prog-mode-hook #'hl-parens-hook)
            ))

;; (use-package elec-pair
;;   :config (electric-pair-mode t))

(use-package electric
  :config (electric-indent-mode t))

;; Trailing whitespace

(defun disable-show-trailing-whitespace()
  (setq show-trailing-whitespace nil))

(add-hook 'term-mode-hook #'disable-show-trailing-whitespace)

(setq-default show-trailing-whitespace t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c C-x" . ace-jump-mode-pop-mark))
  :init (progn
          ;; ;; Lowercase keys only please.
          ;; (setq ace-jump-mode-move-keys
          ;;       (loop for i from ?a to ?z collect i))

          ;; Only jump in this window.
          (setq ace-jump-mode-scope 'window)
          ))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :init (progn (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
               ))

(use-package anzu
  :ensure t
  :config (global-anzu-mode t))

(use-package aggressive-indent
  :disabled t
  :config (global-aggressive-indent-mode t))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  ;; :init (progn )
  )

(use-package key-chord
  :disabled t
  :init (progn (key-chord-mode 1))
  :config (progn
            (key-chord-define-global "VV" #'other-window)
            ))

(use-package guide-key
  :ensure t
  :config (progn (guide-key-mode t)
                 (setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC"))
                 (setq guide-key/recursive-key-sequence-flag t)

                 ;; Alignment and extra spacing
                 (setq guide-key/align-command-by-space-flag t)
                 ))

(use-package multiple-cursors
  :ensure t
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :preface (setq mc/list-file (concat user-cache-directory "mc-lists.el"))
  :init (progn (setq mc/unsupported-minor-modes '(company-mode
                                                  auto-complete-mode
                                                  flyspell-mode
                                                  jedi-mode))

               (global-unset-key (kbd "M-<down-mouse-1>"))
               (bind-key "M-<mouse-1>" #'mc/add-cursor-on-click)
               ))

(use-package ag
  :ensure t
  :commands (ag ag-regexp))


;; Version Control;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode t))

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine))



;; Clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; ;; If emacs is run in a terminal, the clipboard- functions have no effect. Instead, we use of xsel,
;; ;; see http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line program for getting and
;; ;; setting the contents of the X selection"
;; (unless window-system
;;   (when (getenv "DISPLAY")
;;     ;; Callback for when user cuts
;;     (defun xsel-cut-function (text &optional push)
;;       ;; Insert text to temp-buffer, and "send" content to xsel stdin
;;       (with-temp-buffer
;;         (insert text)
;;         ;; I prefer using the "clipboard" selection (the one the typically is used by c-c/c-v)
;;         ;; before the primary selection (that uses mouse-select/middle-button-click)
;;         (call-process-region (point-min) (point-max)
;;                              "xsel"
;;                              nil 0
;;                              nil "--clipboard" "--input")))
;;     ;; Callback for when user pastes
;;     (defun xsel-paste-function()
;;       ;; Find out what is current selection by xsel. If it is different from the top of the
;;       ;; kill-ring (car kill-ring), then return it. Else, nil is returned, so whatever is in the top
;;       ;; of the kill-ring will be used.
;;       (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
;;         (unless (string= (car kill-ring) xsel-output)
;;           xsel-output )))
;;     ;; Attach callbacks to hooks
;;     (setq interprogram-cut-function #'xsel-cut-function)
;;     (setq interprogram-paste-function #'xsel-paste-function)
;;     ;; Idea from http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;;     ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
;;     ))



;; Hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :init (progn
          (bind-key "<f2>" (defhydra hydra-zoom ()
                                   "zoom"
                                   ("i" text-scale-increase "in")
                                   ("o" text-scale-decrease "out")))

          (bind-key "C-z" (defhydra hydra-vi
                                  (:pre
                                   (set-cursor-color "#40e0d0")
                                   :post
                                   (progn (set-cursor-color "#ffffff")))
                                  "vi"
                                  ("l" forward-char)
                                  ("h" backward-char)
                                  ("j" next-line)
                                  ("k" previous-line)
                                  ("m" set-mark-command "mark")
                                  ("a" move-beginning-of-line "beg")
                                  ("e" move-end-of-line "end")
                                  ("d" delete-region "del" :color blue)
                                  ("y" kill-ring-save "yank" :color blue)
                                  ("q" nil "quit")
                                  ))
          ))



;; Project Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :defer 5
  :preface (progn
              (setq projectile-cache-file (concat user-cache-directory "projectile.cache"))
              (setq projectile-known-projects-file (concat user-cache-directory "projectile-bookmarks.eld")))
  :bind ("C-c a" . projectile-find-other-file)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (progn (setq projectile-enable-caching t)

                 ;; (setq projectile-indexing-method 'native)
                 (add-to-list 'projectile-globally-ignored-directories "elpa")

                 (projectile-global-mode t)
                 ))

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



;; Helm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)

         ("C-x b" . helm-buffers-list)
         ("C-c u" . helm-buffers-list)

         ("C-c y" . helm-show-kill-ring))
  :config (progn (setq-default helm-mode-line-string "")

                 ;; Scroll 4 lines other window using M-<next>/M-<prior>
                 (setq helm-scroll-amount 4)

                 ;; Do not display invisible candidates
                 (setq helm-quick-update t)

                 ;; Be idle for this many seconds, before updating in delayed sources.
                 (setq helm-idle-delay 0.01)

                 ;; Be idle for this many seconds, before updating candidate buffer
                 (setq helm-input-idle-delay 0.01)

                 (setq helm-full-frame nil)
                 ;; (setq helm-split-window-default-side 'other) ;; open helm buffer in another window
                 ;; (setq helm-split-window-in-side-p t)         ;; open helm buffer inside current window, not occupy whole other window

                 (setq helm-candidate-number-limit 200)

                 ;; show all candidates when set to 0
                 (setq helm-M-x-requires-pattern 0)

                 ;; Loop helm lists
                 (setq helm-move-to-line-cycle-in-source t)

                 ;; Free up some visual space.
                 (setq helm-display-header-line nil)

                 ;; Lots of fuzzy matching.
                 (setq helm-bookmark-show-location t)
                 (setq helm-buffers-fuzzy-matching t)
                 (setq helm-M-x-fuzzy-match t)
                 (setq helm-apropos-fuzzy-match t)
                 (setq helm-recentf-fuzzy-match t)
                 (setq helm-locate-fuzzy-match t)
                 (setq helm-file-cache-fuzzy-match t)
                 (setq helm-semantic-fuzzy-match t)
                 (setq helm-imenu-fuzzy-match t)
                 (setq helm-lisp-fuzzy-completion t)

                 ;; ;; "Remove" source header text
                 ;; (set-face-attribute 'helm-source-header nil :height 1.0)

                 ;; Save current position to mark ring when jumping to a different place
                 (add-hook 'helm-goto-line-before-hook #'helm-save-current-pos-to-mark-ring)

                 (helm-mode t)

                 (bind-key "C-z"   #'helm-select-action  helm-map)

                 ;; Tab -> do persistent action
                 (bind-key "<tab>" #'helm-execute-persistent-action helm-map)

                 ;; Make Tab work in terminal. Cannot use "bind-key" since it would detect that we
                 ;; already bound tab.
                 (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
                 ))

(use-package helm-imenu
  :bind ("C-c o" . helm-imenu))

(use-package helm-swoop
  :ensure t
  :bind ("C-c s" . helm-swoop)
  :init (progn (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)

               ;; disable pre-input
               (setq helm-swoop-pre-input-function (lambda () ""))
               ))

(use-package helm-ag
  :ensure t
  :commands (helm-ag))

(use-package helm-projectile
  :ensure t
  :config (progn (helm-projectile-on)
                 (setq projectile-completion-system 'helm)
                 ))



;; Ido-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :preface (progn (setq evil-want-C-u-scroll t)
                  (setq evil-move-cursor-back nil)
                  (setq evil-cross-lines t)
                  (setq evil-intercept-esc 'always)

                  (setq evil-auto-indent t))
  ;; :init (progn)
  :config (progn (bind-key "<f12>" #'evil-local-mode)

                 ;; Toggle evil-mode
                 (evil-set-toggle-key "C-\\")

                 ;; (evil-mode t)

                 (setq evil-emacs-state-cursor    '("lightgreen"  box))
                 (setq evil-normal-state-cursor   '("white"       box))
                 (setq evil-insert-state-cursor   '("white"       bar))

                 (setq evil-visual-state-cursor   '("orange"      hollow))
                 (setq evil-replace-state-cursor  '("red"         hollow))
                 (setq evil-operator-state-cursor '("lightblue"   box))

                 ;; ;; List of modes that should start up in Evil state.
                 ;; (defvar dotemacs-evil-state-modes
                 ;;   '(fundamental-mode
                 ;;     text-mode
                 ;;     prog-mode
                 ;;     sws-mode
                 ;;     dired-mode
                 ;;     comint-mode
                 ;;     log-edit-mode
                 ;;     compilation-mode))

                 ;; (defun my-enable-evil-mode ()
                 ;;   (if (apply 'derived-mode-p dotemacs-evil-state-modes)
                 ;;       (turn-on-evil-mode)))
                 ;; (add-hook 'after-change-major-mode-hook #'my-enable-evil-mode)

                 (evil-set-initial-state 'package-menu-mode 'normal)

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
                 (bind-key "C-a" #'evil-beginning-of-line  evil-insert-state-map)
                 (bind-key "C-a" #'evil-beginning-of-line  evil-motion-state-map)

                 (bind-key "C-b" #'evil-backward-char      evil-insert-state-map)
                 (bind-key "C-d" #'evil-delete-char        evil-insert-state-map)

                 (bind-key "C-e" #'evil-end-of-line        evil-insert-state-map)
                 (bind-key "C-e" #'evil-end-of-line        evil-motion-state-map)

                 (bind-key "C-f" #'evil-forward-char       evil-insert-state-map)

                 ;; (bind-key "C-k" #'evil-kill-line          evil-insert-state-map)
                 ;; (bind-key "C-k" #'evil-kill-line          evil-motion-state-map)

                 ;; ;; Delete forward like Emacs.
                 ;; (bind-key "C-d" #'evil-delete-char evil-insert-state-map)

                 ;; ;; Make end-of-line work in insert
                 ;; (bind-key "C-e" #'end-of-line evil-insert-state-map)

                 ;; gj gk by default
                 (bind-key "j" #'evil-next-visual-line     evil-normal-state-map)
                 (bind-key "k" #'evil-previous-visual-line evil-normal-state-map)

                 ;; Other evil keybindings
                 (evil-define-operator evil-join-previous-line (beg end)
                   "Join the previous line with the current line."
                   :motion evil-line
                   (evil-previous-visual-line)
                   (evil-join beg end))

                 ;; Let K match J
                 (bind-key "K" #'evil-join-previous-line evil-normal-state-map)

                 ;; Make Y work like D
                 (bind-key "Y" (kbd "y$") evil-normal-state-map)

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

                 ;; ;; Workgroups2
                 ;; (bind-key "g T" #'wg-switch-to-workgroup-left  evil-normal-state-map)
                 ;; (bind-key "g t" #'wg-switch-to-workgroup-right evil-normal-state-map)

                 ;; (bind-key "g t" #'wg-switch-to-workgroup-right evil-motion-state-map)

                 ;; (evil-ex-define-cmd "tabnew"   #'wg-create-workgroup)
                 ;; (evil-ex-define-cmd "tabclose" #'wg-kill-workgroup)

                 ;; ;; "Unimpaired"
                 ;; (bind-key "[ b" #'previous-buffer evil-normal-state-map)
                 ;; (bind-key "] b" #'next-buffer     evil-normal-state-map)
                 ;; (bind-key "[ q" #'previous-error  evil-normal-state-map)
                 ;; (bind-key "] q" #'next-error      evil-normal-state-map)

                 ;; Bubble Text up and down. Works with regions.
                 (bind-key "[ e" #'move-text-up   evil-normal-state-map)
                 (bind-key "] e" #'move-text-down evil-normal-state-map)

                 ;; Commentin'
                 (bind-key "g c c" #'comment-line-or-region
                           evil-normal-state-map)
                 (bind-key "g c" #'comment-line-or-region evil-visual-state-map)

                 ;; ;; Multiple cursors should use emacs state instead of insert state.
                 ;; (add-hook 'multiple-cursors-mode-enabled-hook #'evil-emacs-state)
                 ;; (add-hook 'multiple-cursors-mode-disabled-hook #'evil-normal-state)

                 ;; (define-key evil-normal-state-map (kbd "g r") 'mc/mark-all-like-this)
                 ;; (bind-key "C->" 'mc/mark-next-like-this)
                 ;; (bind-key "C-<" 'mc/mark-previous-like-this)

                 ;; Don't quit!
                 (defadvice evil-quit (around advice-for-evil-quit activate)
                   (message "Thou shall not quit!"))
                 (defadvice evil-quit-all (around advice-for-evil-quit-all activate)
                   (message "Thou shall not quit!"))

                 ;; git-timemachine integration.
                 ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
                 (eval-after-load 'git-timemachine
                   '(progn
                      (evil-make-overriding-map git-timemachine-mode-map 'normal)
                      ;; force update evil keymaps after git-timemachine-mode loaded
                      (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))
                 ))

(use-package evil-leader
  :ensure t
  :config (progn (setq evil-leader/in-all-states t
                       evil-leader/leader "SPC"
                       evil-leader/non-normal-prefix "s-")

                 (global-evil-leader-mode t)

                 (define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
                 (define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
                 (define-key evil-emacs-state-map  (kbd "M-SPC") evil-leader--default-map)

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

                 ;; Buffers
                 (evil-leader/set-key "b" #'buffer-menu)
                 (evil-leader/set-key "k" #'ido-kill-buffer)
                 (evil-leader/set-key "u" #'helm-buffers-list)

                 (evil-leader/set-key "o" #'helm-imenu)
                 (evil-leader/set-key "x" #'helm-M-x)

                 ;; Kill ring
                 (evil-leader/set-key "y" #'helm-show-kill-ring)

                 ;; Git
                 (evil-leader/set-key "m" #'magit-status)

                 ;; Projectile
                 (evil-leader/set-key "p" #'projectile-command-map)

                 ;; Swoop
                 (evil-leader/set-key "s" #'helm-swoop)

                 ;; Ace-jump-mode (has evil-integration built in!)
                 (evil-leader/set-key "SPC" #'ace-jump-word-mode)
                 (evil-leader/set-key "l"   #'helm-locate)

                 ;; Expand region
                 (evil-leader/set-key "v" #'er/expand-region)

                 ;; Terminal
                 (evil-leader/set-key "t" #'open-terminal)

                 ;; Help!
                 (evil-leader/set-key
                   "hc" #'describe-char
                   "hf" #'describe-function
                   "hk" #'describe-key
                   "hl" #'describe-package
                   "hm" #'describe-mode
                   "hp" #'describe-personal-keybindings
                   "hv" #'describe-variable)
                 ))

(use-package evil-surround
  :disabled t
  :config (global-evil-surround-mode t))

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



;; Language Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package js2-mode
  :disabled t
  :mode ("\\.js$" . js2-mode)
  :config (js2-highlight-level 3))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package sgml-mode
  :ensure t
  :mode ("\\.html\\'" . html-mode))



;; Yasnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  ;; :commands (yas-expand yas-minor-mode)
  :init (progn (setq yas-snippet-dirs (concat user-emacs-directory "snippets")))
  :config (progn ;; (yas-load-directory (concat user-emacs-directory "snippets"))
                 (yas-reload-all)
                 (add-hook 'prog-mode-hook #'yas-minor-mode)
                 (add-hook 'markdown-mode-hook #'yas-minor-mode)
                 ))



;; Auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package irony
  :ensure t)

(use-package company-irony
  :ensure t)

(use-package company
  :ensure t
  :init (progn (bind-key "C-n" #'company-select-next     company-active-map)
               (bind-key "C-p" #'company-select-previous company-active-map)
               )
  :config (progn (setq-default company-idle-delay 0)
                 (setq-default company-minimum-prefix-length 1)
                 ;; (setq-default company-show-numbers t)

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
                 ;; (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

                 (setq-default company-backends (quote (company-files
                                                        company-irony
                                                        company-elisp
                                                        company-yasnippet
                                                        ;; company-css
                                                        ;; company-eclim
                                                        ;; company-clang
                                                        ;; company-capf
                                                        ;; (company-dabbrev-code company-keywords)
                                                        company-keywords
                                                        ;; company-dabbrev
                                                        )))

                 ;; (optional) adds CC special commands to `company-begin-commands' in order to
                 ;; trigger completion at interesting places, such as after scope operator
                 ;; std::|
                 (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)

                 (global-company-mode t)
                 ))



;; Flycheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :disabled t
  :init (progn
          ;; Remove newline checks, since they would trigger an immediate check
          ;; when we want the idle-change-delay to be in effect while editing.
          (setq flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))
          ))



;; Org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :defer t
  :config (progn (setq org-replace-disputed-keys t)

                 ;; Fontify org-mode code blocks
                 (setq org-src-fontify-natively t)
                 ))



;; Other Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package erc
  :defer t
  :config (progn (setq-default erc-nick "MaryHadALittle")))

(use-package sublimity
  :load-path "site-lisp/sublimity"
  :disabled t
  :defer t
  :init (progn (defun load-minimap-package ()
                   (interactive)
                   (require 'sublimity)
                   (require 'sublimity-map)
                   (sublimity-mode 1)
                   (sublimity-map-set-delay nil)
                   (setq sublimity-map-size 30)
                   (setq sublimity-map-fraction 0.3)
                   )
                 ))



;; Extra Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove suspend-frame. Three times.
(global-unset-key (kbd "C-x C-z"))
;; (global-unset-key (kbd "C-z"))
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

;; Character-targeted movements
(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package jump-char
  :ensure t
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))



;; Finishing Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server
  :config (unless (server-running-p)
            (server-start)))

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
