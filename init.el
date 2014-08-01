(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-list '(ag
                     anzu
                     company
                     company-irony
                     diminish
                     evil-leader
                     evil
                     flx
                     flx-ido
                     hemisu-theme
                     highlight-parentheses
                     ido-ubiquitous
                     irony
                     json
                     js2-mode
                     lua-mode
                     markdown-mode
                     projectile
                     smex
                     undo-tree
                     workgroups2))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; package-initialize normally gets executed twice, stop it for slightly faster startup
(setq package-enable-at-startup nil)

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

(loadCustomFile)



;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-loadpath (&rest dirs)
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))



;; Sane Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs will run garbage collection after `gc-cons-threshold' bytes of consing.
;; The default value is 800,000 bytes, or ~ 0.7 MiB.
;; By increasing to 10 MiB we reduce the number of pauses due to garbage collection.
(setq gc-cons-threshold (* 10 1024 1024))

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
;; (setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Show active region
(transient-mark-mode t)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode t)

;; Always display line and column numbers
;; (setq line-number-mode t)
;; (setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode t)

;; Move .recentf location
(setq recentf-save-file (concat user-emacs-directory "cache/recentf"))
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 50)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode t)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode t)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Useful frame title, that show either a file or a buffer name (if the buffer isn't visiting a file)
;; (setq frame-title-format
;;       '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
;;                                                     (abbreviate-file-name (buffer-file-name))
;;                                                   "%b"))))

;; Represent undo-history as an actual tree (visualize with C-x u)
;; (setq undo-tree-mode-lighter "")
;; (require 'undo-tree)
;; (global-undo-tree-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Nic says eval-expression-print-level needs to be set to 0 (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Mouse support
;; (require 'mouse)
;; (xterm-mouse-mode t)
;; (defun track-mouse (e))
;; (setq mouse-sel-mode t)

;; Seed the random number generator
(random t)



;; Backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq create-lockfiles nil)



;; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)

;; Dired uses human readable sizes.
;;(setq dired-listing-switches "-alh")
(setq dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")



;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Ignore wiki packages during package-list-packages
(defadvice package--add-to-archive-contents
  (around package-filter-wiki-packages (package archive) activate compile)
  (unless (string-match-p "\\[wiki\\]$" (package-desc-doc (cdr package)))
    ad-do-it))

;; Transparency after load theme...
;; On Linux, if in terminal, clear the background. If GUI, set background to black and set
;; frame transparency.
(defadvice load-theme (after load-theme activate compile)
  (if (string= system-type "gnu/linux")
      (if (string= window-system "x")
          (progn (set-frame-parameter (selected-frame) 'alpha '(80 80))
                 (add-to-list 'default-frame-alist '(alpha 80 80))
                 (set-face-attribute 'default nil :background "black"))
        (progn (when (getenv "DISPLAY")
                   (set-face-attribute 'default nil :background "unspecified-bg")
                   ))
        )))

;; Window Rebalancing
(setq split-height-threshold nil)
(setq split-width-threshold 0)

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

;; Don't kill scratch buffer, just bury it.
(defadvice kill-buffer (around my-advice-for-kill-buffer activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*Scratch*")
        (bury-buffer)
      ad-do-it)))



;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-cursor-color "#CCCCCC")
(set-mouse-color "#CCCCCC")

;; Default window metrics
(setq default-frame-alist
      '((top   . 10) (left   . 2)
        (width . 80) (height . 30)
        (mouse-color  . "#CCCCCC")
        ;; (cursor-color . "#CCCCCC")
        ))

;; Load custom theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/theme/hemisu-theme"))
(add-to-list 'load-path (concat user-emacs-directory "/theme/hemisu-theme"))
(load-theme 'hemisu-dark t)

;; Set font
(if (string= system-type "windows-nt")
    ;; If Windows
    (progn (setq myFrameFont "Consolas 10")
           (add-to-list 'default-frame-alist '(font . "Consolas 10")))
  ;; If not Windows
  (progn (setq myFrameFont "Inconsolata 10")
         (add-to-list 'default-frame-alist '(font . "Inconsolata 10")))
  )

;; Diminish modeline clutter
(require 'diminish)

(eval-after-load "anzu"
  '(diminish 'anzu-mode))
(eval-after-load "company"
  '(diminish 'company-mode))
(eval-after-load "highlight-parentheses"
  '(diminish 'highlight-parentheses-mode))
(eval-after-load "projectile"
  '(diminish 'projectile-mode))
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(setq-default mode-line-format
              (list
               " %3l   "

               ;; was this buffer modified since the last save?
               ;; '(:eval (propertize  ("*")
               ;;                'face (if (buffer-modified-p)
               ;;                         mode-line-urgent
               ;;                       mode-line-highlight)))

               '(:eval (propertize "*"
                                   'face (if (buffer-modified-p)
                                             'mode-line-urgent
                                           'mode-line-highlight)
                                   'help-echo "Buffer has been modified"))

               ;; the buffer name; the file name as a tool tip
               " "
               '"%b" 'face nil
               " "

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (propertize "[RO]"
                                     'face nil
                                     'help-echo "Buffer is read-only")))

               ;; The current major mode for the buffer.
               "                    "
               `(:propertize  ("# ") face mode-line-highlight)
               '"%m"
               `(:propertize  (" #") face mode-line-highlight)

               ;; global-mode-string
               ;; (propertize "%M" 'face nil)

               ;; List minor modes
               ;; I hide so many of them, should I even have this here?
               minor-mode-alist
               ))


;; Toolbars and such
;; (add-hook 'before-make-frame-hook 'turn-off-tool-bar)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

;; No splash screen please
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(line-number-mode t)   ;; have line numbers and
(column-number-mode t) ;; column numbers in the mode line

;; No Tabs, just spaces
(setq-default indent-tabs-mode nil)

;; Don't add newlines when cursor goes past end of file
(setq next-line-add-newlines nil)
(setq require-final-newline nil)

;; Don't Blink Cursor
(blink-cursor-mode -1)

;; Fringe and window margins
(set-fringe-mode 0)

;; Set margins to 1 if not in terminal
(when (display-graphic-p)
  (setq-default left-margin-width 1 right-margin-width 1)
         (set-window-buffer nil (current-buffer)))

(setq visible-bell nil
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#dd2222")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

(defun hl-parens-hook()
  (highlight-parentheses-mode 1))
(add-hook 'prog-mode-hook 'hl-parens-hook)

;; Whitespace-style
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Anzu
(require 'anzu)
(global-anzu-mode 1)



;; Ido-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(setq ido-save-directory-list-file (concat user-emacs-directory "cache/ido.last"))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Use ido everywhere
(ido-everywhere 1)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

;;(ido-ubiquitous-use-new-completing-read webjump 'webjump)
;;(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;;(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; Better matching
(require 'flx-ido)
(flx-ido-mode t)

;; Disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Disable flx highlights
;; (setq flx-ido-use-faces nil)

(require 'smex)
(smex-initialize)

(setq smex-key-advice-ignore-menu-bar t)
(setq smex-save-file (concat user-emacs-directory "cache/smex-items"))



;; Projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq projectile-enable-caching t)

(defvar projectile-cache-file (concat user-emacs-directory "cache/projectile.cache"))
(defvar projectile-known-projects-file (concat user-emacs-directory "cache/projectile-bookmarks.eld"))

(require 'projectile)

;; (setq projectile-indexing-method 'native)

(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")

(projectile-global-mode t)



;; Workgroups2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'workgroups2)

;; Change workgroups session file
(setq wg-default-session-file (concat user-emacs-directory "cache/workgroups2"))
(setq wg-use-default-session-file nil)

;; Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-c z"))

(workgroups-mode 1)



;; Language Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs-Lisp Hooks


;; C Mode Hooks
(defun c-mode-common-custom ()
  (setq c-default-style "bsd")
  (setq c-basic-offset 4)
  (c-set-offset 'access-label '-)
  (c-set-offset 'inclass '++)
  ;; (c-set-offset 'inclass 'my-c-lineup-inclass)
  )

(add-hook 'c-mode-common-hook 'c-mode-common-custom)

;; Markdown Mode Hooks
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

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))

;; Javascript
(require 'js2-mode)
(setq js2-highlight-level 3)
;; (setq-default js2-basic-offset 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)



;; Auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Irony-mode
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

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(setq-default company-backends (quote (company-files
                                       company-elisp
                                       company-css
                                       company-eclim
                                       company-clang
                                       company-capf
                                       (company-dabbrev-code company-keywords)
                                       company-dabbrev)))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Complete immediately
(setq-default company-idle-delay 0)
(setq-default company-minimum-prefix-length 1)
;; (setq-default company-show-numbers t)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)



;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil tag colors
;; (setq evil-normal-state-tag   (propertize " Normal "   'face '((:background "LimeGreen" :foreground "DarkGreen" :weight bold)))
;;       evil-insert-state-tag   (propertize " Insert "   'face '((:background "grey80" :foreground "NavyBlue" :weight bold)))
;;       evil-visual-state-tag   (propertize " Visual "   'face '((:background "DarkOrange" :foreground "Red4" :weight bold)))
;;       evil-replace-state-tag  (propertize " Replace "  'face '((:background "red3" :foreground "grey80" :weight bold)))
;;       evil-emacs-state-tag    (propertize " Emacs "    'face '((:background "MediumOrchid" :foreground "DarkMagenta" :weight bold)))
;;       evil-motion-state-tag   (propertize " Motion "   'face '((:background "goldenrod4" :foreground "goldenrod1" :weight bold)))
;;       evil-operator-state-tag (propertize " Operator " 'face '((:background "RoyalBlue4" :foreground "DarkBlue" :weight bold))))

;; (setq evil-emacs-state-cursor    '("red" box))
;; (setq evil-normal-state-cursor   '("green" box))
;; (setq evil-visual-state-cursor   '("orange" box))
;; (setq evil-insert-state-cursor   '("red" bar))
;; (setq evil-replace-state-cursor  '("red" bar))
;; (setq evil-operator-state-cursor '("red" hollow))

;; pre-evil Stuff
(setq evil-want-C-u-scroll t)
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil)
(setq evil-cross-lines t)
(setq evil-intercept-esc 'always)

(setq evil-auto-indent t)

(require 'evil)

;; Actually activate evil mode
(evil-mode t)

;; Reclaim C-z for suspend in terminal
(evil-set-toggle-key "C-\\")
;; (evil-set-toggle-key "<pause>")

; (require 'surround)
; (global-surround-mode t)

;; evil-leader
(setq evil-leader/in-all-states t
      evil-leader/leader "SPC"
      evil-leader/non-normal-prefix "s-")

(require 'evil-leader)
(global-evil-leader-mode)

;; Unset shortcuts which shadow evil leader
(eval-after-load "compile"
 (define-key compilation-mode-map (kbd "SPC") nil))

;; make leader available in visual mode
(define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
(define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
(define-key evil-emacs-state-map (kbd "SPC") evil-leader--default-map)

;; Stop evil from overwriting cursor color
(setq evil-default-cursor t)
;; (setq evil-insert-state-cursor '("#aa0000" hbar))



;; Special Buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html

;; Force split horizontal
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "compilation errors, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (when (string-match "*compilation*" (buffer-name buf))
            ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
            (bury-buffer "*compilation*")
            (winner-undo)
            (message "Compilation Complete")
            ))))

(setq special-display-function
      (lambda (buffer &optional args)
        (split-window)
        (switch-to-buffer buffer)
        (get-buffer-window buffer 0)))



;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ag, The Silver Searcher
(require 'ag)
(setq ag-highlight-search t)



;; Smooth Scrolling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scroll-margin 8
      scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; Keep cursor away from edges when scrolling up/down
;; (require 'smooth-scrolling)



;; Clipboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)

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
    ;; Call back for when user pastes
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


;; Keybinding Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;; (Other) Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove suspend-frame. Three times.
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
(put 'suspend-frame 'disabled t)

;; Manual completion
;; (global-set-key "\t" 'company-complete-common)

;; Make end-of-line work in insert
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

;; Redefine ESC for Evil (By default it's meta)
(define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-operator-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-motion-state-map (kbd "ESC") 'evil-normal-state)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; Easier version of "C-x k" to kill buffer
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Evaluate Buffer
(global-set-key (kbd "C-c C-v") 'eval-buffer)
(global-set-key (kbd "C-c C-r") 'eval-region)

;; Frames
;; (define-key global-map (kbd "C-c f k") 'delete-frame)
;; (define-key global-map (kbd "C-c f n") 'make-frame-command)
;; (define-key global-map (kbd "C-c f o") 'other-frame)

;; (define-key evil-normal-state-map (kbd "g t") 'other-frame)

;; Workgroups2
;; (global-set-key (kbd "C-c l") 'wg-reload-session)
;; (global-set-key (kbd "C-c s") 'wg-save-session)
(global-set-key (kbd "C-c w") 'wg-switch-to-workgroup)
(global-set-key (kbd "C-c w") 'wg-create-workgroup)

(define-key evil-normal-state-map (kbd "g T") 'wg-switch-to-workgroup-left)
(define-key evil-normal-state-map (kbd "g t") 'wg-switch-to-workgroup-right)

;; Window Registers
(global-set-key (kbd "<f9>") '(lambda () (interactive) (jump-to-register 9)
                                (message "Windows disposition loaded")))
(global-set-key (kbd "<f10>") '(lambda () (interactive) (window-configuration-to-register 9)
                                 (message "Windows disposition saved")))

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Other
(global-set-key (kbd "RET") 'newline-and-indent)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around nil)

(global-set-key [kp-delete] 'delete-char)

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

;; Alternate escapes
;; (require 'key-chord)
;; (key-chord-mode 1)
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

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
                                                   (comment-or-uncomment-region (line-beginning-position) (line-end-position))
                                                   ))
(define-key evil-visual-state-map (kbd "g c") 'comment-or-uncomment-region)



;; Evil-leader Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alternate
(evil-leader/set-key "a" 'ff-find-other-file)

(evil-leader/set-key "c" '(lambda()
                            (interactive)
                            (file-name-directory (or load-file-name buffer-file-name))))

;; Eval
(evil-leader/set-key "eb" 'eval-buffer)
(evil-leader/set-key "er" 'eval-region)

;; Files and Directories
(evil-leader/set-key "d" 'ido-dired)
(evil-leader/set-key "f" 'ido-find-file)

(evil-leader/set-key "o" 'imenu)
(evil-leader/set-key "u" 'ido-switch-buffer)
(evil-leader/set-key "x" 'smex)

;; Projectile
(evil-leader/set-key "p"  'projectile-find-file)

;; Terminal
(evil-leader/set-key "t"  '(lambda()
                             (interactive)
                             (shell-command "$TERMINAL -e fish")))

;; Org Mode settings
(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  (kbd "TAB") 'org-cycle
  "za" 'org-cycle
  "zA" 'org-shifttab
  "zm" 'hide-body
  "zr" 'show-all
  "zo" 'show-subtree
  "zO" 'show-all
  "zc" 'hide-subtree
  "zC" 'hide-all
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'normal orgstruct-mode-map
  (kbd "RET") 'org-open-at-point
  (kbd "TAB") 'org-cycle
  "za" 'org-cycle
  "zA" 'org-shifttab
  "zm" 'hide-body
  "zr" 'show-all
  "zo" 'show-subtree
  "zO" 'show-all
  "zc" 'hide-subtree
  "zC" 'hide-all
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert org-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert orgstruct-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

;; Extra ido-mode keybindings
(defun split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun ido-invoke-in-other-window ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

(defun ido-invoke-in-horizontal-split ()
  "signals ido mode to split horizontally and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun ido-invoke-in-vertical-split ()
  "signals ido mode to split vertically and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defadvice ido-read-internal (around ido-read-internal-with-minibuffer-other-window activate)
  (let* (ido-exit-minibuffer-target-window
         (this-buffer (current-buffer))
         (result ad-do-it))
    (cond
     ((equal ido-exit-minibuffer-target-window 'other)
      (if (= 1 (count-windows))
          (split-window-horizontally-and-switch)
        (other-window 1)))
     ((equal ido-exit-minibuffer-target-window 'horizontal)
      (split-window-horizontally-and-switch))

     ((equal ido-exit-minibuffer-target-window 'vertical)
      (split-window-vertically-and-switch))
     ((equal ido-exit-minibuffer-target-window 'frame)
      (make-frame)))
    (switch-to-buffer this-buffer) ;; why? Some ido commands, such as textmate.el's textmate-goto-symbol don't switch the current buffer
    result))

(defadvice ido-init-completion-maps (after ido-init-completion-maps-with-other-window-keys activate)
  (mapcar (lambda (map)
            (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
            (define-key map (kbd "C-2") 'ido-invoke-in-vertical-split)
            (define-key map (kbd "C-3") 'ido-invoke-in-horizontal-split)
            (define-key map (kbd "C-4") 'ido-invoke-in-other-window)
            (define-key map (kbd "C-5") 'ido-invoke-in-new-frame))
          (list ido-buffer-completion-map
                ido-common-completion-map
                ido-file-completion-map
                ido-file-dir-completion-map)))

(defun my-ido-define-keys()
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook 'my-ido-define-keys)
