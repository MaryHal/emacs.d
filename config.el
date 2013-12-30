;; Helper Functions
(defun add-to-loadpath (&rest dirs)
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;; Custom configuration files
(add-to-loadpath (concat user-emacs-directory "pkg/emacs-clang-complete-async"))

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
;;(setq undo-tree-mode-lighter "")
;;(require 'undo-tree)
;;(global-undo-tree-mode)

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

;; When popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Ignore wiki packages
(defadvice package--add-to-archive-contents
  (around package-filter-wiki-packages (package archive) activate compile)
  (unless (string-match-p "\\[wiki\\]$" (package-desc-doc (cdr package)))
    ad-do-it))

;; After load theme, if in terminal, clear background.
(defadvice load-theme (after load-theme activate compile)
  (unless window-system
    (when (getenv "DISPLAY")
      (set-face-attribute 'default nil :background "unspecified-bg")
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

(defadvice delete-window
  (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'delete-window)

;; Mouse support
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; Seed the random number generator
(random t)

;; Fonts + theme
;; Theme is loaded via Customize
;; (load-theme 'base16-eighties)

(set-cursor-color "#CCCCCC")
(set-mouse-color "#CCCCCC")

(setq default-frame-alist
      '((top   . 10) (left   . 2)
        (width . 80) (height . 30)
        (mouse-color  . "#CCCCCC")
        (cursor-color . "#CCCCCC")
        ))

;; Setting font
(if (string= system-type "windows-nt")
    ;; If Windows
    (progn (setq myFrameFont "Consolas 10")
           (add-to-list 'default-frame-alist '(font . "Consolas 10"))
           )
  ;; Not Windows
  (progn (setq myFrameFont "Inconsolata 10")
         (add-to-list 'default-frame-alist '(font . "Inconsolata 10"))
         )
  )

;; Clear background if in terminal
;; (unless window-system
;;   (when (getenv "DISPLAY")
;;     (set-face-attribute 'default nil :background "unspecified-bg")
;;     ))

;; In text terminals, change vertical divider


;; Thematic configuration
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

(setq-default indent-tabs-mode nil) ;; No tabs

;; Don't add newlines when cursor goes past end of file
(setq next-line-add-newlines nil)
(setq require-final-newline nil)

;; Don't Blink Cursor
(blink-cursor-mode -1)

;; Fringe
(set-fringe-mode 0)

(setq visible-bell nil
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#def")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;; Whitespace-style
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(setq ido-save-directory-list-file (concat user-emacs-directory "cache/ido.last"))

(defun my-ido-define-keys()
(define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
(define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)
(define-key ido-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook 'my-ido-define-keys)

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
;; (setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(require 'flx-ido)
(flx-ido-mode t)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Disable flx highlights
;; (setq flx-ido-use-faces nil)

(require 'smex)
(smex-initialize)

(setq smex-key-advice-ignore-menu-bar t)
(setq smex-save-file (concat user-emacs-directory "cache/smex-items"))

;; Helm
(require 'helm)
(require 'helm-config)
(require 'helm-swoop)

;; (setq helm-idle-delay 0.1
;;       helm-input-idle-delay 0
;;       helm-quick-update t
;;       helm-candidate-number-limit nil
;;       helm-su-or-sudo "sudo"
;;       helm-allow-skipping-current-buffer nil
;;       helm-enable-shortcuts t)

;; Helm keybindings
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-h") 'helm-previous-source)
(define-key helm-map (kbd "C-l") 'helm-next-source)

(setq projectile-enable-caching t)

(defvar projectile-cache-file (concat user-emacs-directory "cache/projectile.cache"))
(defvar projectile-known-projects-file (concat user-emacs-directory "cache/projectile-bookmarks.eld"))

(require 'projectile)

(setq projectile-indexing-method 'native)

(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")

(projectile-global-mode t)

(require 'dired)

;; Dired uses human readable sizes.
;;(setq dired-listing-switches "-alh")
(setq dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")


;; Emacs-Lisp Hooks
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "ξLisp")))

;; C Mode Hooks
(defun c-mode-common-custom ()
  (setq c-default-style "bsd")
  ;; 4-space tab size
  (setq c-basic-offset 4))

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook 'c-mode-common-custom)

;; Guess indentation style --------------------
;;Automagically find the indentation style in the source file.
;;(Good for me, since my collaborator is using tabs)

;;Use autoload for faster start-up (rather than "require"):
;;(autoload 'dtrt-indent-mode "dtrt-indent" "Adapt to foreign indentation offsets" t)
;;(add-hook 'c-mode-common-hook 'dtrt-indent-mode)

(add-hook 'c-mode-common-hook
          (lambda()
            (when (require 'dtrt-indent nil 'noerror)
              (dtrt-indent-mode t))))

;; Haskell Mode Hooks
(defun haskell-mode-common-custom()
  (haskell-doc-mode)
  ;; (haskell-indentation-mode)
  ;; (haskell-simple-indent-mode)
  (haskell-indent-mode)
  )
(add-hook 'haskell-mode-hook 'haskell-mode-common-custom)

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

;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)

;; Auto-complete dictionary directories. It should already contain the default dictionaries.
;; (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict/"))

(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable (concat user-emacs-directory
                                             "pkg/emacs-clang-complete-async/clang-complete"))
  (setq ac-clang-flags
        (mapcar (lambda (item)(concat "-I" item))
                (split-string
                 "
 /usr/include/c++/4.8.2
 /usr/include/c++/4.8.2/x86_64-unknown-linux-gnu
 /usr/include/c++/4.8.2/backward
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include
 /usr/local/include
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include-fixed
 /usr/include
"
                 )))
  (setq ac-sources '(ac-source-clang-async ac-source-dictionary ac-source-filename))
  (ac-clang-launch-completion-process)
  )

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-dictionary
                             ac-source-filename
                             ac-source-words-in-buffer
                             ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode t))
                       ))
(real-global-auto-complete-mode t)

(my-ac-config)

;; Triggered Auto complete
;; (setq ac-auto-start nil)
;; (setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;;(define-key ac-mode-map  [(control tab)] 'auto-complete)

;; Automatic Auto Complete
(setq ac-auto-start 2
      ac-auto-show-menu 0.1
      ac-quick-help-delay 0.5
      ac-quick-help-height 50)
(setq ac-show-menu-immediately-on-auto-complete t)

;; Fuzzy matching
(setq ac-use-fuzzy t)

;; Set history file location
(setq ac-comphist-file (expand-file-name "cache/ac-comphist.dat" user-emacs-directory))

;; Key mappings
(setq ac-use-menu-map t)

(define-key ac-menu-map (kbd "<tab>") 'ac-next)
(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
(define-key ac-menu-map (kbd "C-j") 'ac-next)
(define-key ac-menu-map (kbd "C-k") 'ac-previous)

(define-key ac-menu-map (kbd "RET") 'ac-complete)
(define-key ac-menu-map (kbd "ESC") 'ac-stop)
(define-key ac-menu-map (kbd "C-l") 'ac-expand-common)

;; Colors
;; (set-face-background 'ac-candidate-face "lightgray")
;; (set-face-underline 'ac-candidate-face "darkgray")
;; (set-face-background 'ac-selection-face "steelblue")
(set-face-foreground 'ac-selection-face "black")

;; PDF stuff
(setq TeX-PDF-mode t)
;; (setq latex-run-command "pdflatex")
;; (setq TeX-engine 'pdflatex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; (setq ac-math-unicode-in-math-p t)

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))

;; Tag colors (For use in modeline)
;; (setq evil-normal-state-tag   (propertize " Normal "   'face '((:background "LimeGreen" :foreground "DarkGreen" :weight bold)))
;;       evil-insert-state-tag   (propertize " Insert "   'face '((:background "grey80" :foreground "NavyBlue" :weight bold)))
;;       evil-visual-state-tag   (propertize " Visual "   'face '((:background "DarkOrange" :foreground "Red4" :weight bold)))
;;       evil-replace-state-tag  (propertize " Replace "  'face '((:background "red3" :foreground "grey80" :weight bold)))
;;       evil-emacs-state-tag    (propertize " Emacs "    'face '((:background "MediumOrchid" :foreground "DarkMagenta" :weight bold)))
;;       evil-motion-state-tag   (propertize " Motion "   'face '((:background "goldenrod4" :foreground "goldenrod1" :weight bold)))
;;       evil-operator-state-tag (propertize " Operator " 'face '((:background "RoyalBlue4" :foreground "DarkBlue" :weight bold))))

;; Mode line
(setq sml/theme 'dark)
(require 'smart-mode-line)
(sml/setup)

;; Highlight current line
;; (global-hl-line-mode t)
;; (set-face-background 'hl-line "#222")

;; pre-evil Stuff
(setq evil-want-C-u-scroll t)
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil)
(setq evil-cross-lines t)
(setq evil-intercept-esc 'always)

(setq evil-auto-indent t)

;; evil
(require 'evil)

;; Actually activate evil mode
(evil-mode t)

;; Reclaim C-z for suspend in terminal
(evil-set-toggle-key "C-\\")

(require 'surround)
(global-surround-mode t)

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

;; Cursor Color
(setq evil-default-cursor t)
;; (setq evil-insert-state-cursor '("#aa0000" hbar))

;; Bury the compilation buffer when compilation is finished and successful.
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (bury-buffer "*compilation*")
         (winner-undo)
         (message "Build successful."))
        (t
         (message "Compilation exited abnormally: %s" string))))

(setq special-display-function
      (lambda (buffer &optional args)
        (split-window)
        (switch-to-buffer buffer)
        (get-buffer-window buffer 0)))

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

;; ag, The Silver Searcher
(require 'ag)
(setq ag-highlight-search t)

;; Keep cursor away from edges when scrolling up/down
;; (require 'smooth-scrolling)
(setq scroll-margin 8
      scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)

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

;; (require 'switch-window)
;; (setq switch-window-shortcut-style 'qwerty)

;; Ace Jump
(require 'ace-jump-mode)
(setq ace-jump-mode-case-fold t)
(setq ace-jump-mode-scope 'window)
(ace-jump-mode-enable-mark-sync)

;; ;; Lowercase only for ace-jump
;; (setq ace-jump-mode-move-keys
;;       (loop for i from ?a to ?z collect i))

;; (global-set-key (kbd "C-c SPC") 'ace-jump-char-mode)
;; (define-key evil-normal-state-map (kbd "") 'ace-jump-mode)


;; Redefine ESC (By default it's meta)
(define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-operator-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-motion-state-map (kbd "ESC") 'evil-normal-state)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; Expand Region
(require 'expand-region)
;;(global-set-key (kbd "C-q") 'er/expand-region)

;; Easier version of "C-x k" to kill buffer
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Evaluate Buffer
(global-set-key (kbd "C-c C-v") 'eval-buffer)
(global-set-key (kbd "C-c C-r") 'eval-region)

;; Commentin'
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Create new frame
(define-key global-map (kbd "C-c f k") 'delete-frame)
(define-key global-map (kbd "C-c f n") 'make-frame-command)
(define-key global-map (kbd "C-c f o") 'other-frame)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Projectile
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

;; ;; Multiple Cursors
;; (define-key evil-emacs-state-map (kbd "C->") 'mc/mark-next-like-this)
;; (define-key evil-emacs-state-map (kbd "C-<") 'mc/mark-previous-like-this)
;; (define-key evil-visual-state-map (kbd "C->") 'mc/mark-all-like-this)
;; (define-key evil-normal-state-map (kbd "C->") 'mc/mark-next-like-this)
;; (define-key evil-normal-state-map (kbd "C-<") 'mc/mark-previous-like-this)

;; Other
(global-set-key (kbd "RET") 'newline-and-indent)

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

;; Package list: don't need to switch to evil mode if I have these two keys...
(define-key package-menu-mode-map "j" 'next-line)
(define-key package-menu-mode-map "k" 'previous-line)

;; gj gk by default
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Let K match J
(define-key evil-normal-state-map (kbd "K") 'evil-join-previous-line)

;; Make Y work like D
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

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
    ; ensure mark is less than point
    (when (> (mark) (point))
        (exchange-point-and-mark)
    )
    (evil-normal-state)
    (evil-shift-left (mark) (point))
    ;; re-select last visual-mode selection
    (evil-visual-restore)))

;; Alternate escapes
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

(key-chord-define evil-normal-state-map "jc" 'ace-jump-char-mode)
(key-chord-define evil-normal-state-map "jl" 'ace-jump-line-mode)
(key-chord-define evil-normal-state-map "jw" 'ace-jump-word-mode)

;; "Unimpaired"
(define-key evil-normal-state-map (kbd "[ SPC") 'evil-insert-line-above)
(define-key evil-normal-state-map (kbd "] SPC") 'evil-insert-line-below)
(define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
(define-key evil-normal-state-map (kbd "] b") 'next-buffer)

;; Bubble Text up and down. Works with regions.
(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)

;; evil-leader keybindings
(evil-leader/set-key "SPC" 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; Alternate
(evil-leader/set-key "aa" 'ff-find-other-file)
(evil-leader/set-key "as" (lambda()
                            (interactive)
                            (split-window-below)
                            (evil-window-down 1)
                            (ff-find-other-file)))
(evil-leader/set-key "av" (lambda()
                            (interactive)
                            (split-window-right)
                            (evil-window-right 1)
                            (ff-find-other-file)))

;; Buffers
(evil-leader/set-key "bb" 'ido-switch-buffer)
(evil-leader/set-key "bk" 'ido-kill-buffer)
(evil-leader/set-key "bm" 'buffer-menu)
(evil-leader/set-key "bn" 'switch-to-next-buffer)
(evil-leader/set-key "bp" 'switch-to-prev-buffer)
(evil-leader/set-key "bw" (lambda()
                            (interactive)
                            (kill-this-buffer)
                            (delete-window)))
(evil-leader/set-key "bW" 'kill-this-buffer)

;; Eval
(evil-leader/set-key "eb" 'eval-buffer)
(evil-leader/set-key "er" 'eval-region)

;; File
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

;; Helm
(evil-leader/set-key "hb" 'helm-buffers-list)
(evil-leader/set-key "hc" 'helm-browse-code)
(evil-leader/set-key "hf" 'helm-find-files)
(evil-leader/set-key "hi" 'helm-imenu)
(evil-leader/set-key "hk" 'helm-show-kill-ring)
(evil-leader/set-key "hm" 'helm-mini)
(evil-leader/set-key "hs" 'helm-swoop)
(evil-leader/set-key "hx" 'helm-M-x)

;; Jump. ACE Jump.
(evil-leader/set-key "jc" 'ace-jump-char-mode)
(evil-leader/set-key "jw" 'ace-jump-word-mode)

;; Line insertion
(evil-leader/set-key "jj" 'evil-insert-line-below)
(evil-leader/set-key "kk" 'evil-insert-line-above)

;; narrow & widen
(evil-leader/set-key "nr" 'narrow-to-region)
(evil-leader/set-key "np" 'narrow-to-page)
(evil-leader/set-key "nf" 'narrow-to-defun)
(evil-leader/set-key "nw" 'widen)

;; Projectile
(evil-leader/set-key "p"  'projectile-ag)

;; Terminal
(evil-leader/set-key "t"  '(lambda()
                             (interactive)
                             (async-shell-command "urxvtc")))

;; Selection
(evil-leader/set-key "v" 'er/expand-region)

;; Window
(evil-leader/set-key "wb" 'balance-windows)
(evil-leader/set-key "wc" 'delete-window)

(evil-leader/set-key "wH" 'evil-window-move-far-left)
(evil-leader/set-key "wh" 'evil-window-left)

(evil-leader/set-key "wJ" 'evil-window-move-very-bottom)
(evil-leader/set-key "wj" 'evil-window-down)
(evil-leader/set-key "wK" 'evil-window-move-very-top)
(evil-leader/set-key "wk" 'evil-window-up)
(evil-leader/set-key "wL" 'evil-window-move-far-right)
(evil-leader/set-key "wl" 'evil-window-right)

(evil-leader/set-key "wm" 'toggle-maximize-buffer)
(evil-leader/set-key "ws" 'split-window-vertically)
(evil-leader/set-key "wv" 'split-window-horizontally)
(evil-leader/set-key "ww" 'switch-window)

;; text
(evil-leader/set-key "xdw" 'delete-trailing-whitespace)
;; (evil-leader/set-key "xtc" 'transpose-chars)
;; (evil-leader/set-key "xtl" 'transpose-lines)
;; (evil-leader/set-key "xtw" 'transpose-words)
(evil-leader/set-key "xbu" 'untabify)
(evil-leader/set-key "xbt" 'tabify)
(evil-leader/set-key "xU"  'upcase-word)
(evil-leader/set-key "xu"  'downcase-word)

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
