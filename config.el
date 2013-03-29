;; Helper Functions
(defun add-to-loadpath (&rest dirs)
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(add-to-loadpath "~/.emacs.d/pkg/tomorrow-theme"
                 "~/.emacs.d/config")

;; (add-to-list 'load-path (concat user-emacs-directory
;;                                 (convert-standard-filename "config")))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
;(setq delete-by-moving-to-trash t)

;;Real emacs knights don't use shift to mark things
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
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
;;(setq line-number-mode t)
;;(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
;;(recentf-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Represent undo-history as an actual tree (visualize with C-x u)
;;(setq undo-tree-mode-lighter "")
;;(require 'undo-tree)
;;(global-undo-tree-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
;(require 'uniquify)
;(setq uniquify-buffer-name-style 'forward)

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

;; Make the bars pretty
;; (require 'powerline)

;; Diminish modeline clutter
(require 'diminish)
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "El")))
(eval-after-load "Undo-Tree" '(diminish 'undo-tree-mode "ut"))

;; Fonts + theme
(set-mouse-color "#CCCCCC")

(require 'color-theme)
(require 'color-theme-tomorrow)
(color-theme-tomorrow-night)
;;(require 'ujelly-theme)
;;(load-theme 'ujelly t)
(set-frame-font "Inconsolata 10")

(setq default-frame-alist
      '((top . 10) (left . 2)
        (width . 80) (height . 53)
        (font . "Inconsolata 10")
        ))

(unless window-system
  (when (getenv "DISPLAY")
    (set-face-attribute 'default nil :background "unspecified-bg")
    ;(set-face-attribute 'font-lock-keyword-face nil :foreground "#8959a8")
    ;(set-face-attribute 'font-lock-keyword-face nil :foreground "#8959a8" :weight 'bold)

    ;; Powerline settings
    (custom-set-faces
     '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
     '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
    ))

;; Thematic configuration
;(add-hook 'before-make-frame-hook 'turn-off-tool-bar)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

;; No splash screen please ... jeez
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

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


;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
      (list
       "  "

       ;; the buffer name; the file name as a tool tip
       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                           'help-echo (buffer-file-name)))

       ;; the current major mode for the buffer.
       "["

       '(:eval (propertize "%m" 'face 'font-lock-string-face
                           'help-echo buffer-file-coding-system))

       ;; i don't want to see minor-modes; but if you want, uncomment this:
       minor-mode-alist  ;; list of minor modes
       "] "


       "[" ;; insert vs overwrite mode, input-method in a tooltip
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))

       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "Mod"
                                          'face 'font-lock-warning-face
                                          'help-echo "Buffer has been modified"))))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-type-face
                                          'help-echo "Buffer is read-only"))))
       "] "

       ;; add the time, with the date and the emacs uptime in the tooltip
       ;; '(:eval (propertize (format-time-string "%H:%M")
       ;;           'help-echo
       ;;           (concat (format-time-string "%c; ")
       ;;                   (emacs-uptime "Uptime:%hh"))))
       ;; " --"

       ;; relative position, size of file
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "] "

       ;; line and column
       "(" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-type-face) ","
       (propertize "%02c" 'face 'font-lock-type-face)
       ") "

       ;; "%-" ;; fill with '-'

       ))


;; Parenthesis matching
(require 'paren)
(show-paren-mode 1)
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
;; (require 'whitespace)
;; (whitespace-mode t)
;; (setq whitespace-display-mappings
;;       '((space-mark   ?\    [?\xB7]     [?.])        ; space
;;         (space-mark   ?\xA0 [?\xA4]     [?_])        ; hard space
;;         (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])    ; end-of-line
;;         ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun c-mode-common-custom ()
  (setq c-default-style "linux") ; linux-kernel-developers style indentation
  (setq c-basic-offset 4)        ; 4-space tab size

  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'access-label '0)
  (c-set-offset 'inline-open '0)

  (c-set-offset 'brace-list-open '0)
  )

(add-hook 'c-mode-common-hook 'c-mode-common-custom)

;; Interactively Do Things
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(setq ido-save-directory-list-file "~/.emacs.d/ido.last")

(add-hook
 'ido-setup-hook
 #'(lambda ()
     ;; Use C-w to go back up a dir to better match normal usage of C-w
     ;; - insert current file name with C-x C-w instead.
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
     (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
;;(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;;(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(require 'smex)
(smex-initialize)

(setq smex-key-advice-ignore-menu-bar t)
(setq smex-save-file "~/.emacs.d/smex-items")

(require 'dired)

;; Dired uses human readable sizes.
;;(setq dired-listing-switches "-alh")
(setq dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")

(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-clang)
;(require 'auto-complete-yasnippet)

;(require 'auto-complete-emacs-lisp)
;(require 'auto-complete-latex)
;(require 'ac-math)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")

;(ac-config-default)
(defcustom mycustom-system-include-paths
  '(
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/../../../../include/c++/4.7.1"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/../../../../include/c++/4.7.1/x86_64-unknown-linux-gnu"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/../../../../include/c++/4.7.1/backward"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/include"
    "/usr/local/include"
    "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/include-fixed"
    "/usr/include"
    )
  "This is a list of include paths that are used by the clang auto completion."
  :group 'mycustom
  :type '(repeat directory)
  )

(setq clang-completion-suppress-error 't)
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (append
               mycustom-system-include-paths
               )
              )
      )

;; C-common mode setup
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-dictionary
                             ac-source-filename
                             ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;;(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

(my-ac-config)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(ac-set-trigger-key "TAB")
;;(define-key ac-mode-map  [(control tab)] 'auto-complete)

;; Key mappings
(setq ac-use-menu-map t)

;(define-key ac-menu-map "\C-n" 'ac-next)
;(define-key ac-menu-map "\C-p" 'ac-previous)
;(define-key ac-menu-map (kbd "TAB") 'ac-next)
;(define-key ac-menu-map (kbd "M-TAB") 'ac-previous)
(define-key ac-menu-map (kbd "<tab>") 'ac-next)
(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)

;; Stuff to help in terminal emacs
(define-key ac-menu-map (kbd "ESC") 'ac-stop)
(define-key ac-menu-map (kbd "C-j") 'ac-next)
(define-key ac-menu-map (kbd "C-k") 'ac-previous)

;; (define-key ac-menu-map (kbd "TAB") nil)
;; (define-key ac-menu-map (kbd "RET") 'ac-complete)

;; Colors
;(set-face-background 'ac-candidate-face "lightgray")
;(set-face-underline 'ac-candidate-face "darkgray")
;(set-face-background 'ac-selection-face "steelblue")
(set-face-foreground 'ac-selection-face "black")

(require 'ac-math)

;; PDF stuff
(setq TeX-PDF-mode t)
(setq latex-run-command "pdflatex")
;(setq TeX-engine 'pdflatex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq ac-math-unicode-in-math-p t)

;(add-hook ‘latex-mode-hook ‘LaTeX-math-mode)
;(add-hook ‘lateX-mode-hook ‘auto-fill-mode)

;; (setq TeX-view-program-list
;;       '(("zathura" "/usr/bin/zathura %q")))

;; (setq TeX-view-program-selection '((output-pdf "zathura")))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))

;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             (require 'rename-sgml-tag)
;;             (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))


;; Evil Stuff
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil)
(setq evil-cross-lines t)
(setq evil-intercept-esc 'always)

(setq evil-auto-indent t)

(require 'evil)
(evil-mode t)

(require 'surround)
(global-surround-mode 1)

;; Cursor Color
(setq evil-default-cursor t)
(set-cursor-color "#CCCCCC")
;;(setq evil-insert-state-cursor '("#aa0000" hbar))


;; Alternate File
(evil-ex-define-cmd "A"  'ff-find-other-file)
(evil-ex-define-cmd "AS" (lambda()
                           (interactive)
                           (split-window-below)
                           (evil-window-down 1)
                           (ff-find-other-file)))
(evil-ex-define-cmd "AV" (lambda()
                           (interactive)
                           (split-window-right)
                           (evil-window-right 1)
                           (ff-find-other-file)))

;; Ido-open file
(evil-ex-define-cmd "F" 'ido-find-file)
(evil-ex-define-cmd "FS" (lambda()
                           (interactive)
                           (split-window-below)
                           (evil-window-down 1)
                           (ido-find-file)))
(evil-ex-define-cmd "FV" (lambda()
                           (interactive)
                           (split-window-right)
                           (evil-window-right 1)
                           (ido-find-file)))

;; Buffers
(evil-ex-define-cmd "b"  'ido-switch-buffer)      ;B to switch buffers
(evil-ex-define-cmd "B"  'ido-switch-buffer)      ;B to switch buffers
(evil-ex-define-cmd "bm" 'buffer-menu)            ;Bm to open buffer menu
(evil-ex-define-cmd "bw" (lambda()
                           (interactive)
                           (kill-this-buffer)
                           (delete-window)))      ;Bw to delete buffers
(evil-ex-define-cmd "BW" 'kill-this-buffer)       ;Bw to delete buffers

;; Workgroups
;; (require 'workgroups)

;; (evil-ex-define-cmd "tabnew"   'wg-create-workgroup)
;; (evil-ex-define-cmd "tabclone" 'wg-clone-workgroup)
;; (evil-ex-define-cmd "tabdel"   'wg-kill-workgroup)
;; (evil-ex-define-cmd "tabprev"  'wg-switch-left)
;; (evil-ex-define-cmd "tabnext"  'wg-switch-right)

(define-key evil-normal-state-map "J" 'wg-switch-left)
(define-key evil-normal-state-map "K" 'wg-switch-right)

;; Redefine ESC (By default it's meta)
(define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-operator-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-motion-state-map (kbd "ESC") 'evil-normal-state)

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

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

;; Ace Jump Motions
(defmacro evil-enclose-ace-jump (&rest body)
  `(let ((old-mark (mark)))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (progn
           ,@body
           (recursive-edit))
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (evil-enclose-ace-jump
   (ace-jump-mode 9)))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 1)))

(evil-define-motion evil-ace-jump-char-to-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)
   (forward-char -1)))

;; some proposals for binding:

(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
;;(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)
;;
;;(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-char-mode) ; similar to f
;;(define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
;;(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;; Compile display to split window
(setq special-display-buffer-names
      '("*compilation*"))

(setq special-display-function
      (lambda (buffer &optional args)
        (split-window)
        (switch-to-buffer buffer)
        (get-buffer-window buffer 0)))

;; File functions
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

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

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-q") 'er/expand-region)

;; Easier version of "C-x k" to kill buffer
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Evaluate Buffer
(global-set-key (kbd "C-c C-v") 'eval-buffer)
(global-set-key (kbd "C-c C-r") 'eval-region)

;; Commentin'
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Ace Jump Mode
(global-set-key (kbd "M-q") 'ace-jump-mode)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around nil)

(global-set-key [kp-delete] 'delete-char)

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
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Various superfluous white-space. Just say no.
;;(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Seed the random number generator
(random t)

; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

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
