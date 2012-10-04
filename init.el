;; Pre-init settings (really ugly)
(setq evil-want-C-u-scroll t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq
 el-get-sources
 '(el-get
   switch-window
   color-theme
   color-theme-tomorrow
   auto-complete
   auto-complete-clang
   auto-complete-emacs-lisp
   auctex
   ac-math
   yasnippet
   auto-complete-yasnippet
   workgroups
   powerline
   smex
   expand-region
   ;;multiple-cursors
   diminish
   (:name undo-tree
    :type http
    :url "file:///home/sanford/.emacs.d/site-lisp/undo-tree/undo-tree.el")
   evil
   smooth-scrolling
   lua-mode
   haskell-mode))

(el-get 'sync)

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))


(require 'workgroups)
;(workgroups-mode 1)

;; Personal Stuff
(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "config")))

(require 'sane-defaults)
(require 'appearance)          ; uses color-theme, color-theme-tomorrow, powerline

(require 'setup-cc-hooks)

(require 'setup-ido)           ; uses smex
(require 'setup-dired)

; Uses auto-complete-config, auto-complete-clang,
; auto-complete-yasnippet, yasnippet, auto-complete-emacs-lisp
(require 'setup-yasnippet)
(require 'setup-ac)

(require 'setup-evil)        ; uses evil

(require 'func)
(require 'keybindings) ; Uses expand-region, smex
(require 'misc)

(require 'setup-copypaste)

;; Diminish modeline clutter
(require 'diminish)

