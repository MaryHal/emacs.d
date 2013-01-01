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
   ;auto-complete-yasnippet
   ;auto-complete-emacs-lisp
   auctex
   ;ac-math
   yasnippet
   workgroups
   powerline
   smex
   magit
   (:name expand-region
          :type git
          :url "git://github.com/magnars/expand-region.el.git")
   diminish
   undo-tree
   evil
   evil-surround
   ace-jump-mode
   smooth-scrolling
   php-mode
   lua-mode))

(el-get 'sync)

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

(require 'tramp)

(require 'workgroups)
;(workgroups-mode 1)

;; Personal Stuff
(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "config")))

(require 'sane-defaults)
(require 'appearance)        ; uses color-theme, color-theme-tomorrow, powerline

;; Mode hooks
(require 'setup-cc-hooks)

(require 'setup-ido)         ; uses smex
(require 'setup-dired)

(require 'setup-yasnippet)
(require 'setup-ac)          ; Uses auto-complete-config, auto-complete-clang,
(require 'setup-latex-mode)  ; uses auctex, ac-math

(require 'setup-html-mode)   ; uses zencoding-mode
;(require 'setup-web-mode)    ; uses web mode

(require 'setup-evil)        ; uses evil, evil-surround

(require 'func)
(require 'keybindings)       ; Uses expand-region, smex, ace-jump-mode
(require 'misc)              ; Uses smooth-scrolling

(require 'setup-copypaste)
