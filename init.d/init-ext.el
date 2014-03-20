;; save packages

(req-package save-packages
  :init
  (progn (setq save-packages-file "~/.emacs.d/saved-packages")
         (add-hook 'kill-emacs-hook 'save-packages)))

;; swoop

(req-package swoop)

;; sudo support

(req-package sudo-ext)

;; shell command

(req-package shell-command)

;; insert shebang

;; (req-package insert-shebang
;;   :init
;;   (add-hook 'find-file-hook 'insert-shebang))

;; indentation

(setq-default tab-width 4)
(add-hook 'find-file-hook (lambda () (setq indent-tabs-mode nil)))
(electric-indent-mode 1)

;; mark ring tweaks

(setq set-mark-command-repeat-pop t)

;; save bookmarks on emacs exit

;; (setq bookmark-save-flag 1)

;; turn off backup files

;; (setq make-backup-files nil)

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

;; duplicate thing

(req-package duplicate-thing
  :init (progn (global-set-key (kbd "M-c") 'duplicate-thing)
               (global-set-key (kbd "M-с") 'duplicate-thing)))

;; auto pair

(req-package autopair
  :init (add-hook 'prog-mode-hook (lambda () (autopair-mode 1))))

;; auto complete

(req-package auto-complete
  :init (progn (require 'auto-complete-config)
               (global-auto-complete-mode t)
               (setq ac-auto-start 1)
               (setq ac-quick-help-delay 0.1)
               (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)))

;; yasnippet

(req-package yasnippet
  :init (yas-global-mode 1))

(provide 'init-ext)
