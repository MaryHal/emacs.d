;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;disable backup
;(setq backup-inhibited t)

;disable auto save
(auto-save-mode nil)
(setq auto-save-default nil)

;; Place Backup Files in a Specific Directory
(setq make-backup-files t)
;; Write backup files to own directory
;(setq backup-directory-alist `(("." . ,(expand-file-name
;                                       (concat "~/.emacs.d/" "backups")))))
(setq backup-directory-alist
      `((".*" . , (concat temporary-file-directory "emacs_backups"))))
;(setq auto-save-file-name-transforms
;      `((".*" ,temporary-file-directory t)))

;; Various superfluous white-space. Just say no.
;(add-hook 'before-save-hook 'cleanup-buffer-safe)

(require 'smooth-scrolling)

;; Seed the random number generator
(random t)

;; Whitespace-style
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)


(provide 'misc)

