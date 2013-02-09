;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;disable backup
(setq backup-inhibited t)

;disable auto save
(auto-save-mode nil)
(setq auto-save-default nil)
(with-current-buffer (get-buffer "*scratch*")
  (auto-save-mode -1))

;; Place Backup Files in a Specific Directory
(setq make-backup-files nil)
;; Write backup files to own directory
;(setq backup-directory-alist `(("." . ,(expand-file-name
;                                       (concat "~/.emacs.d/" "backups")))))
(setq backup-directory-alist
      `((".*" . , (concat temporary-file-directory "emacs_backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Various superfluous white-space. Just say no.
;(add-hook 'before-save-hook 'cleanup-buffer-safe)

(require 'smooth-scrolling)

;; Seed the random number generator
(random t)

(provide 'misc)

