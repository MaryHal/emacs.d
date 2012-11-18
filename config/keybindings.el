;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-q") 'er/expand-region)

;; Easier version of "C-x k" to kill buffer
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Evaluate Buffer
(global-set-key (kbd "C-c C-v") 'eval-buffer)

;; Commentin'
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'comment-or-uncomment-region)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Multiple Cursors
;; (require 'multiple-cursors)
;; (mc/execute-command-for-all-fake-cursors 'backward-char)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; ;; Mark additional regions matching current region
;; (global-set-key (kbd "C-c C-e") 'mc/mark-more-like-this-extended)
;; (global-set-key (kbd "C-c r") 'mc/mark-all-in-region)

;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around nil)

(global-set-key [kp-delete] 'delete-char)

(provide 'keybindings)

