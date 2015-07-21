
;;; Code:

;;; init.el

(defconst user-org-init-file (concat user-emacs-directory "init.org"))

;; Manually evaluate emacs-lisp code blocks in init.org
(with-temp-buffer
  (insert-file-contents user-org-init-file)
  (goto-char (point-min))
  (search-forward "\n* Preload Init")
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report Headers
     ((looking-at
       "\\*+ +.*$")
      (message "%s" (match-string 0)))
     ;; Evaluate Code Blocks
     ((looking-at "^ *#\\+BEGIN_SRC +emacs-lisp *$")
      (let ((l (match-end 0)))
        (search-forward-regexp "^ *#\\+END_SRC")
        (eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 header
     ((looking-at "^\\* ")
      (goto-char (point-max))))))

(provide 'init)
;;; init.el ends here
