;; completion with clang

(req-package auto-complete-clang
  :require
  (auto-complete cc-mode)
  :init
  (progn (add-hook 'c++-mode-hook 'cc-mode-clang-hook)
         (add-hook 'c-mode-hook 'cc-mode-clang-hook)
         (setq-default c-basic-offset 4)
         (setq-default c-default-style "bsd")))

;; headers completion

(req-package auto-complete-c-headers
  :require
  auto-complete-clang)

;; detect mode for .h file

(req-package dummy-h-mode
  :require
  cc-mode
  :init
  (progn (add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
         (add-hook 'dummy-h-mode-hook
                   (lambda ()
                     (setq dummy-h-mode-default-major-mode 'c++-mode)))
         (add-hook 'dummy-h-mode-hook
                   (lambda ()
                     (setq dummy-h-mode-search-limit 60000)))))

;; gdb

(req-package gdb-mi
  :require
  cc-mode
  :init
  (progn (setq gdb-many-windows t)
         (setq gdb-show-main t)))

;; snippets using helm

(req-package helm-c-yasnippet
  :require
  (helm yasnippet cc-mode auto-complete auto-complete-clang)
  :init
  (define-key global-map (kbd "C-M-y") 'helm-c-yas-complete))

;; rtags

(req-package rtags)

;; some utils

(defun find-makefile-dir (cur)
  (if (file-exists-p (concat cur "Makefile"))
      cur
    (if (string-equal (expand-file-name cur) "/")
        nil
      (find-makefile-dir (expand-file-name (concat cur "../"))))))

(defun expand-include-flag (a)
  (if (string-prefix-p "-I" a)
      (concat "-I" (expand-file-name (concat (find-makefile-dir "./") (substring a 2))))
    a))

(defun cc-mode-clang-hook ()
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-clang)
  (add-to-list 'ac-sources 'ac-source-c-headers)

  (setq cc-search-directories (split-string (shell-command-to-string "bash ~/.emacs.d/clang-include-paths.sh")))

  (setq ac-clang-flags
        (mapcar (lambda (item) (concat "-I" item))
                (split-string (shell-command-to-string "bash ~/.emacs.d/clang-include-paths.sh"))))

  (setq ac-clang-flags (append ac-clang-flags
                               (mapcar 'expand-include-flag
                                       (split-string (shell-command-to-string (concat (concat "make -C " (find-makefile-dir "./")) " -s print-cflags")))))))

(provide 'init-cc)
