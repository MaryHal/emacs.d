(if (eq system-type 'gnu/linux)

	(progn (req-package battery
             :init
             (progn (setq battery-mode-line-format " %p%%")
                    (display-battery-mode)))

           ;; aur interface
           (req-package aurel)))

(provide 'init-linux)
