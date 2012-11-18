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

(provide 'setup-latex-mode)
