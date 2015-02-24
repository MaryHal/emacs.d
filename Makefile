profile:
	emacs -Q -l site-lisp/profile-dotemacs/profile-dotemacs.el \
    --eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
    -f profile-dotemacs
