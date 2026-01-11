;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell
;; Custom commands are kept in .emacs.d/eshell/commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define the commands which will be run in an ansi-term instead of eshell
(defvar my:eshell-visual-commands '("ssh" "tail" "htop" "tmux" "vim"))


(use-package eshell
  :ensure t
  :init
  (load (expand-file-name "~/.emacs.d/eshell/commands.el"))
  (setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:" ;TODO: Add all the proper paths in here
         (getenv "PATH")))
  
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prefer-lisp-functions nil)
  (eshell-destroy-buffer-when-process-dies t)
  :hook
  (eshell-mode . (lambda ()
                   ;; Must define keymap in mode hook
                   (define-key eshell-mode-map (kbd "C-!") 'eshell/x)
                   (setq eshell-visual-commands
                         (delete-dups (append my:eshell-visual-commands eshell-visual-commands)))))
  )
                   
