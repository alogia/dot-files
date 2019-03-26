(setq slime-contribs '(slime-fancy))

(require 'rtags)
(require 'ido)
(require 'magit)
(require 'counsel-etags)

(require 'clang-format)
(global-set-key (kbd "C-c C-f") 'clang-format-region)

;(require 'modern-cpp-font-lock)
;(modern-c++-font-lock-global-mode t)

(ido-mode 'both)


(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize) ;; You might already have this line

(setq backup-directory-alist `(("." . "~/.saves")))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-n") 'forward-word)

(add-hook 'lisp-mode-hook 
	  (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook 
	  (lambda () (inferior-slime-mode t)))

(add-hook 'slime-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-n") 'previous-line)
	    (local-set-key (kbd "M-h") 'backward-char)))

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t
   "Modify the behavior of the open-*-line functions to cause them to autoindent.")


(setq inferior-lisp-program "/usr/bin/sbcl")
(set-default-font "-*-terminus-*-*-*-*-*-320-*-*-*-*-*-*")
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq tramp-default-method "ssh")


(bind-keys*
 ("C-t"   . forward-paragraph)
 ("C-S-t" . backward-paragraph)
 ("M-t"   . next-line)
 ("M-n"   . previous-line)
 ("M-s"   . forward-char)
 ("M-h"   . backward-char)
 ("M-b"   . backward-word)
 ("M-l"   . forward-word)
 ("C-n"   . move-end-of-line)
 ("S-SPC" . set-mark-command)
 ("C-q"   . iflipb-next-buffer)
 ("C-;"   . iflipb-previous-buffer)
 ("<f7>"  . flyspell-mode)
 ("<f8>"  . flyspell-auto-correct-word)
 ("C-,"   .  imenu-anywhere)
)

;;; AucTex setup
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Markdown mode setup
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (modern-cpp-font-lock clang-format counsel-etags rtags imenu-anywhere org-link-minor-mode iflipb markdown-mode ## flyspell-correct haskell-mode haskell-snippets haskell-tab-indent slime slime-company use-package macrostep ace-jump-buffer)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
