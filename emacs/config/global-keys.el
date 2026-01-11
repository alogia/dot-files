(use-package bind-key
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Bindings regardless of any mode
;; ONLY USE FOR BINDINGS WHICH SHOULD NEVER BE OVERRIDDEN BY ANY MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys*
 ;;Movement Keys
 ("C-t"     . forward-paragraph)
 ("C-n"     . backward-paragraph)
 
 ("C-h"     . back-to-indentation)
 ("C-s"     . move-end-of-line)

 ("M-t"     . next-line)
 ("M-n"     . previous-line)

 ("M-s"     . forward-char)
 ("M-h"     . backward-char)
 ("M-b"     . backward-word)
 ("M-l"     . forward-word)

 ("M-c"     . avy-goto-char)
 ("M-r"     . avy-goto-word-1)

 ("C-S-s"   . isearch-forward)
 
 ("S-SPC"   . set-mark-command)
 ("M-<tab>" . switch-to-previous-buffer)
 ("C-e"     . mark-sexp)
 ("<f5>"    . toggle-truncate-lines)
 ("<f6>"    . linum-mode)
 ("M-o"     . vi-open-next-line)
 ("M-S-o"   . vi-open-previous-line)
 ("C-c l"   . org-store-link)
 ("C-c a"   . org-agenda)
 ("C-c c"   . org-capture)
 ("C-c o"   . org-open-main)
 ("C-c u"   . upcase-word)
 ("M-u"     . capitalize-word)
 ("C-/"     . undo)
 ("C-M-;"   . comment-region)
 ("C-x M-f" . recentf)
 ("C-x C-x" . kill-buffer-and-window)
 ("C-o"     . other-window)
 ("C-S-v"   . clipboard-yank)
 )

;; Rebind isearch mode map to conform to the same init sequence.
(define-key isearch-mode-map (kbd "C-S-s") 'isearch-repeat-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Bindings Which can be overridden by minor modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Navigate by sentence, but can be overridden by paredit

(global-set-key (kbd "C-M-t") 'forward-sentence)
(global-set-key (kbd "C-M-n") 'backward-sentence)

(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c i") 'ielm)
(global-set-key (kbd "M-.")  'xref-find-definitions)
(global-set-key (kbd "C-c .") 'org-time-stamp)
;; Unbind C-z from suspend-frame
(global-unset-key (kbd "C-z"))

