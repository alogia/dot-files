;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define setup dirs and load init file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Folder which contains all additional config files
(defvar my:config "~/.emacs.d/config/")
;;Folder for config elc files
(defvar my:compiled "~/.emacs.d/compiled")
;; Bin folder for linking to external apps
(defvar my:bin "~/.emacs.d/bin")
;; Extra plugins and config files are stored here
(defvar my:plugin-dir  "~/.emacs.d/plugins")
;; Should emacs use a compiled init file?
(defvar my:compiled-init nil)


;;Load main init file
(load-file (expand-file-name (concat my:config "/" "init.el")))
