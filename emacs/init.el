(require 'package)
(package-initialize)

;;; This file is the launcher for the emacs install. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 128MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 128000000)


;; Extra plugins and config files are stored here
(defvar init:plugin-dir  "~/.emacs.d/plugins")
(defvar init:load-alist '())
(defvar init:config-directory '("~/.emacs.d/config"))

;; Create the plugin directory and add it to load path if it doesn't already exist.
(make-directory (expand-file-name init:plugin-dir) :parents)
(add-to-list 'load-path (expand-file-name init:plugin-dir))

;; Create the config directory and add it to load path if it doesn't already exist.
(make-directory (expand-file-name init:config-dir) :parents)
(add-to-list 'load-path (expand-file-name init:config-dir))


;;; Pulled from xah's init-config. Use this function to load individual files.
(defun init-get-fullpath (@file-relative-path)
  "Use this function to load individual files.  Return the full path of @FILE-RELATIVE-PATH, relative to caller's file location.  @FILE-RELATIVE-PATH can be called from any config file, and will set the relative path correctly."
  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path))

(defun init-load-all ()
  "Load all files in INIT:LOAD-ALIST"
  (mapcar (lambda (init-file)
            (load (init-get-fullpath (init-file))))
          init:load-alist))

(provide 'init)
;;; Init ends here
