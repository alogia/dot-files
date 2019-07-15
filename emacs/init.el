(require 'package)
(package-initialize)

;;; This file is the launcher for the emacs install. 

(defvar init-load-alist '(""))
(defvar init-config-directory '("config"))


;;; Pulled from xah's init-config. Use this function to load individual files.
(defun get-load-fullpath (@file-relative-path)
  "Use this function to load individual files.  Return the full path of @FILE-RELATIVE-PATH, relative to caller's file location.  @FILE-RELATIVE-PATH can be called from any config file, and will set the relative path correctly."
  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path))

(provide 'init)
;;; Init ends here
