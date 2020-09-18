;;;;emacs init file -- Sets up loading and compile code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define setup dirs and load init file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 64000000)

;;Folder which contains all additional config files
(defvar my:config "~/.emacs.d/config/")
;;Folder for config elc files
(defvar my:compiled "~/.emacs.d/compiled")
;; Bin folder for linking to external apps
(defvar my:bin "~/.emacs.d/bin")
;; Extra plugins and config files are stored here
(defvar my:plugin-dir  "~/.emacs.d/plugins")
;; Should emacs use a compiled init file?
(defvar my:compiled-init t)


;;; Define default symlinks to use in my:bin folder
(defvar my:browser "browser")
(defvar my:shell "shell")
(defvar my:lisp "lisp")

;; Create the plugin directory and add it to load path if it doesn't already exist.
(make-directory (expand-file-name my:plugin-dir) :parents)
(add-to-list 'load-path (expand-file-name my:plugin-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun full-command-path (name)
  "takes a symlink name and produces a full path."
  (concat
   (file-truename my:bin)
   "/"
   name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-config-sources ()
  "Return all config files in my:config."
  (delete "." (delete ".." (directory-files (expand-file-name my:config) nil "\\.el$"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-config-compiled ()
  "Return all compiled elc files in my:compiled."
  (delete "." (delete ".." (directory-files (expand-file-name my:compiled) nil "\\.elc$"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun compiled-config-exists? (file)
;;   "If compiled FILE exists, return the filename otherwise nil."
;;   (let ((compiled-files (get-config-compiled))
;;         (path (file-truename (concat my:config "/" file)))
;;         (has-compiled? (lambda (rem)
;;                          (if (string= (file-name-sans-extension file)
;;                                       (file-name-sans-extension (car rem)))
;;                              file
;;                            (if rem
;;                                (funcall has-compiled? (cdr rem))
;;                              nil)))))
;;     (if (and compiled-files (file-exists-p path))
;;         (funcall has-compiled? compiled-files)
;;       nil))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-exists? (file)
  "If config file exists, return T otherwise nil."
  (file-exists-p (file-truename (concat my:config "/" file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-compiled-exists? (file)
  "If compiled FILE exists, return T otherwise nil."
  (file-exists-p (file-truename (concat my:compiled "/"
                                        (file-name-sans-extension file) ".elc"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-compile-update? (file)
  "Return FILE if config FILE needs to be updated, otherwise NIL."
  (if (and (config-compiled-exists? file)
           (config-exists? file)
           (file-newer-than-file-p
            (file-truename (concat my:config   "/" file))
            (file-truename (concat my:compiled "/" file "c"))))
      t
    nil)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-compile-file (file)
  "Compile config FILE and move it to the my:compiled directory."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete))
      (setq byte-compile-warnings
            '(unresolved
              callargs
              redefine
              obsolete
              noruntime
              cl-warnings
              interactive-only)))
    (let ((f (expand-file-name (concat my:config "/" file))))
      (byte-compile-file f)
      (rename-file (concat f "c") (concat my:compiled "/") t)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-update-compiled ()
    "Check if my:compiled are out of date and recompile."
  (mapc (lambda (file)
          (if (config-compile-update? file)
              (config-compile-file file)))
        (get-config-sources)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save config files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar config-init-names '("~/.emacs" "~/.emacs.el" "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-init-save-load ()
  "Compile and load all init buffers."
  (mapc (lambda (f)
          (if (string= (file-truename f)
                       (file-truename (buffer-file-name)))
              (if my:compiled-init
                  (progn (byte-compile-file (expand-file-name f))
                         (load-file (expand-file-name (concat f "c"))))
                (load-file (expand-file-name f)))
            )) config-init-names)
  )
(add-hook 'after-save-hook 'config-init-save-load)


;;Load main init file
(load-file (expand-file-name (concat my:config "/" "init.el")))
