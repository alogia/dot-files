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
(defun config-load (file)
  "Loads either FILE or compiled."
  (if (and my:compiled-init
           (config-compiled-exists? file))
      (progn
        (if (config-compile-update? file)
            (config-compile-file file))
        (load-file (expand-file-name (concat my:compiled "/"
                                             (file-name-sans-extension file) ".elc"))))
    (load-file (expand-file-name (concat my:config "/" file))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-directory (dir)
  "Load all files from DIR excluding the current buffer. Excludes current buffer
in case of loading an old elc file and replacing the current file."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (remove (buffer-file-name) 
                          (remove (concat (buffer-file-name) "c")
                                  (directory-files dir nil "\\.el[c]?$"))))))




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
                         (load-file (expand-file-name (concat (file-name-sans-extension f) ".elc"))))
                (load-file (expand-file-name f)))
            )) config-init-names)
  )
;; Add hook to save function. 
(add-hook 'after-save-hook 'config-init-save-load)


;;Load main init file
(config-load "init.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(git-gutter:update-interval 5)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(which-func ((t (:foreground "blue")))))
