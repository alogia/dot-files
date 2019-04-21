;;; package --- Summary - Init file for alogia

;; Extra plugins and config files are stored here
(defvar my:plugin-dir  "~/.emacs.d/plugins")
;; Should emacs use a compiled init file?
(defvar my:compiled-init nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default directory for Org files
(defvar my:org-dir "~/org/")
;; Default file for Org mode to open with if started with C-c o
(defvar my:org-default-file "master.org")
;; Default file for org captures
(defvar my:org-default-notes "notes.org")
;; Defines targets files for refiling
(defvar my:org-refile-targets
     '(("notes.org"    .  (:maxlevel . 8)) ;; Default landing place for captures
       ("tasks.org"    .  (:maxlevel . 7)) ;; The TODO list
       ("projects.org" .  (:maxlevel . 3)) ;; Programming projects stuff
       ("ideas.org"    .  (:maxlevel . 5)) ;; Ideas for articles and such
       ("books.org"    .  (:maxlevel . 9)) ;; Notes on books
       ("master.org"   .  (:maxlevel . 9)) ;; A places for all the rest
       ("bookmarks.org" . (:maxlevel . 3)) ;; Bookmarks from web
       ))
;;Defines capture templates
(setq org-capture-templates
      `(
        ;; TODO  =============================  FIX ALL THESE TEMPLATES
        ("n" "Notes" entry (file ,(concat my:org-dir "notes.org"))
         "* %?%^G\n  :PROPERTIES:\n  :ENTERED_ON: %T\n  :END:\n%i\n")
        ("i" "Ideas" entry (file ,(concat my:org-dir "ideas.org"))
         "* %?%^G\n  :PROPERTIES:\n  :ENTERED_ON: %T\n  :END:\n%i\n" :empty-lines 1)
        ("t" "Todo" entry (file ,(concat my:org-dir "tasks.org"))
         "* %?%^G\n  :PROPERTIES:\n  :ENTERED_ON: %T\n  :END:\n%i\n")
        ("p" "Projects" entry (file ,(concat my:org-dir "projects.org"))
         "* %?%^G\n  :PROPERTIES:\n  :ENTERED_ON: %T\n  :END:\n%i\n")
        ("b" "Bookmarks" entry (file ,(concat my:org-dir "bookmarks.org"))
         "* [[%:link][%:description]]\n TAGS:%?%^G\n %i" :empty-lines 1)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler setups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compilation command for C/C++
(defvar my:compile-command "clang++ -Wall -Wextra -std=c++14 ")
(global-set-key (kbd "C-c C-c") 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 64000000)
;; (add-hook 'after-init-hook #'(lambda ()
;;                                ;; restore after startup
;;                                (setq gc-cons-threshold 800000)))


;; Create the plugin directory and add it to load path if it doesn't already exist.
(make-directory (expand-file-name my:plugin-dir) :parents)
(add-to-list 'load-path (expand-file-name my:plugin-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start emacs server if not already running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)
(scroll-bar-mode -1)

(defvar my-font-size 85)
;; Make mode bar small
(set-face-attribute 'mode-line nil  :height my-font-size)
;; Set the header bar font
(set-face-attribute 'header-line nil  :height my-font-size)
;; Turn off line numbers by default
(global-linum-mode -1)
;; Show empty lines at end of file
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
;; Show function in menu bar
(which-function-mode t)
;; Change tab key behavior to insert spaces instead
(setq-default indent-tabs-mode nil)
;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 2)
;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 2)
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; Use CUA to delete selections
(setq cua-mode t)
(setq cua-enable-cua-keys nil)
;; Don't ring the bell
(setq ring-bell-function 'ignore)
;; Disable the horrid auto-save
(setq auto-save-default nil)
;; Prevent emacs from creating a backup file filename~
(setq make-backup-files nil)
;; Set backup dir in case we want to enable it
(setq backup-directory-alist `(("." . "~/.saves")))
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)
(set-face-background 'hl-line "#372E2D")
;; Disable the toolbar at the top since it's useless
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
;; Disable the menu bar since we don't use it, especially not in the terminal
(menu-bar-mode -1)
;; Auto-wrap at 80 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
;; Non-nil means draw block cursor as wide as the glyph under it.
(setq x-stretch-cursor t)
;; Don't ask to follow symlink in git
(setq vc-follow-symlinks t)
;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
(turn-on-auto-fill)
;; set 'term default shell
(setq-default explicit-shell-file-name "/usr/bin/zsh")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Some default hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove trailing white space upon saving
(add-hook 'before-save-hook #'(delete-trailing-whitespace))

;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Check (on save) whether the file edited contains a shebang, if yes, make it
;; executable.
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     General Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-open-main ()
    "Opens the default org file from any buffer."
    (interactive)
    (find-file (concat
                (expand-file-name my:org-dir)
                my:org-default-file)))

;; Behave like vi's o command
(defun vi-open-next-line (arg)
  "Move to the next line and then open ARG new lines.  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun vi-open-previous-line (arg)
  "Open ARG new lines before the current one.  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; Start default term/shell
(defun start-default-term ()
  (interactive) (term explicit-shell-file-name))

(define-key global-map (kbd "C-c t") #'start-default-term)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save ~/.emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-compile-init-files (file)
  "Automatically compile FILE."
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
    (byte-compile-file (expand-file-name file)))
  )


(add-hook
 'after-save-hook
 (function
  (lambda ()
    (if (string= (file-truename "~/.emacs.el")
                 (file-truename (buffer-file-name)))
        (if my:compiled-init
            (progn (byte-compile-init-files (file-truename "~/.emacs.el"))
                   (load-file (file-truename "-/.emacs.elc")))
          (load-file (file-truename "~/.emacs.el")))
      )
    )
  )
 )


;; Byte-compile again to ~/.emacs.elc if it is outdated
(if my:compiled-init
    (if (file-newer-than-file-p
         (file-truename "~/.emacs.el")
         (file-truename "~/.emacs.elc"))
        (byte-compile-init-files "~/.emacs.el")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'use-package))
(use-package bind-key
  :ensure t)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package
  :commands use-package-autoload-keymap)
;; Auto update packages once a week
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now")))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Bindings regardless of any mode
;; ONLY USE FOR BINDINGS WHICH SHOULD NEVER BE OVERRIDDEN BY ANY MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys*
 ("C-t"     . forward-paragraph)
 ("C-n"     . backward-paragraph)
 ("C-h"     . back-to-indentation)
 ("M-t"     . next-line)
 ("M-n"     . previous-line)
 ("M-s"     . forward-char)
 ("M-h"     . backward-char)
 ("M-b"     . backward-word)
 ("M-l"     . forward-word)
 ("C-s"     . move-end-of-line)
 ("S-SPC"   . set-mark-command)
 ("C-e"     . mark-sexp)
 ("<f6>"    . linum-mode)
 ("C-o"     .  vi-open-previous-line)
 ("M-o"     .  vi-open-next-line)
 ("C-c l"   . org-store-link)
 ("C-c a"   . org-agenda)
 ("C-c c"   . org-capture)
 ("C-c o"   . org-open-main)
 ("C-/"     . undo)
 ("C-x M-f" . project-find-file)
 ("C-c i"   . ielm))

;; Unbind C-z from suspend-frame
(global-unset-key (kbd "C-z"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    GENERAL PACKAGES                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; async - library for async/thread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s is used by origami, etc and sometimes during Emacs
;; upgrades disappears so we try to install it on its own.
;; s is an emacs general string manipulation library.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :commands (ivy-mode)
  :config
  (require 'ivy)
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-wrap t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; Show #/total when scrolling buffers
  (setq ivy-count-format "%d/%d ")
  )

(use-package swiper
  :ensure t
  :bind (("C--" . swiper))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package define-word
  :ensure t
  :bind
  ("C-c d" . define-word-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Slime mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :ensure t
  :custom
  (inferior-lisp-program "/usr/bin/sbcl")
  :hook
  (lisp-mode . slime-mode)
  (inferior-lisp-mode . inferior-slime-mode)
  :config
  (use-package slime-company
    :ensure t
    :custom
    (slime-contribs '(slime-fancy slime-company))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Dired extensions -- all packages require dash
;; https://github.com/Fuco1/dired-hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dash
  :ensure t
  :config
  (eval-after-load 'dash '(dash-enable-font-lock)))

(use-package dired-rainbow
  :ensure t
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    )) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Counsel mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git-grep)
         ("C-c j" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-c r" . counsel-rg)
         ("C-x l" . counsel-locate)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add)
         )
  :config
  (if (executable-find "rg")
      ;; use ripgrep instead of grep because it's way faster
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s ."
            )
    (warn "\nWARNING: Could not find the ripgrep executable. It "
          "is recommended you install ripgrep.")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Counsel-etags support with ctags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.

;; TODO --- Clean up this use-package call
(use-package counsel-etags
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
    (declare-function counsel-etags-guess-program "counsel-etags.el")
    (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
  :bind (
         ("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ("M-s" . counsel-etags-find-tag))
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  ;; Set up auto-update
  (add-hook
   'prog-mode-hook
   (lambda () (add-hook 'after-save-hook
                        (lambda ()
                          (counsel-etags-virtual-update-tags))))
   )

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  (defun my-scan-dir (src-dir &optional force)
    "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
    (let* ((find-pg (or
                     counsel-etags-find-program
                     (counsel-etags-guess-program "find")))
           (ctags-pg (or
                      counsel-etags-tags-program
                      (format "%s -e -L" (counsel-etags-guess-program
                                          "ctags"))))
           (default-directory src-dir)
           ;; run find&ctags to create TAGS
           (cmd (format
                 "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
                 find-pg
                 (mapconcat
                  (lambda (p)
                    (format "-iwholename \"*%s*\"" p))
                  counsel-etags-ignore-directories " -or ")
                 counsel-etags-max-file-size
                 (mapconcat (lambda (n)
                              (format "-not -name \"%s\"" n))
                            counsel-etags-ignore-filenames " ")
                 ctags-pg))
           (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
           (doit (or force (not (file-exists-p tags-file)))))
      ;; always update cli options
      (when doit
        (message "%s at %s" cmd default-directory)
        (shell-command cmd)
        (visit-tags-table tags-file t)
        )
      )
    )

  (setq counsel-etags-update-tags-backend
        (lambda ()
          (interactive)
          (let* ((tags-file (counsel-etags-locate-tags-file)))
            (when tags-file
              (my-scan-dir (file-name-directory tags-file) t)
              (run-hook-with-args
               'counsel-etags-after-update-tags-hook tags-file)
              (unless counsel-etags-quiet-when-updating-tags
                (message "%s is updated!" tags-file))))
          )
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
(use-package window-numbering
  :ensure t
  :config
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function window-numbering-mode "window-numbering.el"))
  (window-numbering-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit server to allow editing of things in Chrome with Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package edit-server
  :ensure t
  :config
  (progn
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function edit-server-start "edit-server-start.el"))
    (when (daemonp)
      (edit-server-start)
      )
    (add-hook 'edit-server-start-hook
              (lambda ()
                (when (string-match "github.com" (buffer-name))
                  (markdown-mode))))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EasyPG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package epa
  :ensure t
  :custom
  (epg-gpg-program "gpg2")
  (epg-file-select-keys t)
  :bind
  ("C-c e" . epa-encrypt-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up mu4e with gmail account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mu4e
  ;; This package should be installed with AUR mu program
  ;; There is no Melpa package under this name, so don't :ensure it.
  :custom
  (mu4e-maildir (expand-file-name "~/Maildir"))
  (mu4e-drafts-folder "/Drafts")
  (mu4e-sent-folder   "/Sent Mail")
  (mu4e-trash-folder  "/Trash")
  ;; don't save message to Sent Messages, GMail/IMAP will take care of this
  (mu4e-sent-messages-behavior 'delete)
  ;; setup some handy shortcuts
  (mu4e-maildir-shortcuts
        '(("/INBOX"     . ?i)
          ("/Sent Mail" . ?s)
          ("/Trash"     . ?t)))
  ;; allow for updating mail using 'U' in the main view:
  (mu4e-get-mail-command "offlineimap")
    (user-mail-address "alogia@gmail.com")
  (user-full-name  "Tyler Thomas")
   ;; message-signature
   ;;  (concat
   ;;    "Foo X. Bar\n"
   ;;    "http://www.example.com\n")    

  ;;Must have the gnutls package installed
  :config
  (use-package smtpmail
    :ensure t
    :custom
    (message-send-mail-function 'smtpmail-send-it)
    (starttls-use-gnutls t)
    (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
    (smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
    (smtpmail-default-smtp-server "smtp.gmail.com")
    (smtpmail-smtp-server "smtp.gmail.com")
    (smtpmail-smtp-service 587)
    (smtpmail-debug-info t))
  (use-package mu4e-alert
  :ensure t
  :custom
  (mu4e-alert-interesting-mail-query  (concat
       "flag:unread"
       " AND NOT flag:trashed"
       " AND NOT maildir:"
       "\"All Mail\""))
  :hook
  (after-init . mu4e-alert-enable-mode-line-display)
  :config
  (defun gjstein-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display)
    )
  (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function which-key-mode "which-key.el"))
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: always fast jump to char inside the current view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1))
  ;; Set keys for Dvorak mode instead of qwerty
  :init (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
                            ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
                            ?p ?y ?f ?g ?c ?r ?l
                            ?P ?Y ?F ?G ?C ?R ?L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zzz-to-char: replaces the built-in zap-to-char with avy-like
;;              replacement options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zzz-to-char
  :ensure t
  :bind ("M-z" . zzz-up-to-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RealGud - https://github.com/realgud/realgud
;; A rewrite of GUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package realgud
  :ensure t
  :init
  (setenv "TERM" "dumb")
  :custom
  (realgud:pdb-command-name "python -m pdb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default pdb-command-name "python -m pdb")
  :custom
  (python-indent 4)
  (python-indent-offset 4)
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  :hook
  (python-mode . (lambda ()
                   (setq tab-width 4)))
  :config
  (bind-key "C-n" 'comint-previous-input inferior-python-mode-map)
  (bind-key "C-t" 'comint-next-input inferior-python-mode-map)
 ; ("M-." . xref-find-definitions)
  )


(use-package pyenv-mode
  :ensure t
  :config
  (use-package pyenv-mode-auto
    :ensure t)
  )


(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :after python
  :custom
  (elpy-rpc-backend "jedi")
  (elpy-use-ipython "ipython")
  :config
  (elpy-enable)
  :hook
  (python-mode . elpy-mode)
  :bind
  ("M-." . elpy-goto-definition)
  ("M-," . pop-tag-mark))

;; (use-package yapfify
;;   :ensure t
;;   :hook
;;   (python-mode . yapf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
  :ensure t
  :bind ("C-c C-f" . clang-format-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern C++ code highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modern-cpp-font-lock
  :ensure t
  :init
  (eval-when-compile
      ;; Silence missing function warnings
    (declare-function modern-c++-font-lock-global-mode
                      "modern-cpp-font-lock.el"))
  :config
  (modern-c++-font-lock-global-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  :bind
  (:map c++-mode-map
        ("C-c C-k" . kill-compilation))
  :config
  (setq compile-command my:compile-command)
  (use-package google-c-style
    :ensure t
    :hook
    ;; This prevents the extra two spaces in a namespace that Emacs
    ;; otherwise wants to put... Gawd!
    (c-mode-common . google-set-c-style)
    ;; Autoindent using google style guide
    (c-mode-common . google-make-newline-indent)
    )
  )

(use-package cpputils-cmake
  :if (executable-find "cmake")
  :ensure t
  :config
  ;; TODO -- Configure this further
  )


;; We want to be able to see if there is a tab character vs a space.
;; global-whitespace-mode allows us to do just that.
;; Set whitespace mode to only show tabs, not newlines/spaces.
(use-package whitespace
  :ensure t
  :init
  (eval-when-compile
      ;; Silence missing function warnings
      (declare-function global-whitespace-mode "whitespace.el"))
  :custom
  (whitespace-style '(tabs tab-mark))
  :config
  ;; Turn on whitespace mode globally.
  (global-whitespace-mode t)
  )

;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :bind
  (:map company-active-map
        ("M-t" . (lambda () (interactive) (company-complete-common-or-cycle 1)))
        ("M-n" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
        )
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.25)
  (company-selection-wrap-around t)
  :config
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  ;; Setup company to cycle with tab
  ;; (use-package company-tng
  ;;   :bind
  ;;   (:map company-active-map
	;;         ("<M-tab>" . company-complete-common-or-cycle))
  ;;   :config
  ;;   (company-tng-configure-default))
  (if window-system
	    (custom-set-faces
	     '(company-preview
	       ((t (:foreground "darkgray" :underline t))))
	     '(company-preview-common
	       ((t (:inherit company-preview))))
	     '(company-tooltip
	   ((t (:background "lightgray" :foreground "black"))))
	     '(company-tooltip-selection
	       ((t (:background "steelblue" :foreground "white"))))
	     '(company-tooltip-common
	       ((((type x)) (:inherit company-tooltip :weight bold))
		      (t (:inherit company-tooltip))))
	     '(company-tooltip-common-selection
	       ((((type x)) (:inherit company-tooltip-selection :weight bold))
		      (t (:inherit company-tooltip-selection))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Company Tern backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company-tern
  :ensure t
  :hook
  (js2-mode .
            (lambda ()
              (add-to-list 'company-backends 'company-tern)
              (tern-mode)))
  :bind
  (:map tern-mode-keymap
        ;; Disable completion keybindings, as we use xref-js2 instead
        ("M-." . nil)
        ("M-," . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Indium setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package indium
  :ensure t
)


;; Setup loading company-jedi for python completion
;; This requines running jedi:install-server the first time
;; (use-package company-jedi
;;   :ensure t
;;   :after python
;;   :init
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;;   :hook
;;   (python-mode . jedi:setup)
;;   ;; :custom
;;   ;; (jedi:setup-keys t)
;;   ;; (jedi:complete-on-dot t)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :ensure t
  :defer t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )
(use-package flycheck-pyflakes
  :ensure t
  :after python)

(use-package google-translate
  :ensure t
  :bind
  ("C-c C-t" . google-translate-at-point)
  ("C-c C-T" . google-translate-at-point-reverse)
  :custom
  (google-translate-show-phonetic t)
  (google-translate-default-target-language "en")
  (google-translate-default-source-language "zh-CN"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-inflection
;; used for switching between different cases, eg CamelCase,
;; lowerCamelCase, snake_case, and SCREAMING_SNAKE_CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package string-inflection
  :ensure t
  :defer t
  :bind (("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :init
  (require 'org-protocol)
  (require 'org-capture)
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory (expand-file-name my:org-dir))
  (org-default-notes-file (concat org-directory my:org-default-notes))
  (org-agenda-files (list org-directory))
  (org-log-done 'time)
  (org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE")))
  (org-todo-keyword-faces '(("INPROGRESS" . (:foreground "red" :weight bold))))
  (org-refile-targets (mapcar (lambda (tg) (cons (concat (expand-file-name my:org-dir) (car tg)) (cdr tg))) my:org-refile-targets))
  :config
  (use-package org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode))
  (use-package writegood-mode
    :ensure t
    :hook
    (org-mode . writegood-mode)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically at closing brace, bracket and quote
(use-package autopair
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function autopair-global-mode "autopair.el"))
  :config
  (autopair-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode slime-repl-mode)
   . enable-paredit-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load hungry Delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set hungry delete:
(use-package hungry-delete
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-hungry-delete-mode "hungry-delete.el"))
  :config
  (global-hungry-delete-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting in CUDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell Mode for Spelling Corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function flyspell-goto-next-error "flyspell.el")
    (declare-function flyspell-mode "flyspell.el")
    (declare-function flyspell-prog-mode "flyspell.el"))
  (setq flyspell-issue-welcome-flag nil)
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  :bind
  (("<f7>"  . flyspell-mode)
   ("<f8>"  . flyspell-auto-correct-word))
  :hook
  ((text-mode LaTeX-mode org-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  ((LaTeX-mode text-mode) . flyspell-buffer)
  )

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :requires dash
  :after (ivy)
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :config
  (add-hook 'magit-mode-hook (lambda () (setq whitespace-mode -1)))
  (setq magit-completing-read-function 'ivy-completing-read)
  )
(use-package magit-gerrit
  :ensure t
  :after magit
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GitGutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-gutter
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-git-gutter-mode "git-gutter.el"))
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; Auto update every 5 seconds
  (custom-set-variables
   '(git-gutter:update-interval 5))

  ;; Set the foreground color of modified lines to something obvious
  (set-face-foreground 'git-gutter:modified "purple")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (add-to-list 'company-backends 'company-cmake)))
  :config
  (use-package cmake-font-lock
    :ensure t
    :defer t
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                  1 font-lock-warning-face t)))
                          ))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/google/protobuf/master/editors/protobuf-mode.el"
     "~/.emacs.d/plugins/protobuf-mode.el"))
(if (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el")
    (use-package protobuf-mode
      :mode ("\\.proto")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode (".json" ".imp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode and additions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("javascript" . js2-mode)
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :config
  (use-package js2-refactor
    :ensure t
    :init
    (declare-function js2r-add-keybindings-with-prefix "js2-refactor.el")
    :hook
    (js2-mode . js2-refactor-mode)
    :bind
    (:map js2-mode-map
          ("C-k" . js2r-kill)
          ("M-." . xref-find-definitions))
    (:map js-mode-map
          ("M-." . nil)) ;; Unbind js-mode key
    :config
    (js2r-add-keybindings-with-prefix "C-c C-r"))
  (use-package xref-js2
    :ensure t
    :init
    :hook
    (js2-mode . (lambda ()
                  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
    (js2-mode-hook . js2-refactor-mode)
    :bind
    (:map js2-mode-map
          ("C-k" . js2r-kill))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :defer 5
  :config
  (yas-global-mode t)
  (yas-reload-all))
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))
;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
  :mode ("\\.s\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :mode (".md" ".markdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
    ((java-mode c-mode c++-mode) . lsp)
  :config
  (require 'lsp-clients)
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :config
    (push 'company-lsp company-backends))
;  (use-package lsp-ui
;    :ensure t
;    :commands lsp-ui-mode
;    :hook
;    (lsp-mode . lsp-ui-mode)
;    :config
;    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; When we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :config
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-source-correlate-start-server t
                Tex-view-program-list '(("Evince" "evince --page-index=%(outpage) %o"))
                TeX-view-program-selection '(output-pdf "Evince")
                reftex-plug-into-AUCTeX t)
  :hook
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . auto-fill-mode)
  )
  


;; Remove function from mode bar
(setq mode-line-misc-info
      (delete (assoc 'which-func-mode
                     mode-line-misc-info) mode-line-misc-info))


(defmacro with-face
    (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  "."
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]")
         )
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat
           (with-face sl/header
                      ;; :background "red"
                      :foreground "red"
                      :weight 'bold)))
      (concat (if window-system ;; In the terminal the green is hard to read
                  (with-face sl/header
                             ;; :background "green"
                             ;; :foreground "black"
                             :weight 'bold
                             :foreground "#8fb28f"
                             )
                (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "blue"
                           ))
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  "Create the header string and display it."
  ;; The dark blue in the header for which-func is terrible to read.
  ;; However, in the terminal it's quite nice
  (if window-system
      (custom-set-faces
       '(which-func ((t (:foreground "#8fb28f")))))
    (custom-set-faces
     '(which-func ((t (:foreground "blue"))))))
  ;; Set the header line
  (setq header-line-format

        (list "-"
              '(which-func-mode ("" which-func-format))
              '("" ;; invocation-name
                (:eval (if (buffer-file-name)
                           (concat "[" (sl/make-header) "]")
                         "[%b]")))
              )
        )
  )
;; Call the header line update
(add-hook 'buffer-list-update-hook
          'sl/display-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline theme where the modes are on the right side.
(use-package powerline
  :ensure t
  :config
  (defun powerline-right-theme ()
    "Setup a mode-line with major and minor modes on the right side."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (powerline-buffer-size face0 'l)
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (powerline-raw " ")
                                       (funcall separator-left face0 face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-vc face1)))
                            (center (list (powerline-raw global-mode-string face1 'r)
                                          (powerline-raw "%4l" face1 'r)
                                          (powerline-raw ":" face1)
                                          (powerline-raw "%3c" face1 'r)
                                          (funcall separator-right face1 face0)
                                          (powerline-raw " ")
                                          (powerline-raw "%6p" face0 'r)
                                          (powerline-hud face2 face1)
                                          ))
                            (rhs (list (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face2 'l))
                                       (powerline-major-mode face2 'l)
                                       (powerline-process face2)
                                       (powerline-raw " :" face2)
                                       (powerline-minor-modes face2 'l)
                                       (powerline-raw " " face2)
                                       (funcall separator-right face2 face1)
                                       ))
                            )
                       (concat (powerline-render lhs)
                               (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                               (powerline-render center)
                               (powerline-fill face1 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (powerline-right-theme)
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Mono for Powerline" :foundry "GOOG" :slant normal :weight normal :height 85 :width normal))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(which-func ((t (:foreground "#8fb28f")))))

(provide '.emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 5)
 '(package-selected-packages
   (quote
    (define-word mu4e-alert mu4e dired-rainbow pyenv-mode-auto pyenv-mode slime-company org-plus-contrib zzz-to-char yasnippet-snippets yapfify yaml-mode xref-js2 writegood-mode window-numbering which-key wgrep web-mode vlf use-package tree-mode string-inflection slime request-deferred realgud rainbow-delimiters powerline paredit origami org-bullets modern-cpp-font-lock markdown-mode magit-gerrit json-mode indium hungry-delete google-translate google-c-style git-gutter flyspell-correct-ivy flycheck-pyflakes elpy ein edit-server cuda-mode cpputils-cmake counsel-etags company-tern company-lsp cmake-font-lock clang-format bui beacon autopair auto-package-update auctex 0blayout))))
