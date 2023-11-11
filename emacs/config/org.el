;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode setup
;;
;;Setup org mode and related modes:
;;
;;Org-mode
;;Org-roam
;;
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
        ("t" "Todo" entry (file+headline "tasks.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("p" "Projects" entry (file ,(concat my:org-dir "projects.org"))
         "* %?%^G\n  :PROPERTIES:\n  :ENTERED_ON: %T\n  :END:\n%i\n")
        ("b" "Bookmarks" entry (file ,(concat my:org-dir "bookmarks.org"))
         "* [[%:link][%:description]]\n TAGS:%?%^G\n %i" :empty-lines 1)
        ))


(defun org-open-main ()
    "Opens the default org file from any buffer."
    (interactive)
    (find-file (concat
                (expand-file-name my:org-dir)
                my:org-default-file)))


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
;; Org-Roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package deft
  :bind ("<f8>" . deft)
  :commands deft
  :ensure t
  :init
  :custom
  ;; Set the default directory
  (deft-directory "~/org")
  (deft-extensions '("md" "txt" "tex" "org"))
  (deft-default-extension "org")
  ;; de-couples filename and note title:
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  ;; disable auto-save
  (deft-auto-save-interval -1.0)
  ;; converts the filter string into a readable file-name using kebab-case:
  (deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-")
      (case-fn . downcase)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-ref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-ref
  :config
  :custom
  (org-ref-completion-library 'org-ref-ivy-cite)
  (org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
  (org-ref-default-bibliography (list "/home/haozeke/GDrive/zotLib.bib"))
  (org-ref-bibliography-notes "/home/haozeke/Git/Gitlab/Mine/Notes/bibnotes.org")
  (org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n")
  (org-ref-notes-directory "/home/haozeke/Git/Gitlab/Mine/Notes/")
  (org-ref-notes-function 'orb-edit-notes)
  )
