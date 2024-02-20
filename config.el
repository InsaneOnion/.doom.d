;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "onion"
      user-mail-address "2100300312@mails.guet.edu.cn"
      doom-font (font-spec :family "Iosevka SS09" :size 22)
      doom-symbol-font (font-spec :family "微软雅黑")
      doom-theme 'doom-plain-dark
      display-line-numbers-type t

      org-superstar-headline-bullets-list '("✿" "✸" "◉" "⁖" "○" )
      org-download-image-dir "./images"
      org-download-heading-lvl nil
      org-directory "~/.org/"
      org-preview-latex-image-directory "/tmp/ltximg/"
      org-ellipsis " ▼ "
      org-noter-notes-search-path '("~/.org/braindump/org/reference/")
      org-noter-doc-split-fraction '(0.55 0.45)

      conda-anaconda-home "~/.conda"
      conda-env-home-directory "~/.conda")

(setq zot-bib '("~/.org/braindump/org/biblio.bib")
      zot-pdf "~/zotfile"
      org-literature "~/.org/braindump/org/reference")

(custom-theme-set-faces! 'doom-plain-dark
  '(doom-dashboard-banner :foreground "gray" :weight bold))

(after! org
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "gtd/inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("s" "Slipbox" entry  (file "braindump/org/inbox.org")
           "* %?\n"))))

(after! org-download
  :custom
  (setq org-download-method 'directory))

(require 'org-ref)
(require 'org-ref-ivy)
(use-package! ivy-bibtex
  :init
  (setq bibtex-completion-notes-path org-literature
        bibtex-completion-bibliography zot-bib
        bibtex-completion-library-path zot-pdf))

(use-package! org-roam-bibtex
  :init
  (map! :leader
        :prefix "nr"
        :desc "insert an literature note link" "k" #'orb-insert-link
        :desc "some actions for orb-note" "A" #'orb-note-actions)
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-insert-interface 'ivy-bibtex)
  (orb-insert-link-description 'citekey)
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))

(use-package! org-roam
  :init
  (setq org-roam-directory (concat org-directory "braindump/org/")
        org-roam-db-location (concat org-roam-directory "db/org-roam.db")
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-database-connector 'sqlite-builtin)
  :config
  (org-roam-db-autosync-mode +1)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("l" "literature" plain
           "#+FILETAGS: reading research \n - tags :: %^{keywords} \n* %^{title}
:PROPERTIES:\n:Custom_ID: %^{citekey}\n:URL: %^{url}\n:AUTHOR: %^{author-or-editor}\n:NOTER_DOCUMENT: ~/zotfile/%^{citekey}.pdf\n:NOTER_PAGE:\n:END:"
           :target
           (file+head "reference/${citekey}.org" "#+title: ${title}\n"))
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t))
        )
  (defun onion/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'onion/tag-new-node-as-draft)
  )

(after! python
  (add-hook
   'python-mode-hook
   (lambda ()
     (setq-local python-indent-offset 2))))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-auto-character-face-perc 65))

(defun onion/get_current_line ()
  (buffer-substring
   (line-beginning-position)
   (line-end-position)))

(defun onion/extract-after-colon (str)
  (when (string-match "\\[\\[.*?:\\(.*?\\)\\]\\]$" str)
    (match-string 1 str)))

(defun onion/org-delete-img-and-imglink ()
  (interactive)
  (let* ((line (onion/get_current_line))
         (img-file-path (concat org-download-image-dir "/" (onion/extract-after-colon line))))
    (when (file-exists-p img-file-path)
      (delete-file img-file-path)
      (kill-whole-line))))

(map! "C-c i d" #'onion/org-delete-img-and-imglink)

(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window)

