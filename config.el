;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "onion"
      user-mail-address "2100300312@mails.guet.edu.cn"
      doom-font (font-spec :family "Iosevka SS09" :size 22)
      doom-theme 'doom-one
      display-line-numbers-type t

      org-directory "~/.org/"
      org-ellipsis " ▼ "
      org-download-method 'directory
      org-download-heading-lvl nil
      org-download-image-dir "./images")

;; conda settings
(setq conda-anaconda-home "~/.conda"
      conda-env-home-directory "~/.conda")

(setq org-preview-latex-image-directory "/tmp/ltximg/")

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
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))
  (defun onion/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'onion/tag-new-node-as-draft)
  )

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))

(after! python
  (add-hook
   'python-mode-hook
   (lambda ()
     (setq-local python-indent-offset 2))))

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
      (kill-whole-line)))
  )

(map! "C-c i d" #'onion/org-delete-img-and-imglink)

;; (map! :leader :desc "delete both the image and the image link in the cursor line" :n "C-d" #'onion/org-delete-img-and-imglink)
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
