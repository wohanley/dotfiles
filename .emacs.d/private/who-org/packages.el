;;; packages.el --- who-org layer packages file for Spacemacs.
;;
;; org-mode stuff, much from https://github.com/jethrokuan/.emacs.d
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <wohanley@pigeon>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `who-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `who-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `who-org/pre-init-PACKAGE' and/or
;;   `who-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst who-org-packages
  '(calfw calfw-org deft org org-agenda org-clock-convenience org-download org-emms org-gcal org-noter org-pdftools org-roam org-wild-notifier))

(defun who-org/init-calfw ()
  "Initialize calfw and add key-bindings"
  (use-package calfw
    :commands (cfw:open-calendar-buffer)
    :init
    (spacemacs/set-leader-keys "aCd" 'cfw:open-calendar-buffer)
    :config
    (progn
      (setq calendar-week-start-day 1) ;; start week on Monday
      (define-key cfw:calendar-mode-map (kbd "TAB") 'cfw:show-details-command)
      (define-key cfw:calendar-mode-map (kbd "C-j") 'cfw:navi-next-item-command)
      (define-key cfw:calendar-mode-map (kbd "C-k") 'cfw:navi-prev-item-command))))

(defun who-org/init-calfw-org ()
  "Initialize calfw-org and add key-bindings"
  (use-package calfw-org
    :commands (cfw:open-org-calendar)
    :init
    (spacemacs/set-leader-keys "aoCd" 'cfw:open-org-calendar)
    :config
    (progn
      (define-key cfw:org-schedule-map (kbd "TAB") 'cfw:org-open-agenda-day)
      (define-key cfw:org-custom-map (kbd "SPC") 'spacemacs-cmds)
      (define-key cfw:org-custom-map (kbd "TAB") 'cfw:org-open-agenda-day))))

(defun who-org/post-init-deft ()
  (setq deft-recursive 1)
  (setq deft-use-filename-as-title t)
  (setq deft-default-extension "org")
  (setq deft-directory "~/org/zettelkasten"))

(defun who-org/post-init-org ()
  (require 'org-habit)
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-protocol)

  (require 'org-contacts)
  (add-to-list 'org-modules 'org-contacts)
  (setq org-contacts-files (notdeft-list-files-by-query "!all tag:person"))

  (require 'org-id)
  (setq org-id-link-to-org-use-id t)

  (setq org-startup-folded 'showall)
  (setq org-list-indent-offset 2)
  (setq org-return-follows-link t)
  (setq org-link-frame-setup '((file . find-file))) ;; follow links in same window
  (setq org-agenda-diary-file "~/org/diary.org")
  (setq org-catch-invisible-edits 'show)
  (setq org-habit-preceding-days 21)
  (setq org-habit-following-days 7)

  (defun who/style-org ()
    (setq line-spacing 0.2)
    (variable-pitch-mode +1)
    (mapc
     (lambda (face) ;; Other fonts with fixed-pitch.
       (set-face-attribute face nil :inherit 'fixed-pitch))
     (list 'org-code
           'org-block
           'org-table
           'org-verbatim
           'org-block-begin-line
           'org-block-end-line
           'org-meta-line
           'org-document-info-keyword)))

  ;; (add-hook 'org-mode-hook #'who/style-org))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ir" 'who/org-insert-link-to-latest))

(defun who-org/find-projects ()
  (notdeft-list-files-by-query "!all tag:project AND NOT tag:archive"))

(defun who-org/find-areas-of-responsibility ()
  (notdeft-list-files-by-query "!all tag:AoR AND NOT tag:archive"))

(defun who-org/post-init-org-agenda ()
  (setq who/org-agenda-directory "~/org/gtd/")

  (require 'find-lisp)
  (defun who/find-org-files (directory)
    (find-lisp-find-files directory "\.org$"))

  (setq org-agenda-files (append (who/find-org-files who/org-agenda-directory)
                                 (who-org/find-projects)))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil)

  ;;;
  ;; capture
  ;;;

  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat who/org-agenda-directory "inbox.org"))
           "* TODO %?")
          ("a" "appointment" entry (file "~/org/gtd/calendars/personal.org" ))
          ("e" "email" entry (file+headline ,(concat who/org-agenda-directory "inbox.org") "Emails")
           "* TODO [#B] %a" :immediate-finish t)
          ("l" "link" entry (file ,(concat who/org-agenda-directory "inbox.org"))
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file ,(concat who/org-agenda-directory "inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))

  ;;;
  ;; org-agenda
  ;;;

  (setq org-agenda-block-separator nil)
  (setq org-agenda-start-with-log-mode t)
  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

  ;; https://github.com/syl20bnr/spacemacs/issues/3094
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("next.org" :level . 0)
                             ("someday.org" :level . 0)
                             ("reading.org" :level . 1)
                             (who-org/find-projects :level . 0)
                             (who-org/find-areas-of-responsibility :level . 0)))

  (defvar who/org-current-effort "1:00" "Current effort for agenda items.")

  ;;;
  ;; reading view
  ;;;

  (setq who/org-agenda-reading-view
        `("r" "Reading" todo ""
          ((org-agenda-files '(,(concat who/org-agenda-directory "reading.org"))))))

  (add-to-list 'org-agenda-custom-commands `,who/org-agenda-reading-view)

  ;;;
  ;; todo view
  ;;;

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (defun who/set-todo-state-next ()
    "Visit each parent task and change NEXT states to TODO"
    (org-todo "NEXT"))

  (setq who/org-agenda-todo-view
        `(" " "Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 30)))
           (todo "NEXT"
                 ((org-agenda-overriding-header "In Progress")
                  (org-agenda-files (append '(,(concat who/org-agenda-directory "someday.org")
                                              ,(concat who/org-agenda-directory "next.org"))
                                            (who-org/find-projects)))))
           (todo "TODO"
                 ((org-agenda-overriding-header "To Refile")
                  (org-agenda-files '(,(concat who/org-agenda-directory "inbox.org")))))
           (todo "TODO"
                 ((org-agenda-overriding-header "Backlog")
                  (org-agenda-files (append '(,(concat who/org-agenda-directory "next.org"))
                                            (who-org/find-projects)))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
           (todo "HOLD|WAITING"
                 ((org-agenda-overriding-header "Blocked")))
           (todo "TODO"
                 ((org-agenda-overriding-header "Someday")
                  (org-agenda-files '(,(concat who/org-agenda-directory "someday.org")))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
           nil))

  (add-to-list 'org-agenda-custom-commands `,who/org-agenda-todo-view)

  ;;;
  ;; org-journal
  ;;;

  (setq org-journal-dir "~/org/zettelkasten")
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "journal-%Y-%m-%d.org")
  (setq org-journal-date-format "%Y-%m-%d")

  ;;;
  ;; bindings
  ;;;

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
              (define-key org-agenda-mode-map "o" 'org-agenda-clock-out)
              (define-key org-agenda-mode-map "s" 'who/org-agenda-process-inbox-item)
              (define-key org-agenda-mode-map "r" 'who/org-process-inbox)
              (define-key org-agenda-mode-map "R" 'org-agenda-refile)))

  (bind-key "<f1>" 'who/switch-to-agenda)

  (defvar who/org-agenda-bulk-process-key ?f
    "Default key for bulk processing inbox items.")
  (setq org-agenda-bulk-custom-functions `((,who/org-agenda-bulk-process-key who/org-agenda-process-inbox-item)))

  (add-hook 'org-clock-in-hook 'who/set-todo-state-next 'append))

(defun who-org/init-org-clock-convenience ()
  (use-package org-clock-convenience
    :defer t
    :bind (:map org-agenda-mode-map
               ("<S-up>" . org-clock-convenience-timestamp-up)
               ("<S-down>" . org-clock-convenience-timestamp-down)
               ("o" . org-clock-convenience-fill-gap)
               ("e" . org-clock-convenience-fill-gap-both))))

(defun who-org/post-init-org-download ()
  (use-package org-download
    :defer t
    :after org
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "s-Y" 'org-download-screenshot
      "s-y" 'org-download-yank)
    :config
    (if (memq window-system '(mac ns))
        (setq org-download-screenshot-method "screencapture -i %s")
      (setq org-download-screenshot-method "maim -s %s"))

    (defun my-org-download-method (link)
      "This is a helper function for org-download.
It creates a folder in the root directory (~/.org/img/) named after the
org filename (sans extension) and puts all images from that file in there.
Inspired by https://github.com/daviderestivo/emacs-config/blob/6086a7013020e19c0bc532770e9533b4fc549438/init.el#L701"
      (let ((filename
             (file-name-nondirectory
              (car (url-path-and-query
                    (url-generic-parse-url link)))))
            ;; Create folder name with current buffer name, and place in root dir
            (dirname (concat "~/org/download/"
                             (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name))))))

        ;; Add timestamp to filename
        (setq filename-with-timestamp (format "%s%s.%s"
                                              (file-name-sans-extension filename)
                                              (format-time-string org-download-timestamp)
                                              (file-name-extension filename)))
        ;; Create folder if necessary
        (unless (file-exists-p dirname)
          (make-directory dirname))
        (expand-file-name filename-with-timestamp dirname)))
    (setq org-download-method 'my-org-download-method)))

(defun who-org/init-org-emms ()
  (add-to-list 'load-path "~/.emacs.d/private/who-org/extra/emms-5.3/lisp")

  (require 'emms-setup)
  (emms-all)
  ;; not sure why this doesn't work, but seeking is wildly inconsistent with the default players - vlc seems to work
  ;; (emms-default-players)
  (setq emms-player-list '(emms-player-vlc))

  (use-package org-emms
    :after org
    :config
    (setq org-emms-default-directory "~/org/library")
    (setq org-emms-delay 1)))

(defun who/org-schedule-incl-gcal-at-point ()
  "Schedule the org item at point and post it to gcal."
  (interactive)
  (org-schedule nil)
  (org-entry-put (point) "calendar-id" "willy.ohanley@gmail.com")
  (org-gcal-post-at-point))

(defun who-org/init-org-gcal ()

  (defun who/get-file-contents (filename)
    "Return the contents of FILENAME."
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string)))

  (use-package org-gcal
    :after org
    :config
    (setq org-gcal-client-id (who/get-file-contents "~/org/.org-gcal-client-id")
          org-gcal-client-secret (who/get-file-contents "~/org/.org-gcal-client-secret")
          org-gcal-fetch-file-alist '(("willy.ohanley@gmail.com" . "~/org/gtd/calendars/personal.org")
                                      ("lmv5qq6jrpqbkgveladotgbgmg@group.calendar.google.com" . "~/org/gtd/calendars/class.org")))
    (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync nil t t)))
    (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync nil nil t)))
    :bind (:map org-mode-map
                ("M-m m d S" . who/org-schedule-incl-gcal-at-point))))

(defun who/org-noter-insert-highlighted-note ()
  "Highlight the active region and add a precise note at its position."
  (interactive)
  ;; Adding an annotation will deactivate the region, so we reset it afterward
  (let ((region (pdf-view-active-region)))
    (call-interactively 'pdf-annot-add-highlight-markup-annotation)
    (setq pdf-view-active-region region))
  (call-interactively 'org-noter-insert-precise-note))

(defun who-org/init-org-noter ()
  (use-package org-noter
    :after org
    :bind (:map org-noter-doc-mode-map
                ("d" . 'who/org-noter-insert-highlighted-note)
                ("h" . 'pdf-annot-add-highlight-markup-annotation))))

(defun who-org/init-org-pdftools ()
  (use-package org-pdftools
    :hook (org-load . org-pdftools-setup-link)
    :config
    (setq org-pdftools-root-dir "~/org/library")
    (setq org-pdftools-markup-pointer-color "#FFFF00")))

;; org-pdftools and org-noter integration doesn't seem to work very well right now. maybe check back later, development seems active
;; (defun who-org/init-org-noter-pdftools ()
;;   (use-package org-noter-pdftools
;;     :config
;;     (with-eval-after-load 'pdf-annot
;;       (add-hook 'pdf-annot-activate-handler-functions 'org-noter-jump-to-note))))

(defun who-org/post-init-org-roam ()
  (setq org-roam-directory "~/org/zettelkasten")
  (setq org-roam-buffer-width 0.25)

  ;; build cache in background
  (add-hook 'org-roam-mode-hook 'org-roam--build-cache-async)

  ;; show backlinks on opening zettel. not doing this right now because it has a
  ;; lot of weird knock-on effects. maybe there's a more stable way to do it
  ;; (add-hook 'find-file-hook #'who-org/show-backlinks)

  (defun who-org/show-backlinks ()
    (when (and (not (get-buffer-window org-roam-buffer))
               (f-descendant-of? buffer-file-name (f-join (getenv "HOME") "org/zettelkasten")))
      (org-roam))))

(defun who-org/init-org-wild-notifier ()
  (use-package org-wild-notifier
    :hook (after-init . org-wild-notifier-mode)))
