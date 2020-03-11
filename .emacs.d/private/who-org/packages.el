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
  '(deft org org-agenda org-clock-convenience org-gcal org-roam))

(defun who-org/post-init-deft ()
  (setq deft-recursive 1)
  (setq deft-use-filename-as-title t)
  (setq deft-default-extension "org")
  (setq deft-directory "~/org/zettelkasten"))

(defun who-org/post-init-org ()
  (require 'org-habit)
  (require 'org-protocol)
  (require 'org-tempo)

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-protocol)

  (setq org-startup-folded 'showall)
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

  ;; (add-hook 'org-mode-hook #'who/style-org)

  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)))

(defun who-org/post-init-org-agenda ()

  (setq who/org-agenda-directory "~/org/gtd/")

  (require 'find-lisp)
  (defun who/find-org-files (directory)
    (find-lisp-find-files directory "\.org$"))

  (setq org-agenda-files
         (who/find-org-files who/org-agenda-directory))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil)

  ;;;
  ;; capture
  ;;;

  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat who/org-agenda-directory "inbox.org"))
           "* TODO %?")
          ("e" "email" entry (file+headline ,(concat who/org-agenda-directory "emails.org") "Emails")
           "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
          ("l" "link" entry (file ,(concat who/org-agenda-directory "inbox.org"))
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file ,(concat who/org-agenda-directory "inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))

  (defun who/org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture nil "i"))

  (use-package org-download
    :after org
    :bind
    (:map org-mode-map
          (("s-Y" . org-download-screenshot)
           ("s-y" . org-download-yank)))
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
            (dirname (concat "./images/"
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
    (setq org-download-method 'my-org-download-method))

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
  (defun who/org-agenda-project-files ()
     (who/find-org-files (concat who/org-agenda-directory "projects")))
  (setq org-refile-targets '(("next.org" :level . 0)
                             ("someday.org" :level . 0)
                             ("reading.org" :level . 1)
                             (who/org-agenda-project-files :level . 0)))

  (defvar who/org-current-effort "1:00" "Current effort for agenda items.")

  (defun who/my-org-agenda-set-effort (effort)
    "Set the effort property for the current headline."
    (interactive
     (list (read-string (format "Effort [%s]: " who/org-current-effort) nil nil who/org-current-effort)))
    (setq who/org-current-effort effort)
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           newhead)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (funcall-interactively 'org-set-effort nil who/org-current-effort)
          (end-of-line 1)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker))))

  (defun who/org-agenda-process-inbox-item ()
    "Process a single item in the org-agenda."
    (interactive)
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (call-interactively 'who/my-org-agenda-set-effort)
     (org-agenda-refile nil nil t)))

  (defun who/bulk-process-entries ()
    (if (not (null org-agenda-bulk-marked-entries))
        (let ((entries (reverse org-agenda-bulk-marked-entries))
              (processed 0)
              (skipped 0))
          (dolist (e entries)
            (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
              (if (not pos)
                  (progn (message "Skipping removed entry at %s" e)
                         (cl-incf skipped))
                (goto-char pos)
                (let (org-loop-over-headlines-in-active-region) (funcall 'who/org-agenda-process-inbox-item))
                ;; `post-command-hook' is not run yet.  We make sure any
                ;; pending log note is processed.
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (cl-incf processed))))
          (org-agenda-redo)
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries%s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistent-marks) "" " (kept marked)")))
      ))

  (defun who/org-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (who/bulk-process-entries))

  ;;;
  ;; org-agenda reading view
  ;;;

  (setq who/org-agenda-reading-view
        `("r" "Reading" todo ""
          ((org-agenda-files '(,(concat who/org-agenda-directory "reading.org"))))))

  (add-to-list 'org-agenda-custom-commands `,who/org-agenda-reading-view)

  ;;;
  ;; todo
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
                  (org-agenda-files '(,(concat who/org-agenda-directory "someday.org")
                                      ,(concat who/org-agenda-directory "projects")
                                      ,(concat who/org-agenda-directory "next.org")))
                  ))
           (todo "TODO"
                 ((org-agenda-overriding-header "To Refile")
                  (org-agenda-files '(,(concat who/org-agenda-directory "inbox.org")))))
           (todo "TODO"
                 ((org-agenda-overriding-header "Backlog")
                  (org-agenda-files '(,(concat who/org-agenda-directory "next.org")
                                      ,(concat who/org-agenda-directory "projects")))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))
                 )
           (todo "HOLD"
                 ((org-agenda-overriding-header "Blocked")
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))
                 )
           (todo "TODO"
                 ((org-agenda-overriding-header "Someday")
                  (org-agenda-files '(,(concat who/org-agenda-directory "someday.org")))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))
                 ))
           nil))

  (add-to-list 'org-agenda-custom-commands `,who/org-agenda-todo-view)

  (defun who/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))

  ;;;
  ;; org-journal
  ;;;

  (setq org-journal-dir "~/org/zettelkasten")
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "private-%Y-%m-%d.org")
  (setq org-journal-date-format "%Y-%m-%d")

  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t))

  ;;;
  ;; bindings
  ;;;

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
              (define-key org-agenda-mode-map "o" 'org-agenda-clock-out)
              (define-key org-agenda-mode-map "s" 'who/org-agenda-process-inbox-item)
              (define-key org-agenda-mode-map "r" 'who/org-process-inbox)
              (define-key org-agenda-mode-map "R" 'org-agenda-refile)
              (define-key org-agenda-mode-map "c" 'who/org-inbox-capture)))

  (bind-key "<f1>" 'who/switch-to-agenda)

  (defvar who/org-agenda-bulk-process-key ?f
    "Default key for bulk processing inbox items.")
  (setq org-agenda-bulk-custom-functions `((,who/org-agenda-bulk-process-key who/org-agenda-process-inbox-item)))

  (add-hook 'org-clock-in-hook 'who/set-todo-state-next 'append))

(defun who-org/init-org-clock-convenience ()
  (use-package org-clock-convenience
    :bind (:map org-agenda-mode-map
               ("<S-up>" . org-clock-convenience-timestamp-up)
               ("<S-down>" . org-clock-convenience-timestamp-down)
               ("o" . org-clock-convenience-fill-gap)
               ("e" . org-clock-convenience-fill-gap-both))))

(defun who-org/init-org-gcal ()

  (defun who/get-file-contents (filename)
    "Return the contents of FILENAME."
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string)))

  (require 'org-gcal)
  (setq org-gcal-client-id (who/get-file-contents "~/org/.org-gcal-client-id")
        org-gcal-client-secret (who/get-file-contents "~/org/.org-gcal-client-secret")
        org-gcal-fetch-file-alist '(("willy.ohanley@gmail.com" . "~/org/gtd/calendars/personal.org")
                                    ("lmv5qq6jrpqbkgveladotgbgmg@group.calendar.google.com" . "~/org/gtd/calendars/class.org"))))

(defun who-org/post-init-org-roam ()
  (setq org-roam-directory "~/org/zettelkasten")
  (setq org-roam-buffer-width 0.3)

  ;; build cache in background
  (add-hook 'org-roam-mode-hook 'org-roam--build-cache-async))
