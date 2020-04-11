;;; packages.el --- who-email layer packages file for Spacemacs.
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
;; added to `who-email-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `who-email/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `who-email/pre-init-PACKAGE' and/or
;;   `who-email/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst who-email-packages
  '(notmuch)
  "The list of Lisp packages required by the who-email layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun who-email/init-notmuch ()
  (use-package notmuch
    :preface (setq-default notmuch-command (executable-find "notmuch"))

    :if (executable-find "notmuch")

    :bind (("<f2>" . notmuch)
           :map notmuch-tree-mode-map
           ("<f2>" . (lambda () (interactive) (notmuch-tree "tag:inbox")))
           :map notmuch-search-mode-map
           ("d" . who/notmuch-search-uninbox)
           ("y" . who/notmuch-search-toggle-unread)
           ("a" . notmuch-search-archive-thread)
           ("h" . who/notmuch-search-ham)
           ("H" . who/notmuch-search-spam)
           ("r" . notmuch-search-reply-to-thread)
           ("R" . notmuch-search-reply-to-thread-sender)
           :map notmuch-tree-mode-map
           ("d" . who/notmuch-tree-uninbox)
           ("h" . who/notmuch-tree-ham)
           ("H" . who/notmuch-tree-spam)
           ("y" . who/notmuch-tree-toggle-unread)
           ;; ("r" . notmuch-tree-reply-to-thread)
           ;; ("R" . notmuch-tree-reply-to-thread-sender)
           :map notmuch-show-mode-map
           ("d" . who/notmuch-show-pop-uninbox)
           ("l" . who/notmuch-show-jump-to-latest)
           ("<tab>" . org-next-link)
           ("<backtab>". org-previous-link)
           ("C-<return>" . browse-url-at-point)
           ("c" . who/org-capture-email))

    :config
    (defun who/org-capture-email ()
      (interactive)
      (org-capture nil "e"))

    ;;;
    ;; util
    ;;;

    (defun spam-start-process (query)
      "Start a process to mark selected messages as spam with bogofilter."
      (start-process-shell-command "bogofilter-spam" "*bogofilter*"
                                   (concat notmuch-command " search --output=files "
                                           ;; tag:spam finagling is important - without it we might
                                           ;; have a race with a corresponding 'notmuch tag'
                                           "'(tag:spam or not tag:spam) and (" query ")'"
                                           " | bogofilter -s -b -v")))

    (defun ham-start-process (query)
      "Start a process to mark selected messages as ham with bogofilter."
      (start-process-shell-command "bogofilter-ham" "*bogofilter*"
                                   (concat notmuch-command " search --output=files "
                                           ;; tag:spam finagling is important - without it we might
                                           ;; have a race with a corresponding 'notmuch tag'
                                           "'(tag:spam or not tag:spam) and (" query ")'"
                                           " | bogofilter -n -b -v")))

    ;;;
    ;; notmuch-search
    ;;;

    (defun who/notmuch-search-uninbox ()
      (interactive)
      (notmuch-search-tag (list "-inbox" "-unread"))
      (notmuch-search-next-thread))

    (defun who/notmuch-search-toggle-unread ()
      (interactive)
      (if (member "unread" (notmuch-search-get-tags))
          (notmuch-search-tag (list "-unread"))
        (notmuch-search-tag (list "+unread"))))

    (defun who/notmuch-search-ham ()
      (interactive)
      (ham-start-process (notmuch-search-find-thread-id))
      (notmuch-search-tag (list "-spam" "-maybe-spam")))

    (defun who/notmuch-search-spam ()
      (interactive)
      (spam-start-process (notmuch-search-find-thread-id))
      (notmuch-search-tag (list "+spam" "-maybe-spam" "-inbox"))
      (notmuch-search-next-thread)
)

    ;;;
    ;; notmuch-tree
    ;;;

    (defun who/notmuch-tree-uninbox ()
      (interactive)
      (notmuch-tree-tag-thread (list "-inbox"))
      (notmuch-tree-next-thread)
      (notmuch-tree-show-message-in))

    (defun who/notmuch-tree-toggle-unread ()
      (interactive)
      (if (member "unread" (notmuch-search-get-tags))
          (notmuch-tree-tag (list "-unread"))
        (notmuch-tree-tag (list "+unread"))))

    (defun who/notmuch-tree-ham ()
      (interactive)
      (ham-start-process (notmuch-tree-get-messages-ids-thread-search))
      (notmuch-tree-tag-thread (list "-spam" "-maybe-spam")))

    (defun who/notmuch-tree-spam ()
      (interactive)
      (spam-start-process (notmuch-tree-get-messages-ids-thread-search))
      (notmuch-tree-tag-thread (list "+spam" "-maybe-spam" "-inbox")))

    ;;;
    ;; notmuch-show
    ;;;

    (defun who/notmuch-show-pop-uninbox ()
      (interactive)
      (notmuch-bury-or-kill-this-buffer)
      (who/notmuch-search-uninbox))

    (defun who/notmuch-show-jump-to-latest ()
      "Jump to the message in the current thread with the latest
timestamp."
      (interactive)
      (let ((timestamp 0)
            latest)
        (notmuch-show-mapc
         (lambda () (let ((ts (notmuch-show-get-prop :timestamp)))
                      (when (> ts timestamp)
                        (setq timestamp ts
                              latest (point))))))
        (if latest
            (progn
              (goto-char latest)
              (recenter-top-bottom 10)) ;; scroll down to message
          (error "Cannot find latest message."))))

    :custom
    (message-auto-save-directory "~/.mail/drafts/")

    (message-send-mail-function 'message-send-mail-with-sendmail)
    (sendmail-program (executable-find "msmtp"))

    ;; pick SMTP server based on envelope from: per https://notmuchmail.org/emacstips/#index11h2
    (message-sendmail-envelope-from 'header)
    (mail-envelope-from 'header)
    (mail-specify-envelope-from t)

    ;; where to save sent mail
    (notmuch-fcc-dirs '((".*@wohanley.com" . "me/Sent")
                        ("willy.ohanley@gmail.com" . "gmail/Sent")
                        ("whohanley@uvic.ca" . "uvic/Sent")))

    (message-sendmail-f-is-evil nil)
    (message-kill-buffer-on-exit t)
    (notmuch-always-prompt-for-sender t)
    (notmuch-crypto-process-mime t)
    (notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
    ;; (notmuch-labeler-hide-known-labels t)
    (notmuch-search-oldest-first nil)
    (notmuch-archive-tags '("+archived" "-inbox" "-unread"))
    (notmuch-message-headers '("To" "Cc" "Subject" "Bcc"))
    (notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "j" :search-type tree)
                              (:name "unread" :query "tag:inbox and tag:unread" :key "u")
                              (:name "spam" :query "tag:spam" :key "s")
                              (:name "drafts" :query "tag:draft" :key "d")))))
;;; packages.el ends here
