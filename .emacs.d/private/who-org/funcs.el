(defun who/mtime (file) (let ((attrs (file-attributes file))) (nth 5 attrs)))

(defun who/latest-file (path)
  (let ((e (f-entries path)))
    (car (sort e (lambda (a b)
                   (not (time-less-p (who/mtime a)
                                     (who/mtime b))))))))

(defun who/org-insert-link-to-latest ()
  (interactive)
  (let ((file (who/latest-file "~/org/library")))
    (funcall-interactively 'org-insert-link nil file (f-filename file))))

(defun who/org-player-insert-link-to-position ()
  (interactive)
  (org-player-insert-link-to-position 7))

;;;
;; org-agenda
;;;

(defun who/switch-to-agenda ()
  (interactive)
  (org-agenda nil " "))

(defun who/org-agenda-set-effort (effort)
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
   (call-interactively 'who/org-agenda-set-effort)
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
;; emms
;;;

(defun who/emms-seek-to (time)
  "Seek to TIME, expressed as a string in org-emms style."
  (emms-seek-to (org-emms-time-string-to-seconds time)))
