;;; -*- coding: utf-8-unix -*-
;;; org-player.el - Play audio and video files in org-mode file: links
;;;
;;; Adapted from http://bitbucket.org/eeeickythump/org-player/ to be based on
;;; https://github.com/thomasluquet/playerctl.el rather than Bongo.
;;;
;;; Usage
;;; =====
;;;
;;; Clicking on a link such as
;;;
;;;   [[file:/path/to/song.mp3]]
;;;
;;; adds it to the active Bongo playlist and immediately starts playing
;;; it. Playback can be paused, fast-forwarded etc using Bongo commands (see
;;; below).
;;;
;;; Links can also specify track positions. When a link contains a track
;;; position, playback will start at that position in the track. For example:
;;;
;;; [[file:/path/to/song.mp3::2:43]]      Starts playback at 2 min 43 sec.
;;; [[file:/path/to/song.mp3::1:10:45]]   Starts playback at 1 hr 10 min 45 sec.
;;; [[file:/path/to/song.mp3::3m15s]]     Starts playback at 3 min 15 sec.
;;; [[file:/path/to/song.mp3::49s]]       Starts playback at 0 min 49 sec.
;;; [[file:/path/to/song.mp3::1h21m10s]]  Starts playback at 1 hr 21 min 10 sec.
;;;
;;; Controlling playback
;;; ====================
;;;
;;; When Bongo plays a file it puts some icons in the modeline that resemble the
;;; well-known symbols for 'play', 'stop', 'rewind' and so on, and which can be
;;; used to control playback using the mouse. I found these worked erratically when
;;; outside the actual Bongo playlist buffer, so I have instead bound some org-mode
;;; keys (ctrl + numpad keys) to the relevant functions.  These are:
;;;
;;; C-<keypad 0>     pause/resume
;;; C-<keypad .>     stop, or restart playback from beginning
;;; C-<keypad />     show track info
;;; C-<keypad 4>     skip back 10 seconds
;;; C-<keypad 6>     skip forward 10 seconds
;;; C-<keypad 9>     increase volume (requires separate 'volume' library
;;;                  at https://github.com/dbrock/volume-el )
;;; C-<keypad 3>     decrease volume
;;;
;;; Note that these keys (and the modeline icons, if they work) act on the
;;; track that is active in the Bongo playlist. This will always be the last
;;; track that you added by clicking on a link in org-mode, unless you alter
;;; the Bongo playlist outside org mode.
;;;
;;; Also note that these keys are only bound within org mode buffers.

;;;
;; playerctl
;;;

(defun playerctl--command (msg cmd &rest args)
  (let ((proc (apply 'start-process "playerctl" "*playerctl*" "playerctl" cmd args)))
    (with-current-buffer "*playerctl*"
      (insert msg))
    proc))

;;;###autoload
(defun playerctl-start (file)
  (interactive)
  (start-process "vlc" "*playerctl*" "vlc" file))

;;;###autoload
(defun playerctl-stop ()
  (interactive)
  (playerctl--command "Stop playback" "stop"))

;;;###autoload
(defun playerctl-status ()
  (interactive)
  (let ((proc (playerctl--command "status" "status")))
    (set-process-filter proc (lambda (proc line)
                            (message "Status : %s" line)))))

;;;###autoload
(defun playerctl-get-position ()
  (interactive)
  (playerctl--command (format "Get current position" position) "position"))

;;;###autoload
(defun playerctl-set-position (position)
  (interactive)
  (playerctl--command (format "Seek to %d" position) "position" (format "%d" position)))

(require 'org)

(defvar org-player-file-extensions-regexp
  (concat "\\." (regexp-opt
                 (append '("flac" "mp3" "ogg" "wav")
                         '("avi" "mkv" "mp4" "mpeg" "webm" "wmv")))))

(add-to-list 'org-file-apps
             (cons (concat org-player-file-extensions-regexp "$")
                   (lambda (file link)
                     (org-player-play-file file))))

(add-to-list 'org-file-apps
             (cons (concat org-player-file-extensions-regexp
                           "::\\([0-9]+:[0-9]+\\(:[0-9]+\\)?\\)")
                   (lambda (file link)
                     (org-player-play-file file (match-string 1 link)))))

(add-to-list 'org-file-apps
             (cons (concat org-player-file-extensions-regexp
                           "::[0-9]+[HhMmSs]\\([0-9]+[MmSs]\\|\\)\\([0-9]+[MmSs]\\|\\)")
                   (lambda (file link)
                     (org-player-play-file file (match-string 1 link)))))

(defun org-player-time-to-string (time)
  "Converts a number of seconds to a HH:MM:SS string."
  (let* ((seconds (round time))
         (hours (/ seconds 3600))
         (minutes (/ (- seconds (* hours 3600)) 60))
         (remaining-seconds (- seconds (* hours 3600) (* minutes 60))))
    (format "%02d:%02d:%02d" hours minutes remaining-seconds)))

(defun org-player-string-to-time (str)
  "Understands any of the following formats: MM:SS, HH:MM:SS, NNhNNmNNs,
NNhNNm, NNmNNs, NNh, NNm, NNs."
  (let ((str (substring-no-properties str))
        hours mins secs)
    (cond
     ((string-match "\\([0-9]+\\):\\([0-9]+\\)\\(:[0-9.]+\\|\\)" str)
      (if (string= "" (match-string 3 str)) ; XX:YY
          (setq mins (match-string 1 str)
                secs (match-string 2 str))
        (setq hours (match-string 1 str) ; XX:YY:ZZ
              mins (match-string 2 str)
              secs (subseq (match-string 3 str) 1))))
     ((eq (string-match "[0-9]+[HhMmSs]" str)
          (string-match "\\([0-9]+[Hh]\\|\\)\\([0-9]+[Mm]\\|\\)\\([0-9.]+[Ss]\\|\\)"
                        str))
      (setq hours (match-string 1 str)
            mins (match-string 2 str)
            secs (match-string 3 str))
      (unless (string= "" hours)
        (setq hours (subseq hours 0 (1- (length hours)))))
      (unless (string= "" mins)
        (setq mins (subseq mins 0 (1- (length mins)))))
      (unless (string= "" secs)
        (setq secs (subseq secs 0 (1- (length secs))))))
     (t
      (error "Unrecognised track position specifier: %S" str)))
    (setq hours (if (or (null hours) (string= "" hours))
                    0 (read hours)))
    (setq mins (if (or (null mins) (string= "" mins))
                    0 (read mins)))
    (setq secs (if (or (null secs) (string= "" secs))
                    0 (read secs)))
    (list hours mins secs)))

(defun org-player-play-file (filename &optional pos)
  "POS, if given, is a string that specifies a track position where
playback should begin. This string is taken from the portion of the
hyperlink that follows a double colon. For example:

  [[file:song.mp3::03:22]]
  [[file:song.mp3::1h24m3s]]

See `org-player-string-to-time' for a description of the format of
the POS string."
  (let ((seek (if pos
                  (destructuring-bind (hours mins secs)
                      (org-player-string-to-time pos)
                    (+ secs
                       (* mins 60)
                       (* hours 60 60))))))
    (playerctl-start filename)
    (when seek
      (sleep-for 1) ;; wait for VLC to start up
      (playerctl-set-position seek))))

(defun org-player-insert-link-to-position ()
  "Insert a 'file:' link to the current position in the currently
playing track."
  (interactive)
  (let* ((file-url (string-trim (shell-command-to-string "playerctl metadata xesam:url")))
         (position (string-to-number (shell-command-to-string "playerctl position")))
         (timestamp (org-player-time-to-string position))
         (link (replace-regexp-in-string "file://" "file:" file-url))
         (link-timestamped (concat link "::" timestamp)))
    (insert (format "[[%s][%s]]" link-timestamped timestamp))))

;; ;; Numpad Ctrl-0: pause/resume
;; (define-key org-mode-map (kbd "<C-kp-0>") 'org-player-pause/resume)
;; (define-key org-mode-map (kbd "<C-kp-insert>") 'org-player-pause/resume)
;; ;; Numpad Ctrl-.: stop current track, or restart from beginning if stopped
;; (define-key org-mode-map (kbd "<C-kp-decimal>") 'org-player-start/stop)
;; (define-key org-mode-map (kbd "<C-kp-delete>") 'org-player-start/stop)
;; ;; Numpad Ctrl-PgUp, Ctrl-PgDn: raise/lower volume
;; (define-key org-mode-map (kbd "<C-kp-prior>") 'volume-raise)
;; (define-key org-mode-map (kbd "<C-kp-next>") 'volume-lower)
;; (define-key org-mode-map (kbd "<C-kp-9>") 'volume-raise)
;; (define-key org-mode-map (kbd "<C-kp-3>") 'volume-lower)
;; ;; Numpad Ctrl-left, Ctrl-right: skip back/forward 10 seconds
;; (define-key org-mode-map (kbd "<C-kp-left>") 'bongo-seek-backward-10)
;; (define-key org-mode-map (kbd "<C-kp-right>") 'bongo-seek-forward-10)
;; (define-key org-mode-map (kbd "<C-kp-4>") 'bongo-seek-backward-10)
;; (define-key org-mode-map (kbd "<C-kp-6>") 'bongo-seek-forward-10)
;; ;; Ctrl-/: show track info
;; (define-key org-mode-map (kbd "<C-kp-divide>") 'org-player-print-track-info)


(provide 'org-player)
