;;; packages.el --- who-dired layer packages file for Spacemacs.
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
;; added to `who-dired-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `who-dired/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `who-dired/pre-init-PACKAGE' and/or
;;   `who-dired/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst who-dired-packages
  '(dired dired-hacks-utils dired-avfs dired-collapse dired-open dired-subtree)
  "The list of Lisp packages required by the who-dired layer.

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

(defun who-dired/post-init-dired ()
  (setq dired-listing-switches (purecopy "-lAFh")))

(defun who-dired/init-dired-hacks-utils ()
  (use-package dired-hacks-utils))

(defun who-dired/init-dired-avfs ()
  (use-package dired-avfs))

(defun who-dired/init-dired-collapse ()
  (use-package dired-collapse
    :hook (dired-mode . dired-collapse-mode)))

(defun who/dired-open-dir ()
  "If point is on a directory-file line, open the directory under
point in a new buffer."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (if (string-match dired-re-dir line)
        (dired-find-file))))

(defun who-dired/init-dired-open ()
  (use-package dired-open
    :config
    (setq dired-open-functions '(dired-open-by-extension who/dired-open-dir dired-open-subdir dired-open-xdg))))

(defun who-dired/init-dired-subtree ()
  (use-package dired-subtree
    :bind (:map dired-mode-map
                ("[" . dired-subtree-toggle))))

;;; packages.el ends here
