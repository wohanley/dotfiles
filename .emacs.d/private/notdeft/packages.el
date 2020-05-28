;;; packages.el --- notdeft layer packages file for Spacemacs.
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
;; added to `notdeft-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `notdeft/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `notdeft/pre-init-PACKAGE' and/or
;;   `notdeft/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst notdeft-packages
  '((notdeft :location local))
  "The list of Lisp packages required by the notdeft layer.

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

(defun who/notdeft-saved-search ()
  (interactive)
  (let ((choices '("tag:project AND NOT tag:archive"
                   "tag:AoR AND NOT tag:archive")))
    (notdeft-open-query (ido-completing-read "Search: " choices))))

(defun notdeft/init-notdeft ()
  (use-package notdeft
    :commands (notdeft notdeft-list-files-by-query)
    :custom
    (notdeft-directories '("~/org/zettelkasten"))
    :init
    (spacemacs/declare-prefix "an" "notdeft")
    (spacemacs/set-leader-keys
      "ann" 'notdeft
      "ans" 'who/notdeft-saved-search)
    :config
    (setq notdeft-xapian-program (expand-file-name "~/.emacs.d/private/notdeft/local/notdeft/xapian/notdeft-xapian"))
    ;; track changes to org files
    (add-hook 'org-mode-hook 'notdeft-note-mode)))

;;; packages.el ends here
