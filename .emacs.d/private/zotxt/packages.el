;;; packages.el --- zotxt layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <me@wohanley.com>
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
;; added to `zotxt-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `zotxt/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `zotxt/pre-init-PACKAGE' and/or
;;   `zotxt/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst zotxt-packages
  '((zotxt :location
                 (recipe :fetcher gitlab :repo "egh/zotxt-emacs"))))

(defun zotxt/init-zotxt ()
  (use-package zotxt
    :after org
    ;;:hook
    ;;(after-init . zotxt)
    :init
    ;; activate org-zotxt-mode in org buffers
    (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))
    :custom
    ;; use short reference style per http://www.mkbehr.com/posts/a-research-workflow-with-zotero-and-org-mode/
    (zotxt-default-bibliography-style "who-short")))
