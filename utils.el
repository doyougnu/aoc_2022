;;; utils.el --- utilities for aoc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeff Young
;;
;; Author: Jeff Young <jeff@doyougnu.xyz>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: December 04, 2022
;; Modified: December 04, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/doyougnu/utils
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  utilities for aoc
;;
;;; Code:


(defun utils|slurp! (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))


(provide 'utils)
;;; utils.el ends here
