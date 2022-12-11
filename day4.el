;;; day4.el --- day 4 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeff Young
;;
;; Author: Jeff Young <jeff@doyougnu.xyz>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: December 10, 2022
;; Modified: December 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/doyougnu/day4
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  day 4
;;
;;; Code:

(require 'dash)
(require 'utils)

(defun load-data! (file)
  (utils|slurp! file))

(defun parse-data (stream)
  (split-string stream "\n" t))

(defun example ()
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defun line-parser (line-str)
  (when (string-match "[[:digit:]]-[[:digit:]],[[:digit:]]-[[:digit:]]" line-str)
    (list (match-string 0 line-str)
          (match-string 1 line-str)
          (match-string 2 line-str)
          (match-string 3 line-str))))

(line-parser "2-3,3-4")



(provide 'day4)
;;; day4.el ends here
