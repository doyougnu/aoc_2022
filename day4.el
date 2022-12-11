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
  (let ((regex (rx (seq (group (+ num)) "-"
                        (group (+ num)) ","
                        (group (+ num)) "-"
                        (group (+ num))))))
    (when (string-match regex line-str)
      (list :minL (string-to-number (match-string 1 line-str))
            :maxL (string-to-number (match-string 2 line-str))
            :minR (string-to-number (match-string 3 line-str))
            :maxR (string-to-number (match-string 4 line-str))))))

(defun fully-contains (pairs)
  "Given a pair of pairs, the left pair fully contains the right
pair iff the left min is less than or equal to the right min and
iff the left max in greater than or equal to the right max"
  (or (and (<= (plist-get pairs :minL)
               (plist-get pairs :minR))
           (>= (plist-get pairs :maxL)
               (plist-get pairs :maxR)))
      (and (<= (plist-get pairs :minR)
               (plist-get pairs :minL))
           (>= (plist-get pairs :maxR)
               (plist-get pairs :maxL)))))


;; 1
(->> (load-data! "./day4/data")
    parse-data
    (-map #'line-parser)
    (-map #'(lambda (e)
              (if (fully-contains e)
                  1 0)))
    -sum)

;; 2
(defun between (n l r)
  "Is N, included in the range determined by L and R (inclusive)."
  (or (<= l n)
      (>= r n)))

(defun overlap? (pairs)
  "Does one pair overlap with the range of another pair."
  (let ((left-min (plist-get pairs :minL))
        (left-max (plist-get pairs :maxL))
        (right-min (plist-get pairs :minR))
        (right-max (plist-get pairs :maxR)))
    (not (or (< left-max right-min)
             (> left-min right-max)))))

(->> (load-data! "./day4/data")
    parse-data
    (-map #'line-parser)
    (-map #'(lambda (e)
              (if (overlap? e)
                  1 0)))
    -sum)


(provide 'day4)
;;; day4.el ends here
