;;; day3.el --- day3 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeff Young
;;
;; Author: Jeff Young <jeff@doyougnu.xyz>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: December 04, 2022
;; Modified: December 04, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/doyougnu/day3
;; Package-Requires: ((emacs "25.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  day3
;;  1. find common elements in two sequences
;;  2. convert these to integers
;;  3. sum
;;
;;  input is not so big that we need to stream, so this will be real easy
;;  it'll be O(n^2) if we do the naive algorithm we could:
;;  1. calc the levensteihn distance first and if its equal string length then skip but this is likely slower than the naive algorithm
;;  2. sort both strings then just iterate through them in two-pointer fashion, this would be n log n for the sort only
;;  3.
;;
;;; Code:

(require 'dash)
(require 'utils)

;; (seq-intersection "hello" "ell")

(defconst priority-list
  (-zip (number-sequence 1 52)
        (string-to-list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defun load-data! (file)
  (utils|slurp! file))

(defun parse-data (stream)
  (split-string stream "\n" t))

(defun split-by (line &optional f)
  "Split an entry by F, if F is provided, if not then split a line
   by half. Returns a pair of the line before the split and after."
  (let ((half (if (not f)
                  (/ (length line) 2)
                (funcall f line))))
    (list (seq-take line half) (seq-drop line half))))

(defun shared-item (&rest items)
  "Return the shared items in the left and right compartments"
  (-reduce #'seq-intersection items))

(defun item->priority (item)
  (car (rassoc item priority-list)))

(defun priority->item (prio)
  (cdr (assoc prio priority-list)))

;; part one
(-> (load-data! "./day3/data")
    parse-data
    (->>
     (-map #'split-by)
     (-map #'(lambda (e)
               (seq-uniq
                (apply #'shared-item e))))
     (-mapcat #'(lambda (e)
                  (-map #'item->priority e)))
     -sum))

;; part two
(-> (load-data! "./day3/data")
    parse-data
    (->>
     (-partition-all 3)
     (-map #'(lambda (l)
                  (-mapcat #'split-by l)))

     (-map #'shared-item)
     ;; (-mapcat #'(lambda (e)
     ;;              (-map #'item->priority e)))
     ;; -sum
     ))


(provide 'day3)
;;; day3.el ends here
