;;; day1.el --- Advent of Code 2022 in elisp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeff Young
;;
;; Author: Jeff Young <jeff@doyougnu.xyz>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: practice
;; Homepage: https://github.com/doyougnu/day1_
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; need to:
;; 1. Parse the data into a datastructure
;; 2. Name each elf
;; 3. Calculate the weight of the food
;;
;;
;; Package-Requires: ((dash "2.19.1"))
;;; Code:

(require 'dash)
(require 'utils)

(defun parse-supplies (supplies)
  "Convert SUPPLIES from a list of strings to a list of lists."
  (->> supplies
       (-map (lambda (s)
               (-> s (split-string "\n" t))))))

(defun supplies->calories (supplies)
  "Convert SUPPLIES from a list of lists of strings to a list of
   list of numbers."
  (-map (lambda (supply-of-elf)
          (-map (lambda (supply)
                  (string-to-number supply))
                supply-of-elf))
   supplies))

(defun calc-total-calories (calorie-list)
  "Given a list of list of numbers, CALORIE-LIST, sum each
   sub-list."
  (-map #'-sum calorie-list))

(defun calories-ledger ()
  "Read in the calorie ledger data and parse to the actual ledger."
  (--> (util|slurp! "./day1/data")
       (split-string it "\n\n" t)
       (parse-supplies it)
       (supplies->calories it)
       (calc-total-calories it)
       (-zip (number-sequence 1 (length it)) it) ;; not that this relies on each preceding operation being order preserving
       (sort it #'(lambda (l r)
                    (> (cdr l)
                       (cdr r))))))

(defun most-caloric-elf ()
  "The elf holding the most calories. Its bulking season bby!"
  (-> (calories-ledger)
      first
      cdr))

(most-caloric-elf)

(defun top-n-elves (n)
  "Return the top N elves carrying the most calories"
  (-take n (calories-ledger)))

;; and no sum
(->> (top-n-elves 3)
     (map 'list #'cdr)
     -sum)

(provide 'day1)
;;; day1.el ends here
