;;; day2.el --- day2 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeff Young
;;
;; Author: Jeff Young <jeff@doyougnu.xyz>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: December 04, 2022
;; Modified: December 04, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/doyougnu/day2
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;  day2: we already have the solution from day one, just need to change the final call
;;
;;; Code:

(require 'dash)
(require 'utils)

(defconst shape-score
  '(:rock  1 :paper  2 :scissors  3)
  "Association of scores by shape")

(defun shape->score (shape)
  (plist-get shape-score shape))

(defconst outcome-score
  '(:win 6 :lose 0 :draw 3)
  "Association of scores by round")

(defun outcome->score (outcome)
  (plist-get outcome-score outcome))

(defun beats (p0 p1)
  "Compare the shapes of P0 to P1, return an outcome; one of :win,
   :lose or :draw"
  (if (equal p0 p1)
      :draw
    (pcase (list p0 p1)
      ((or `(:rock     :scissors)
           `(:paper    :rock)
           `(:scissors :paper)) :win)

      (_  :lose))))


(defun round-score (opp-move shape)
  "Calculate the score for a round, given a SHAPE and an OUTCOME"
  (+ (shape->score shape)
     (outcome->score (beats shape opp-move))))

(defun parse-opponent-shape (op-shape)
  (pcase op-shape
    ("A" :rock)
    ("B" :paper)
    ("C" :scissors)
    (else (error "parse-opponent-shape encountered `%s'" else))))

(defun parse-move (op-shape)
  (pcase op-shape
    ("X" :rock)
    ("Y" :paper)
    ("Z" :scissors)
    (else (error "parse-move encountered `%s'" else))))

(defun parse-round (round-str)
  "Parse a single round, converting from a string to a pair of
   opponent move and move"
  (pcase (split-string round-str " " t)
    (`(,opp-move ,move) (list (parse-opponent-shape opp-move) (parse-move move)))))

(defun load-data! (file)
  (-> file
      utils|slurp!))

(defun parse! (datastream)
  (-map #'parse-round
        (split-string datastream "\n" t)))

(defun simulate (stream)
  (->> stream
       (-map #'(lambda (rnd)
                 (apply #'round-score rnd)))
       -sum))

;; part one
(-> "./day2/data"
    load-data!
    parse!
    simulate)

;; part two
(defun parse-best-move (code)
  (pcase code
    ("X" :lose)
    ("Y" :draw)
    ("Z" :win)
    (else (error "parse-needed-move encountered `%s'" else))))

(defun parse-best-round (round-str)
  "Parse a single round, converting from a string to a pair of
   opponent move and move"
  (pcase (split-string round-str " " t)
    (`(,opp-move ,move) (list (parse-opponent-shape opp-move) (parse-best-move move)))))

(defun parse-part2! (datastream)
  (-map #'parse-best-round
        (split-string datastream "\n" t)))

(defun best-move (opp-move outcome)
  (pcase (list opp-move outcome)
    (`(:paper :win)    :scissors)
    (`(:rock  :win)    :paper)
    (`(:scissors :win) :rock)
    (`(:paper     :lose) :rock)
    (`(:rock      :lose) :scissors)
    (`(:scissors  :lose) :paper)
    (`(,a     :draw)   a)))

(defun best-round-score (opp-move outcome)
  (let ((move (best-move opp-move outcome)))
    (round-score opp-move move)))

(defun simulate-part2 (stream)
  (->> stream
       (-map #'(lambda (rnd)
                 (apply #'best-round-score rnd)))
       -sum))

;; part two answer
(-> "./day2/data"
    load-data!
    parse-part2!
    simulate-part2)

;; alright enough of this insanity, time to solve with higher ordered functions so part two is fast!!

(provide 'day2)
;;; day2.el ends here
