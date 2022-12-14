;;; 5.el --- problem 5 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jeff Young
;;
;; Author: Jeff Young <jeff@doyougnu.xyz>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: December 11, 2022
;; Modified: December 11, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/doyougnu/5
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  problem 5
;;
;;; Code:

(require 'dash)
(require 'utils)


(defun load-data! (file)
  (utils|slurp! file))

(defun parse-data (stream)
  (split-string stream "\n" t))

(defun parse-move (str)
  (let ((regex (rx (seq "move" space (group (+ num)) space
                        "from" space (group (+ num)) space
                        "to"   space (group (+ num))))))
    (when (string-match regex str)
      (list :amount (string-to-number (match-string 1 str))
            :from   (string-to-number (match-string 2 str))
            :to     (string-to-number (match-string 3 str))))))

(setq start-supply
  (vector '(:w :r :t :g)
          '(:w :v :s :m :p :h :c :g)
          '(:m :g :s :t :l :c)
          '(:f :r :w :m :d :h :j)
          '(:j :f :w :s :h :l :q :p)
          '(:s :m :f :n :d :j :p)
          '(:j :s :c :g :f :d :b :z)
          '(:c :l :w :n :h)))


(defun pop! (list)
  (pop list))

(defun get-stack (num supply)
  (aref supply (1- num)))

(defun replace-stack! (num supply stack)
  (aset supply (1- num) stack))

(defun stack (supply crate num)
  (let ((stack (get-stack num supply)))
    (replace-stack! num
                    supply
                    (cons crate stack))))

(defun move (supply &key from &key to)
  (let* ((from-stk (get-stack from supply))
         (to-stk   (get-stack to   supply))
         (crate    (pop! from-stk)))
    (stack supply crate to)))

(pop! (get-stack 1 start-supply))

(move start-supply :from 2 :to 1)

(message "%s" start-supply)


;;; 5.el ends here
