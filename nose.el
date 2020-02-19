;;; nose.el --- Pick and select elements from lists

;; Copyright (C) 2020 Box O'Rocks

;; Author: Box O'Rocks <rocx@rocx.rocks>
;; Version: 0.1.7
;; Homepage: https://github.com/rocx/nose

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Nose is a small, freshly-shaven yak to grab random elements from a
;; list without invoking either the cl-lib or seq libraries.
;; Why?  You're asking the wrong question.
;; Why not?  That's an even worse question because I don't have an answer!

;;; Code:

(defun nose-random-index (collection)
  "Returns a number between 0 and the length of COLLECTION."
  (random (length collection)))

(defun nose-pick (collection)		; heh heh heh
  "Returns a random element from COLLECTION."
  (elt collection (nose-random-index collection)))

(defun nose-sample (collection &optional amount)
  "Collect an AMOUNT of elements from COLLECTION with replacement.
If AMOUNT is nil, only collect one element.

For collecting a single element, `nose-pick' it is advised to use
`nose-pick' instead."
  (mapcar #'nose-pick (make-list (or amount 1) collection)))

(defun nose-subseq (collection start &optional end)
  "Return a subsequence of the COLLECTION from START to END.
If END is nil, return the collection starting from the index START.
Does not function with negative numbers like `cl-subseq' can."
  (if (null end) (nthcdr start collection)
    (butlast (nthcdr start collection) (- (length collection) end))))

(defun nose-remove-nth (collection index)
  "Remove the Nth element from COLLECTION."
  (if (zerop index) (cdr collection)
    (append
     (nose-subseq collection 0 index)
     (nose-subseq collection (1+ index)))))
     
(provide 'nose)

;;; nose.el ends here
