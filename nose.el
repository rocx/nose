;;; nose.el --- Pick and select elements from lists

;; Copyright (C) 2020 Box O'Rocks

;; Author: Box O'Rocks <rocx@rocx.rocks>
;; Version: 0.1.0
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

;;; nose.el ends here

(defun nose-random-index (collection)
  "Returns a number between 0 and the length of COLLECTION."
  (random (length collection)))

(defun nose-pick (collection)		; heh heh heh
  "Returns a random element from COLLECTION."
  (elt collection (nose-random-index collection)))

(provide 'nose)
