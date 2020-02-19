# 👃☝ Nose - Picker of Lists

Nose is a small, freshly-shaven yak to grab random elements from a
list without invoking either the `cl-lib` or `seq` libraries.
Why?  You're asking the wrong question.
Why not?  That's an even worse question because I don't have an answer!

# API #

## `nose-random-index`

Returns a number between 0 and the length of the collection.

### Usage ###

~~~emacs-lisp
(nose-random-index COLLECTION)

(mapcar #'nose-random-index (make-list 4 (number-sequence 0 9)))
;; => (0 8 9 3), for example
~~~

## `nose-pick`

Returns a random element from the collection.

### Usage ###

~~~emacs-lisp
(nose-pick COLLECTION)

;; Example #1: Pick a number between 1 and 10.

(nose-pick (number-sequence 1 10))
;; 4, for example.

;; Example #2: Polyhedral die roller

(defun d* (sides &optional amount)
  "Return a sequence of die rolls between 1 and SIDES."
  (let ((die (number-sequence 1 sides)))
    (mapcar #'nose-pick (make-list (or amount 1) die))))
;; => d*

(defun d (sides &optional amount)
  "Return the sum of the dice rolled."
  (apply #'+ (d* sides amount)))
;; => d

(d 6 3) ; Roll 3d6, or three six-sided dice and tally the sum.
;; => 13, for example
~~~
