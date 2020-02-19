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

## `nose-sample`

Returns a sample of elements from a collection.
These elements are collected into a list and has a length of the
amount given.
If `amount` is nil, only one element is collected.

For collecting a single element, it's better to use
[`nose-pick`](#nose-pick) unless you need it in a list.

### Usage ###

~~~emacs-lisp
(nose-sample COLLECTION &optional AMOUNT)

;; Let's say I'm some hideous dæmon with three swords that can perform
;; three attacks per round.
;; I wonder who shall fall victim to my thirsty blade...

(let ((party '(fighter thief magic-user dwarf elf)))
  (nose-sample party 3))
;; => (magic-user elf elf), for example

;; Hope you have good evasion saving rolls, puny elf!
~~~
