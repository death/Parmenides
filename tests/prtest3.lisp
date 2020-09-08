#|
I fixed this file so that it compiles properly, however I think this might have been a draft version
an attempt to sort of develop by testing, because the syntax of each 'def-frame' was incorrect.
I think we can probably just delete this file.
|#


(in-package :parmenides-test)

(def-frame living-thing (extendable t propagate t)
  breathes (value 'yes)
  eats regularly)

(def-frame person  (is-a living-thing extendable t propagate t if-added foo)
  legs (value 2)
  talks (value 'yes))
(setq p1 (make-person 'p1 :talks '(value no)))

(def-frame child (is-a person extendable t propagate t)
  talks (value 'no)
  works (value 'no))

(setq child1 (make-child 'child1 :talks '(value yes)))

(def-frame diversion (extendable t)
  fun (value 'usually))

(def-frame baby (is-a (person diversion) extendable t)
  cries (value 'sometimes))


