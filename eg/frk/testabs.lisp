(use-package 'frk)

(LITERALIZE IS-OF-CLASS ()
  :ARG1 NIL :ARG2 NIL)
(LITERALIZE IS-A ()
  :ARG1 NIL :ARG2 NIL)
(LITERALIZE HOLDING-TOOL ()
  :ARG1 NIL :ARG2 NIL)


(RULE testabs
 :LHS
  ((GOAL (LABEL =GOAL6823) :ARG5 =HOLE-DIAMETER :ARG4 =HOLE-DEPTH :ARG0
       IS-TAPPED :ARG1 =PART :ARG2 =HOLE :ARG3 =SIDE :ARG6 =LOC-X :ARG7 =LOC-Y)
   (HOLDING :ARG1 =MACHINE :ARG2 =HOLDING-DEVICE :ARG3 =PART :ARG4
            =SIDE (CHECK =SIDE))
   (<ABS> (is-a :arg1 =side :arg2 =side))
   (is-of-class :arg1 =side))
;;   (<ABS>
;;    (IS-REAMED :ARG1 =PART :ARG2 =HOLE :ARG3 =SIDE :ARG3 =SIDE :ARG4
;;             =HOLE-DIAMETER :ARG5 =LOC-X :ARG6 =LOC-Y (LABEL =IS-REAMED)))
;;   (GOAL (LABEL =GOAL6816) :ARG8 =COUNTERBORE-SIZE :ARG0
;;       IS-COUNTERBORED :ARG1 =PART :ARG2 =HOLE :ARG3 =SIDE :ARG4
;;       =HOLE-DEPTH :ARG5 =HOLE-DIAMETER :ARG6 =LOC-X :ARG7 =LOC-Y)
 :RHS
  ())
