;;; English version of FRulekit messages

(in-package :parmenides)

(put-messages
 :eng
 :go-on "Go on."
 :agenda-y "~%Basic FRulekit loaded. Do you want to load the agenda module?: "
 :trace-y "Do you want to load the trace module?: "
 :frulekit-loaded "~%FRulekit loaded. It is all yours."
 :no-such-rule "No such rule ~S, type *RULE-NAMES* for list of rule names~%"
 :build-initialized "Build initialized~%"
 :recompiling-rule "~%Re-compiling rule ~a..."
 :compiling-rule "~%Compiling rule ~a..."
 :obsolete "~%Error, ~S is obsolete, you must use the new sytnax."
 :slot-not-defined "~%%Warning: slot ~S is not defined by literalize for class ~A"
 :sharing-alpha-test "~%Sharing alpha node test: ~A"
 :sharing-beta-test "~%Sharing beta node test: ~A"
 :lisp-check-any-variables "~%%Warning: lisp check ~A doesn't check any variables!~%"
 :isomorphic "~%[LHS of ~A is isomorphic to ~A]~%"
 :rule-not-completely-compiled "~% [Rule was not completely compiled, can't completely excise.]"
 :not-completely-excised "[Not completely excising since other rules are isomorphic to it.]"
 :excised-rule "~%Excised rule ~A"
 :seconds "~,4F seconds of run time~%"
 :interpreter-initialized "~%Interpreter initialized"
 :halt-r "~%Halting, type :R to continue."
 :halt-go "~%Halting, type GO to resume, Q to get to top-level..."
 :halt-cont "~%Halting, type (continue) to resume."
 :halt-ok "~%Halting, type OK to continue."
 :halting "~%Halting..."
 :legal-breakpoint-values "Sorry. Legal breakpoint values are :BEFORE, :AFTER, :ALWAYS and NIL"
 :no-productions "~%[no productions applicable]~%"
 :cycle "~%--- Cycle ~A ---~%"
 :no-cr-strategies "No more cr-strategies, picking 1st instant arbitrarily~%"
 :cr-strategy "Trying cr strategy ~A~%"
 :in-the " in the ~:R <OR>"
 :firing-production "~% ----------> Firing production ~A~%"
 :with-values " with values:~%"
 :breakpoint-before "~%>>>Breakpoint for rule ~S before firing:~%"
 :breakpoint-after "~%<<<Breakpoint for rule ~S after firing:~%"
 :adding-wme "~%Adding wme "
 :no-rules-class "[no rules recognize the class ~a]~%"
 :remove-no-productions "[remove: no productions apply to wme: "
 :removing-wme "~%Removing wme "
 :modifying-wme "%Modifying wme:"
 :mip-wme "~%Modifying in place wme:"
 :new-slots "New slots are: ~a~%"
 :no-or "[There is no ~:R <OR> in rule ~S]~%"
 :no-disjunct "[There is no ~:R disjunct in the ~:R <OR> of rule ~S]~%"
 :building "~%Building..."
 :inhibiting-rule "~%Inhibiting rule ~A"
 :uninhibiting-rule "Uninhibiting rule ~A~%"
 :added-to-cs "~%>>>Added to conflict-set ~A~%"
 :removed-from-cs "~%<<<Removing from conflict-set ~A~%"
 :token-tested-t "~% Token ~A~% tested successfully by node ~A"
 :only-stored "Only stored ~S cycles, sorry.~%"
 :record-level "Sorry, *RECORD-LEVEL* is ~S but must be at least 1.~%"
 :no-firings-before "[No firings before this.]"
 :no-such-wme "No such wme. ~%"
 :no-such-variable "No variable named ~S in that instantiation."
 :time "%Time:~A"
 :creator " Creator:"
 :instantiation "Instantiation of rule ~A~%"
 :matched-wme  "Matched WMEs:"
 :rete-node "Rete Node~%"
 :type " Type:"
 :left-memory " Left Memory: ~A~%"
 :right-memory " Right Memory: ~A~%"
 :output-memory " Output Memory: ~A~%"
 :rule-wont-fire "Rule ~A matched but won't fire if~%the c.r. strategy picks a different rule to fire.~%"
 :rule-not-compiled "Rule ~A has not been compiled!~%"
 :rule-matched "Rule ~A has matched and fired~%and has been removed from the conflict set.~%"
 :conde "The ~:R condition element in rule ~S~%"
 :not-match "does not partially match.~%"
 :match-but-var-binding "partially matches but there is a variable-binding~%"
 :check-problem "or CHECK problem.~%"
 :conde-1 "~%Conde ~S: ~%"
 :disjunctive "[Disjunctive CE ~2,1F]"
 :conde-2 "~%Conde ~2,1F:~%"
 :no-instantiations "There are no instantiations of the rule ~A~%"
 :1-instantiations "There is only 1 instantiation of the rule ~A~%"
 :not-wme-class "~A is not a WME class.~%"
 :no-rules-try-instances-of "~%No rules test WMEs of the ~S class, try INSTANCES-OF~%"
 :not-defined "~%[Not defined]"
 :is-disjunct " ----------> It is the ~:R disjunct"
 :was-disjunct  "<<<It was the ~:R disjunct"
 :ignore-literalize  "Ignore this literalize"
 :reserved-clisp-type "~%Error in literalize: ~a is a reserved Commonlisp type"
 :ignore-remove  "Ignore that $remove command"
 :non-variable "$Remove: tried to remove a non-variable~%"
 :var-not-wme "$Remove: got variable bound to ~a, which is not a WME.~%"
 :rete-changed "Can't back up because the rete net has been changed."
 :unknown-type  "Internal error, unknown node type ~S~%"
 :illegal-node-type "Internal error in right-beta-push-through, illegal node type ~S~%"
 :bottom-not-negative  "The bottom disj node shouldn't be negative"
 :inappropriate-type  "Internal error, inappropriate node type ~S~%"
 :return-nil  "Return NIL"
 :not-in-rhs "Not in the RHS of a disjunctive rule."
 :var-no-binding "RK variable ~S has no binding in rule ~S"
 :var-no-binding-1 "RK variable ~S has no binding"
 :no-rhs-to-compile "Rule ~A has no right-hand-side to compile!"
 :return-nil-nil  "Return (values NIL NIL)~%"
 :var-not-in-production "varname ~A is not in production ~A"
 :try-next  "Try next instant"
 :var-different "Var ~A in the ~:R instant is different"
 :continue-compilation  "Continue compilation, even though the rule won't work"
 :first-not-abs "The first condition element may not be an absence test."
 :ignore-conde  "Ignore that Condition Element"
 :not-nested-or "Nested <OR>'s are not currently allowed"
 :ignore-negation  "Ignore the rest of the negation in rule ~S"
 :not-negated-conjunctions "Negated conjuctions are not currently allowed"
 :keep-compiling  "Keep trying to compile rule."
 :not-a-class "~A is not a class defined by LITERALIZE"
 :ignore-lisp-var  "Ignore lisp var."
 :cant-put "Can't put lisp var. ~S in first CE into beta test."
 :ignore-command "Ignore the command ~A."
 :illegal-special-cmd "Illegal special cmd ~A, must be CHECK, BIND or LABEL~%"
 :expected-closing "Expected closing ']' in '[' cmd, got: ~S"
 :ignore-bind "Ignore the (BIND ~S ...) command."
 :only-16-bind "Only 16 BIND commands allowed per rule, complain to the authorities."
 :illegal-bind "Illegal BIND command: variable ~S is already bound."
 :ignore-mbind  "Ignore this (MBIND ~S ...) command"
 :slot-facet-not-in-frame "Slot and facet ~A and ~A are not in frame ~A"
 ;; Agenda
 :rule-tested "~%Rule ~A tested ~A. "
 :rule-applied "~%Rule ~A applied ~A. "
 :contents-1 "~%Contents:  ~A ~%"
 :instantiations-1 "~S Instantiations"
 :instantiations-2 "~S instantiations in new bucket ~S.~%"
 :agenda-ptr "~%Agenda-ptr is: ~A "
 :bucket-ptr "~%Bucket-ptr is: ~A "
 :rules-tested "~% ~A rules tested,  ~A rules fired in ~5F CPU seconds. ~%"
 :adding-to-bucket "~%Adding ~A to bucket ~A"
 :deleting "~% Deleting: ~A ~%"
 :illegal-agenda-pos "~% Illegal agenda-pos: ~A ~%"
 :adding-bucket "~%Adding bucket ~A at position: ~A"
 :agenda-pos-not-found "~%Agenda position ~A not found~% "
 :deleting-buckets "~%Deleting buckets: ~A~% "
 :rule-removing "~%Rule ~A removing itself from bucket ~A "
 :rule-in-bucket-not-defined "Rule ~A in bucket ~A has not been defined by RULE.~%"
 :rule-list "Rules must be a list, ~A isn't a list."
 :illegal-bucket-pos  "Add-rule got illegal bucket position, ~S"
 :rule-not-in-bucket  "Rule ~S given to add-rule is not in the bucket"
 ;; Trace
 :tracing-activated "Tracing activated~%"
 :only-positive-wmes "Sorry, ~A can only handle positive WMEs for now.~%"
 :ignoring-test "[Ignoring test ~A ~A]~%"
 :wme-list  "%~A: ?WME ~A must be a list whose car is either POS or ABS~%"
 :not-implemented  "Not implemented~%"
 :only-condes  "there are only ~A condes in ~A, asked for ~A")
