;;; -*- Mode:Lisp; Package:Parmenides -*-

;;;;    PARMENIDES -- A hierarchical class-based frame language.

;;; Copyright (c) 1985, 1989 Peter Shell and Carnegie Mellon University.
;;; To obtain a copy of this software, please contact:
;;;     Peter Shell
;;;     School of Computer Science
;;;     Carnegie Mellon University
;;;     Pittsburgh, PA  15213
;;;
;;; The Parmenides user's manual is available in WeH 7121 or at the CMT
;;; and on-line as: /afs/cs/usr/pshell/parmenides/parmenides.doc.
;;; Implemented in CommonLisp for RuleFermi starting 17-May-86.
;;; This file should be loaded before it is compiled.

;;;     Some of the original Framekit functionality is preserved;
;;;     however the internals have been re-designed and re-implemented, so
;;;     this version will bear as much similarity to Loops as to Framekit.
;;;     It is also influenced by the Slisp implementation of Defstruct,
;;;     but it is portable.
;;;     Parmenides is faster because it represents frames with arrays, and
;;;     so is able to define pre-compiled slot and facet accessor functions
;;;     for each slot or facet.  See the manual for complete details.
;;;
;;; Source file:  /../ml/usr/pshell/parmenides/parmenides.lisp
;;; NOTE: This file should be loaded before it is compiled.
;;; -------------------------------------------------------------------- ;;;
;;;                          --- CHANGE LOG ---
;;;
;;;   9-Dec-92  Pete Shell.  Added the :facets c-slot.  If its value is :NO
;;;             then none of the class's slots have facets, even if their
;;;             values are lists.
;;;
;;;   21-Oct-92  Pete Shell.  Optimized write-accessor-functions and
;;;              write-facet-refs.
;;;
;;;   6-Dec-89   Daniel Borrajo.  Added a freelist module.
;;;
;;;   15-May-89  Added a special called 'oldval' to the variables which
;;;              are accessible by demons.
;;;
;;;   11-Apr-89  Fixed a bug in maybe-fplist-append which caused Parmenides
;;;              to break when defining a sub-class of a class which has
;;;              the propagate class facet, and a slot is being redefined.
;;;
;;;   1-Mar-89   Daniel Borrajo. Also for version 1.5.
;;;              Added multi-lingual capability.  Virtually
;;;              all output to the user is done via ml-format, ml-cerror or
;;;              ml-error.  These macros take a symbol naming the message to
;;;              print, and find the corresponding message string in the
;;;              correct language.  Now the installer has to define
;;;              *PA-PATHNAME*, in this file, so that Parmenides knows where
;;;              to load the message files for each language.
;;;   6-Feb-89   For version 1.5: Added ability to Parmenides to be in its
;;;              own package.
;;;              Renamed class-of to pa-class-of to avoid interference with
;;;              the clx class-of.
;;;              Changed *ALL* to :*ALL*.
;;;   9-Mar-88   When inheriting a slot from a super-frame, def-frame will
;;;              now inherit all facets in superframe not in inherited slot.
;;;   8-Mar-88   Made remove-frame mark it removed by sticking a NIL in the
;;;              class position.  Took away :frame-type and :framep plist
;;;              properties for instances.
;;;   20-May-87  Extended make-frame to inherit for non-faceted slots too.
;;;   15-May-87  Added :cache keyword arg to add-slot. Propagates to subclasses
;;;              and adds added slotname to cache list if cache is true.
;;;              Implemented get-immediate-facet, get-immediate-slot.
;;;              Fixed local-p.
;;;   29-Apr-87  Added support for inverse relations, implemented add-slot,
;;;              add-cslot.
;;;   23-Feb-87.  Changed def-frame so it no longer takes a <parents>
;;; argument but instead the parents are specified through the is-a cslot.
;;; Also, NIL now means facetless slots.
;;;   24-Jan-87. Added optional specification of inheritance type for slots
;;; specified in the slots-inherited slot of relations.  See the part-of
;;; relation for examples.
;;;   7-Jan-87. Now eval'ing default values at make time instead of def-frame
;;; time.  A side-effect of this is that default values are stored UN-evaled
;;; with the class, so get-facet will return the UN-evaled value for classes.
;;; Get-facet2 and get-value2 were thus added to make this transparent:
;;; if the frame accessed is a class, then evaluate the filler.
;;;   29-Nov-86.  Took out the extendable class facet.  Now everything is
;;; extendable.  This eliminates the need for special treatment of
;;; the extendable flag as well as having to pre-define all those svref fns.
;;;   27-Sep-86.  Added a setfable class facet.  Now Parmenides will define the
;;; methods only if setfable is T (by default NIL).
;;;   25-Sep-86.  Took out setf methods since setf using them is as
;;; slow as using set-facet or set-slot, and the expansion of them take up
;;; so much space since there are no pre-defined setting functions for facets,
;;; and pre-defining them would take up too much room.
;;;   7-SEP-86  PShell.  Made def-frame not define facet accessors and setf
;;;     methods for other than the first facet, since it takes up so much
;;;     space and time. Also took out setf and access functions for class
;;;     facets.  Added make-frame, make-frame0 and make-slot.
;;;   17-MAY-86  PShell Created

;;; TO DO:
;;; Fix get-generic-value so it finds out from the class if the slot is
;;;   really faceted or not.
;;; Document set-inverse-isas
;;; Remove old class if it gets re-defined.
;;; Make everything work compiled.  With complicated inheritance and inverse
;;;    relations, compiling a frame file won't work without first loading the
;;;    source frame file.
;;; Either warn about or let it work with, lispy frame names.
;;; Make add-slot handle inverses too.
;;;  make make-frame0 work with (unfaceted) *ALL* for slots-inherited slot.
;;;        document optional specification of combo-type.
;;; Add an if-made facet to compute the value of a slot when the instance
;;;    is made.  The computed value would be stored in the value facet.
;;; Index-plist stuff should be replaced by something more efficient.

;;;; -------------------------------------------------------------------- ;;;
;;;; PACKAGE STUFF, with help and coaxing from Todd Kaufmann
;;;;

(in-package :parmenides)

;;;; -------------------------------------------------------------------- ;;;
;;;; MULTI-LINGUA MODULE, STARTED BY Daniel Borrajo
;;;;

;;; This module defines the operators and variables needed to
;;; configure the multilingual capability of Parmenides.

;;;
;;; IMPORTANT: In order to select a language, use SET-LANGUAGE.
;;; English is the default language.
;;;

;;; *LANGUAGE* can be :eng (english), :esp (espagnol), :deu (deutsch),
;;; :fra (francais), :por (portugues), :ita (italiano), ....
;;;
;;; PUT-MESSAGES can be used to define messages for a given language.
(defvar *language* :eng)

(defvar *language-table*
  (make-hash-table))

(defun get-messages (&optional (language *language*))
  (or (gethash language *language-table*)
      (setf (gethash language *language-table*)
            (make-hash-table))))

;;; The hash table containing the messages.
(defvar *messages*
  (get-messages))

(defun set-language (&optional (language *language*))
  (setf *language* language)
  (setf *messages* (get-messages))
  language)

;;; Functions to access a message and to add a message to *MESSAGES*
(defun get-message (msgname)
  (or (gethash msgname *messages*)
      (gethash msgname (get-messages :eng))
      (format nil "~S" msgname)))

(defun put-message (msgname message)
  (setf (gethash msgname *messages*) message))

(defun put-messages (language &rest plist)
  (let ((*messages* (get-messages language)))
    (do ((msgname (pop plist) (pop plist))
         (message (pop plist) (pop plist)))
        ((or (null msgname) (null message)))
      (put-message msgname message))))

;;;; -------------------------------------------------------------------- ;;;
;;;; SPECIAL VARIABLES AND MACROS
;;;;

;;; Declaim the following symbols as dynamically scoped so the demons
;;; can get the bindings when they fire...
(declaim (special framename slotname facetname frame snum facetnum newval oldval))

(defvar *PARMENIDES-TERSE* NIL) ;;If true, then suppress Parmenides messages.

(defvar *THINGS-TO-EVAL*)       ;;For internal def-frame use.
(defvar *DEFAULT* (list NIL))   ;;Unique default value, for make-frame-slot

;;; FREELIST variables
;;
;; Variable that holds the structure of arrays for freelist. Arrays are going to
;; be normally parmenides frames or frulekit tokens.
;;
(defvar *FREE-LIST* (make-array 20 :initial-element nil :adjustable t))

;;
;; Same as before but for adjustable arrays
;;
(defvar *ADJ-FREE-LIST* (make-array 20 :initial-element nil :adjustable t))

;;; Unique variable which delimits a deleted frame.
(defvar *DELETED* (list :deleted))

;;; If T then Parmenides will make frames faster because it knows that
;;; it doesn't have to copy certain data structures, since it will assume
;;; that the user won't modify any slot values.
(defvar *READ-ONLY* NIL)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (member :phshacks *FEATURES* :test #'eq))
      (defmacro ifnotstatus (feature &rest forms)
        `(if (not (member ,feature *FEATURES* :test #'eq))
             (progn ,@forms)))))

;;; Like concat but expands what it can at expansion time and otherwise wraps
;;; a symbol-name call to function calls.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ifnotstatus :phshacks
               (defmacro smash (&rest seqs)
                 `(intern (concatenate 'string ,@(prepare-seqs seqs))))

               (defun prepare-seqs (seqs)
                 (mapcar #'prepare-seq seqs))

               (defun prepare-seq (seq)
                 (cond ((numberp seq) (princ-to-string seq))
                       ((listp seq) `(princ-to-string ,seq))
                       ((not (stringp seq)) `(smart-symbol-name ,seq))
                       (T seq)))

               (defun smart-symbol-name (thing)
                 (cond ((or (numberp thing) (listp thing))
                        (princ-to-string thing))
                       ((not (stringp thing)) (symbol-name thing))
                       (T thing)))

               (defmacro not-getf (plist prop)
                 `(eq (getf ,plist ,prop *DEFAULT*) *DEFAULT*))

               (defmacro putprop (obj val prop)
                 `(setf (get ,obj ,prop) ,val))

               (if (not (fboundp 'memq))
                   (defmacro memq (a l)
                     `(member-if #'(lambda (mumble) (eq ,a mumble)) ,l)))))

(eval-when (:load-toplevel :execute)
  (ifnotstatus :phshacks  (push :phshacks *FEATURES*)))

(defmacro my-maybe-nconc (elt list)
  `(if (not (memq ,elt ,list))
       (setf ,list (nconc ,list (list ,elt)))))

(defmacro maybe-copy-list (list)
  `(if (consp ,list)
       (copy-list ,list)
       ,list))

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; The only difference between my-pushnew and pushnew is that pushnew
;;; uses setf which uses setf methods.  Pushnew doesn't work inside
;;; set-slot since it causes an infinite recursion between set-slot and
;;; get-slot under some circumstances.
  (defmacro my-pushnew (a list)
    `(if (not (memq ,a ,list))
         (push ,a ,list)))

;;; Multilingual macros to format and errors
  (defmacro ml-format (stream msgname &rest args)
    `(format ,stream (get-message ,msgname) ,@args))

  (defmacro ml-cerror (msgname1 msgname2 &rest args)
    `(cerror (get-message ,msgname1) (get-message ,msgname2) ,@args))

  (defmacro ml-error (msgname &rest args)
    `(error (get-message ,msgname) ,@args))

  (defmacro paformat (stream msgname &rest args)
    `(if (not *PARMENIDES-TERSE*)
         (ml-format ,stream ,msgname ,@args)))

  (defmacro friendly-frame-class (frame)
    `(if (not (framep ,frame))
         (ml-format T :no-such-frame ,frame)
         (frame-class ,frame)))

;;; Generic-frame maybe be either a name or instance.
  (defmacro pa-class-of (generic-frame)
    `(friendly-frame-class (if (symbolp ,generic-frame)
                               (assure-frame ,generic-frame)
                               ,generic-frame)))

  (defmacro frame-type-of (generic-frame)
    `(name-to-frame-type (if (symbolp ,generic-frame)
                             ,generic-frame
                             (frame-class ,generic-frame))))

  (defmacro keyword-to-current (keyword)
    ;; FIXME: Won't a simple FIND-SYMBOL do?  It's also strange to
    ;; need to convert keywords to symbols in the current package.
    `(if (find-symbol (symbol-name ,keyword))
         (intern (symbol-name ,keyword))
         NIL))

  (defmacro assure-current (symbol)
    `(if (keywordp ,symbol)
         (keyword-to-current ,symbol)
         ,symbol))

  ;; Like assure-current but guarantees that it interns the symbol if
  ;; it's a keyword.
  (defmacro intern-current (symbol)
    `(if (keywordp ,symbol)
         (intern (symbol-name ,symbol))
         ,symbol))

  (defun current-to-keyword (symbol)
    (if (listp symbol)
        (let ((symbol (car symbol)));;a hack to *try* and fix an error 28/06/2020
          (intern (symbol-name symbol) (find-package "KEYWORD")))
        (intern (symbol-name symbol) (find-package "KEYWORD"))))

  (defmacro assure-keyword (symbol)
    `(if (keywordp ,symbol)
         ,symbol
         (current-to-keyword ,symbol)))

;;; If the symbol is not a relation, then it should be a keyword, otherwise
;;; it should not be a keyword.
  (defmacro correct-package (symbol)
    `(if (keywordp ,symbol)
         (let ((symbol-in-current (assure-current ,symbol)))
           (if (isa-p symbol-in-current 'relation)
               symbol-in-current
               ,symbol))
         (current-to-keyword ,symbol)))

  (defmacro assure-frame (generic-frame)
    `(let ((get-frame-instance
             (if (symbolp ,generic-frame)
                 (frame ,generic-frame)
                 (and (vectorp ,generic-frame) ,generic-frame))))
       (when (not get-frame-instance)
         (ml-cerror :ignore-frame :no-such-frame-error ,generic-frame))
       get-frame-instance))

;;; Like assure-frame but doesn't cause a cerror.  Used internally only.
  (defmacro maybe-assure-frame (generic-frame)
    `(if (symbolp ,generic-frame)
         (frame ,generic-frame)
         (and (vectorp ,generic-frame) ,generic-frame))))


(defun property-names (plist)
  (do ((slots plist (cddr slots))
       (res NIL (cons (car slots) res)))
      ((null slots) (nreverse res))))


;;; The lisps that this works under...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kyoto-lisp-p ()
    (string= (lisp-implementation-type) "Kyoto Common Lisp on MV"))

  (defun cmu-common-lisp-p ()
    (string= (lisp-implementation-type) "CMU Common Lisp"))

  (defun symbolics-lisp-p ()
    (string= (lisp-implementation-type) "Zetalisp"))

  (defun dec-lisp-p ()
    (string= (lisp-implementation-type) "VAX LISP"))

  (defun xerox-p ()
    (string= (lisp-implementation-type) "Xerox Lisp"))

  (defun lucid-lisp-p ()
    (string= (lisp-implementation-type) "Lucid Common Lisp"))

  (defun allegro-lisp-p ()
    (string= (lisp-implementation-type) "Allegro CL")))


;;;Dec lisp wrongly evaluates printer names in defstruct.
#+DEC(dolist (pname '(pa-frame-printer))
       (set pname pname))

;;; A hack around the Kyoto carriage-return bug:
#+kcl (defvar *FORMAT* (symbol-function 'format))
#+kcl (defun format (stream string &rest vars)
        (terpri)
        (apply *FORMAT* `(,stream ,string ,@vars)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *AREF-FN-NAMES*
    '(%aref-get-0 %aref-get-1 %aref-get-2 %aref-get-3
      %aref-get-4 %aref-get-5 %aref-get-6 %aref-get-7
      %aref-get-8 %aref-get-9  %aref-get-10
      %aref-get-11 %aref-get-12 %aref-get-13
      %aref-get-14 %aref-get-15))

  (defvar *AREF-FACET1-FN-NAMES*
    '(%aref-value-get-0 %aref-value-get-1 %aref-value-get-2
      %aref-value-get-3 %aref-value-get-4 %aref-value-get-5
      %aref-value-get-6 %aref-value-get-7 %aref-value-get-8
      %aref-value-get-9  %aref-value-get-10 %aref-value-get-11
      %aref-value-get-12 %aref-value-get-13 %aref-value-get-14
      %aref-value-get-15)))

(proclaim `(inline ,@*AREF-FN-NAMES* ,@*AREF-FACET1-FN-NAMES*))

;;; To save space and time.  Since lots of facet accessor functions do the
;;; same thing, you might as well make them be eq to same canonical aref
;;; accessor. (like lisp::%get).
(defun %aref-get-0 (frame) (aref frame 0))
(defun %aref-get-1 (frame) (aref frame 1))
(defun %aref-get-2 (frame) (aref frame 2))
(defun %aref-get-3 (frame) (aref frame 3))
(defun %aref-get-4 (frame) (aref frame 4))
(defun %aref-get-5 (frame) (aref frame 5))
(defun %aref-get-6 (frame) (aref frame 6))
(defun %aref-get-7 (frame) (aref frame 7))
(defun %aref-get-8 (frame) (aref frame 8))
(defun %aref-get-9 (frame) (aref frame 9))
(defun %aref-get-10 (frame) (aref frame 10))
(defun %aref-get-11 (frame) (aref frame 11))
(defun %aref-get-12 (frame) (aref frame 12))
(defun %aref-get-13 (frame) (aref frame 13))
(defun %aref-get-14 (frame) (aref frame 14))
(defun %aref-get-15 (frame) (aref frame 15))

;;; Predefine the facet access function which takes the 0th facet from a
;;; slot (this is always the value facet for Rulekit).
(defun %aref-value-get-0 (frame) (aref (aref frame 0) 0))
(defun %aref-value-get-1 (frame) (aref (aref frame 1) 0))
(defun %aref-value-get-2 (frame) (aref (aref frame 2) 0))
(defun %aref-value-get-3 (frame) (aref (aref frame 3) 0))
(defun %aref-value-get-4 (frame) (aref (aref frame 4) 0))
(defun %aref-value-get-5 (frame) (aref (aref frame 5) 0))
(defun %aref-value-get-6 (frame) (aref (aref frame 6) 0))
(defun %aref-value-get-7 (frame) (aref (aref frame 7) 0))
(defun %aref-value-get-8 (frame) (aref (aref frame 8) 0))
(defun %aref-value-get-9 (frame) (aref (aref frame 9) 0))
(defun %aref-value-get-10 (frame) (aref (aref frame 10) 0))
(defun %aref-value-get-11 (frame) (aref (aref frame 11) 0))
(defun %aref-value-get-12 (frame) (aref (aref frame 12) 0))
(defun %aref-value-get-13 (frame) (aref (aref frame 13) 0))
(defun %aref-value-get-14 (frame) (aref (aref frame 14) 0))
(defun %aref-value-get-15 (frame) (aref (aref frame 15) 0))


;;; (aref *AREF-FN-MAP* N) returns the function object which is the aref
;;; accessor for the Nth slot.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *AREF-FN-MAP*
    (make-array 16
                :initial-contents *AREF-FN-NAMES*))
  (defvar *AREF-FACET1-FN-MAP*
    (make-array 16
                :initial-contents *AREF-FACET1-FN-NAMES*))
  (defvar *AREF-SLOT-FN-PLIST* NIL))

;;; Structure for a Parmenides frame. This has not been a recording.
(defstruct pa-frame
  propagatep      ;;T iff it's a propagating kind of frame.
  index-plist     ;;nested plist associating slot, facet names w/ array indices
  slots           ;;list of all local slots, facets & default vals.
  c-slots         ;;plist of slots which are only stored with the class
  numslots        ;;used when the frame is adjustable
  name            ;;name of the frame
  snames          ;;list of slot names
  rel-slots       ;;list of slot names which are relations
  )

(defvar *MARKED-CLASSES* NIL)    ;;For marker propagation

(defvar !!inheritance-type :dfs) ;; defaults to depth-first search
;; dfs = depth first search (stopping
;; on first value list found)
(defvar !!inheritance-link :is-a)


;;; -------------------------------------------------------------------
;;;             FREELIST MODULE
;;; -------------------------------------------------------------------

;;
;; Access macros
;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro first-position (frame)
    `(aref ,frame 0))

  (defmacro get-frame-size (frame)
    `(car (array-dimensions ,frame)))

  ;;
  ;; Creates a frame in case there isn't one free
  ;;

  (defmacro create-frame (size adjustable)
    `(make-array ,size :adjustable ,adjustable))


  ;;
  ;; Macro that looks for a free frame. If there isn't, it creates and
  ;; returns it. If there is not room for frames of that size, it first
  ;; adjust the freelist array.
  ;;

  (defmacro get-free-frame (size &key adjustable)
    (let ((frame-name (gentemp "frame-")))
      `(let ((free-list (if ,adjustable *ADJ-FREE-LIST* *FREE-LIST*)))
         (if (>= ,size (get-frame-size free-list))
             (setf free-list (adjust-array free-list (1+ ,size)
                                           :initial-element NIL)))
         (let ((,frame-name (aref free-list ,size)))
           (cond (,frame-name
                  ;;              (format T "Getting a free frame of size ~S~&" ,size)
                  (setf (aref free-list ,size)
                        (first-position ,frame-name))
                  (setf (first-position ,frame-name) nil)
                  ,frame-name)
                 (T (create-frame ,size ,adjustable)))))))

  ;;
  ;; Macro that pushes a frame into the free list.
  ;;

  (defmacro release-frame (frame)
    (let ((frame-name (gentemp "frame-")))
      `(let* ((,frame-name ,frame)
              (size (get-frame-size ,frame-name))
              (free-list (if (adjustable-array-p ,frame)
                             *ADJ-FREE-LIST*
                             *FREE-LIST*)))
         (if (and (> size 1) (< size (get-frame-size free-list)))
             (if (not (eq (aref ,frame 1) *DELETED*))
                 (let ((first-free-frame (aref free-list size)))
                   (setf (aref ,frame 1) *DELETED*)
                   (setf (aref free-list size) ,frame-name)
                   (setf (first-position ,frame-name) first-free-frame)
                   size))
             (ml-format t :releasing-frame)))
      ))
  )


;;; -------------------------------------------------------------------
;;;             BASIC RETRIEVAL FUNCTIONS:
;;; -------------------------------------------------------------------

;;; For predicates which return 2 values.  For the first member of seq such
;;; that the second value of pred is not NIL, this returns the two values
;;; returned by that pred.  If it gets to the end of the sequence, it returns
;;; (values NIL NIL).
(defmacro some2 (pred seq)
  (let ((varname (gentemp "VAR")))
    `(dolist (,varname ,seq (values NIL NIL))
       (multiple-value-bind (first second)
           (funcall ,pred ,varname)
         (if second (return (values first second)))))))

;;; Returns (values value foundp)
(defun get-facet (frame slotname facetname)
  (get-instance-facet (assure-frame frame) (assure-keyword slotname)
                      (assure-keyword facetname)))

(defun get-instance-facet (frame slotname facetname)
  (let* ((framename (frame-class frame))
         (frame-type (name-to-frame-type framename))
         (slot-index (pa-frame-index-plist frame-type))
         (slot-spec (getf slot-index slotname))
         (snum (car slot-spec))
         (facetnum (if (consp (cdr slot-spec))
                       (getf (cdr slot-spec) facetname))))
    (cond ((not (consp (cdr slot-spec)))
           (ml-cerror :ignore-get-facet :slot-has-no-facets slotname framename))
          ((and snum facetnum)
           (values (aref (aref frame snum) facetnum) T))
          ((null (isas framename)) (values NIL NIL))
          (T (some2 #'(lambda (parent)  ;;depth-first inheritance
                        (get-facet parent slotname facetname))
                    (isas framename))))))

(defmacro get-value (frame slot)    ;;get value facet
  `(get-facet ,frame ,slot :value))

(defun get-immediate-facet (frame slotname facetname)
  (get-immediate-instance-facet (assure-frame frame) slotname facetname))

(defun get-immediate-instance-facet (frame slotname facetname)
  (setq slotname (assure-keyword slotname))
  (setq facetname (assure-keyword facetname))
  (let* ((framename (frame-class frame))
         (frame-type (name-to-frame-type framename))
         (slot-index (pa-frame-index-plist frame-type))
         (slot-spec (getf slot-index slotname))
         (snum (car slot-spec))
         (facetnum (getf (cdr slot-spec) facetname)))
    (cond ((and snum facetnum)
           (values (aref (aref frame snum) facetnum) T))
          (T (values NIL NIL)))))

(defmacro get-immediate-value (frame slot)
  `(get-immediate-facet ,frame ,slot :value))

(defun get-generic-value (frame sname)
  (let ((value (get-slot frame sname))
        (facetp (getf (pa-frame-c-slots (frame-type-of frame)) :facets)))
    (if (and (arrayp value)
             (not (eq facetp :NO)))
        (aref value 0)
        value)))

(defmacro get-value-demons (frame slot)
  `(get-facet-demons ,frame ,slot :value))

(defun get-facet-demons (frame sname facetname)
  (get-facet-demons0 (assure-frame frame) (if (symbolp frame) frame)
                     sname facetname))

(defun get-facet-demons0 (frame framename sname facetname)
  (let* ((frame-type (frame-type-of frame))
         (slot-index (pa-frame-index-plist frame-type))
         (slot-spec (getf slot-index sname))
         (snum (car slot-spec))
         (facetnum (getf (cdr slot-spec) facetname))
         (val (aref (aref frame snum) facetnum)))
    (if (not val)
        (let* ((demonfacetnum (getf (cdr slot-spec) :if-needed))
               (local-demon (and demonfacetnum
                                 (aref (aref frame snum) demonfacetnum)))
               (val (and local-demon
                         (let ((framename framename)   ;;local bindings for demon
                               (slotname sname)
                               (facetname facetname))
                           (eval local-demon)))))
          (if (not val)
              (let* ((c-slots (pa-frame-c-slots frame-type))
                     (class-demon (getf c-slots :if-needed))
                     (val (and class-demon
                               (let ((framename framename)
                                     (slotname sname)
                                     (facetname facetname)
                                     (frame frame))
                                 (eval class-demon)))))
                val)
              val))
        val)))

(defsetf get-slot set-slot)
(defsetf get-facet set-facet)  ;;So you can do things like push on (get-facet ..)
(defsetf get-facet-demons set-facet-demons)
(defsetf get-value set-value)

(defmacro facetedp (class sname)
  `(faceted-p2 (pa-frame-index-plist (frame-type-of ,class))
               (assure-keyword ,sname)))

(defun local-p (frame slotname)
  (multiple-value-bind (val exist)
      (get-immediate-slot frame slotname)
    (declare (ignore val))
    (and exist
         (or (not (facetedp frame slotname))
             (let ((depth (get-immediate-facet frame slotname :depth)))
               (and depth (zerop depth)))))))

(defun faceted-p2 (fi-plist sname)
  (let ((res (cdr (getf fi-plist sname))))
    (and res (consp res))))

;;; Returns the data structure associated with the particular slot/frame pair.
;;; If the slot has no facet, then it will be the value; else it will be a
;;; data structure which contains all the facets and values.
;;; Returns NIL if no such slot exists.
(defun get-slot (frame slotname)
  (get-instance-slot (assure-frame frame) slotname))

(defun get-instance-slot (frame slotname)
  (setq slotname (assure-keyword slotname))
  (let* ((frame-type (name-to-frame-type (frame-class frame)))
         (slot-index (pa-frame-index-plist frame-type))
         (slot-spec (getf slot-index slotname))
         (snum (car slot-spec)))
    (cond (snum (values (aref frame snum) T))
          (T (let ((isas (isas (frame-class frame))))
               (if isas (some2 #'(lambda (parent)
                                   (get-slot parent slotname))
                               isas)
                   (values NIL NIL)))))))

(defun get-immediate-slot (frame slotname)
  (get-immediate-instance-slot (assure-frame frame) (assure-keyword slotname)))

(defun get-immediate-instance-slot (frame slotname)
  (let* ((frame-type (name-to-frame-type (frame-class frame)))
         (slot-index (pa-frame-index-plist frame-type))
         (slot-spec (getf slot-index slotname))
         (snum (car slot-spec)))
    (cond (snum (values (aref frame snum) T))
          (T (values NIL NIL)))))


;;; Returns (values <slotnum> <facetnum>)
;;; sname and facetname must be keywords
(defun pa-get-snf-nums (classname sname facetname)
  (let* ((frame-type (name-to-frame-type classname))
         (slot-spec (getf (pa-frame-index-plist frame-type)
                          (assure-keyword sname)))
         (snum (car slot-spec))
         (facetnum (getf (cdr slot-spec) (assure-keyword facetname))))
    (values snum facetnum)))

(defun get-slot-num (fi-plist sname)
  (car (getf fi-plist sname)))

(defun get-cslot (generic-frame slotname)
  (let* ((frame-type (frame-type-of generic-frame))
         (slot-plist (pa-frame-c-slots frame-type)))
    (getf slot-plist (assure-keyword slotname))))


;;; -----------------------------------------------------------------------
;;;             BASIC STORAGE FUNCTIONS:
;;; -----------------------------------------------------------------------

(defun add-to-facet (frame slotname facetname filler) ;Adds filler to facet
  (push filler (get-facet frame slotname facetname)))

(defun add-to-facet-demons (frame slotname facetname filler) ;Adds w/demons
  (push filler (get-facet-demons frame slotname facetname)))

(defun add-to-value (frame slot filler) ; Adds filler to the value facet
  (push filler (get-facet frame slot :value)))

(defun add-to-slot (frame slotname filler)
  (push filler (get-slot frame slotname)))

(defun maybe-add-to-slot (frame slotname filler)
  (my-pushnew filler (get-slot frame slotname)))

(defun add-to-cslot (frame slotname filler)
  (let* ((real-frame (assure-frame frame))
         (frame-type (if real-frame (frame-type-of real-frame)))
         (slot-plist (if frame-type (pa-frame-c-slots frame-type)))
         (real-slotname (assure-keyword slotname)))
    (when real-frame
      (cond ((not (classp real-frame))
             (ml-error :no-such-class frame))
            ((not-getf slot-plist real-slotname)
             (ml-cerror :ignore-set-cslot :no-cslot-in-class slotname frame))
            (T
             (push filler
                   (getf (pa-frame-c-slots frame-type) real-slotname)))))))

;;;; Set-facet

;;; Since the user is setting a facet, it is assumed that the given slot has
;;; facets.  Every slot that has facets has a :value facet.  If it is
;;; propagatable, then it also has a :depth facet.

;;; Works for both classes and instances.  Propagates the value down is-a and
;;; instance links if frame is a class.
;;; Fires no demons.  Propagates to other instances which the frame has
;;; an invertible relation to.
(defun set-facet (frame slotname facetname newval)
  (init-propagate)
  (set-facet-demons0 (if (symbolp frame) frame)
                     (assure-frame frame) slotname facetname newval 0 NIL))

(defun set-facet-demons (frame slotname facetname newval)
  (init-propagate)
  (set-facet-demons0 (if (symbolp frame) frame)
                     (assure-frame frame) slotname facetname newval 0 :demons))

(defmacro set-value (frame slotname newval)
  `(set-facet ,frame ,slotname :value ,newval))

(defmacro set-value-demons (frame slotname newval)
  `(set-facet-demons ,frame ,slotname :value ,newval))

;;; Frame is guaranteed to be the frame; framename may be NIL
(defun set-facet-demons0 (framename frame slotname facetname newval depth demonsp)
  (setq slotname (assure-keyword slotname))
  (setq facetname (assure-keyword facetname))
  (let* ((frame-type (frame-type-of frame))
         (slot-index (pa-frame-index-plist frame-type))
         (slot-spec (getf slot-index slotname))
         (snum (car slot-spec))
         (fplist (cdr slot-spec))
         (facetnum (and (consp fplist) (getf fplist facetname)))
         (postfacetnum (and demonsp (getf fplist :post-if-set)))
         (prefacetnum (and demonsp (getf fplist :pre-if-set)))
         (dfacetnum (and facetnum (getf fplist :depth)))
         (propagatep (pa-frame-propagatep frame-type)))
    (cond ((or (null snum) (null facetnum))
           (ml-cerror :dont-set :slot-not-facet
                      (frame-class frame) slotname facetname))
          (T
           (when (classp frame)
             (modify-facet-plist frame-type slotname facetname newval)
             (if propagatep
                 (modify-facet-plist frame-type slotname :depth depth)))
           (set-class&instances-facet frame framename slotname facetname snum
                                      facetnum newval depth dfacetnum
                                      prefacetnum postfacetnum demonsp propagatep)
           (if (and (classp frame) propagatep
                    (memq slotname (get-cslot frame :cache)))
               (propagate-down frame slotname facetname newval
                               (1+ depth) demonsp))
           newval))))

(defun funcall-when-list (fun &rest args)
  (if (listp fun)
      (apply #'funcall (first fun) args)
      (apply #'funcall fun args)))

;;; Instances have the property that all of their slot nums and facet nums are
;;; the same as their class, since they are an instance of only one class.
;;; Algorithm:
;;; (1) fire pre-set demons if demonsp is true and there are any.
;;; (2) set the desired facet in the frame
;;; (3) fire post-set demons if demonsp is true and there are any.
;;; (4) do the same for any/all instances
(defun set-class&instances-facet (frame framename sname fname snum fnum newval
                                  depth dfnum prefacetnum postfacetnum
                                  demonsp propagatep)
  (if (not (and snum fnum))     ;;This should never happen
      (ml-error :set-slot-facet
                sname fname (frame-class frame)))
  (let ((oldval (aref (aref frame snum) fnum)))
    (when demonsp
      (if prefacetnum   ;;if there is an if-added demon on the slot
          (let ((demon (aref (aref frame snum) prefacetnum))
                (framename framename)
                (slotname sname)
                (facetname fname))
            (eval demon)))
      (if (not (eq demonsp :slots-only))
          (let ((cdemon (get-cslot frame :pre-if-set)))
            (if cdemon
                (let ((framename framename)
                      (frame frame)
                      (slotname sname)
                      (facetname fname))
                  (eval cdemon))))))
    (if dfnum (setf (aref (aref frame snum) dfnum) depth))
    (setf (aref (aref frame snum) fnum) newval)
    (handle-set-inverse-facet frame framename newval sname fname snum fnum)
    (when demonsp
      (if postfacetnum  ;;if there is an if-added demon on the slot
          (let ((demon (aref (aref frame snum) postfacetnum))
                (framename framename)
                (slotname sname)
                (facetname fname))
            (eval demon)))
      (if (not (eq demonsp :slots-only))
          (let ((cdemon (get-cslot frame :post-if-set)))
            (if cdemon
                (let ((framename framename)
                      (frame frame)
                      (slotname sname)
                      (facetname fname))
                  (eval cdemon))))))
    (if (and (classp frame) propagatep)
        (let ((depth (1+ depth)))          ;;instances are 1 away from the class.
          (dolist (instance (maybe-instances-of framename))
            (if (>= (aref (aref instance snum) dfnum) depth)
                (set-class&instances-facet instance NIL sname fname snum fnum
                                           newval depth dfnum prefacetnum
                                           postfacetnum demonsp propagatep)))))))

(defun handle-set-inverse-facet (frame framename newval sname fname snum fnum)
  ;;  (if (and (not framename) (not (listp newval)))
  ;;      (setq newval (assure-frame newval)))
  (let ((usname (assure-current sname)))
    (if (and newval (invertible-relation usname))
        (let ((invname (get-slot usname :inverse-name)))
          (cond ((consp newval)
                 (dolist (oneval newval)
                   (set-inverse-facet (or framename frame) oneval sname
                                      invname fname)))
                (T
                 (setf (aref (aref frame snum) fnum) (list newval))
                 (set-inverse-facet (or framename frame) newval sname invname
                                    fname)))))))

(defun set-inverse-facet (frame val sname invname fname)
  (if (not (symbolp val))
      (setq frame (assure-frame frame)))
  (if (not (framep val))
      (if (or (not (symbolp val))
              (and (not (get val :classp))
                   (not (get val :frame))))
          (paformat t :no-frame-but-relation
                    val frame sname))
      (multiple-value-bind (invval exist) (get-facet val invname fname)
        (if (not exist)
            (paformat t :inverse-slot-facet
                      invname fname (pa-class-of val))
            (if (listp invval)
                (my-pushnew frame (get-facet val invname fname))
                (if (not (eq invval frame))
                    (set-facet val invname fname (list frame invval))))))))

(defun propagate-down (class slotname facetname newval depth demonsp)
  (ecase !!inheritance-type
    ;;    (:bfs (propagate-down-bfs class slotname facetname newval depth demonsp))
    (:dfs (propagate-down-dfs class slotname facetname newval depth demonsp))))

(defun propagate-down-dfs (class slotname facetname newval depth demonsp)
  (dolist (descendant (inverse-isas class))
    (when (and (pa-frame-propagatep (name-to-frame-type descendant))
               (member slotname (get-slot-names descendant))
               (>= (get-facet descendant slotname :depth) depth)
               (not (marked-p descendant)))
      (mark-classname descendant)
      (set-facet-demons0
       descendant (frame descendant) slotname facetname newval depth demonsp))))

(defun modify-facet-plist (classtruct slotname facetname newval)
  (setf (getf (getf (pa-frame-slots classtruct) slotname) facetname) newval))

(defun modify-slot-plist (classtruct slotname newval)
  (setf (getf (pa-frame-slots classtruct) slotname) newval))

;;; Added 6-Sep-86.  Is also used changing classes.
(defun set-instance-slot (frame framename slotname newval)
  (cond ((not frame)
         (ml-format T :no-such-frame NIL))
        (T
         (setq slotname (assure-keyword slotname))
         (let* ((classname (frame-class frame))
                (frame-type (name-to-frame-type classname))
                (slot-index (pa-frame-index-plist frame-type))
                (slot-spec (getf slot-index slotname))
                (snum (car slot-spec)))
           (cond ((null slot-spec)
                  (ml-cerror :ignore-set-slot :no-slot-in-class slotname frame))
                 (T
                  (if (eq (name-to-frame classname) frame)   ;;Means it's a class
                      (modify-slot-plist frame-type slotname newval))
                  (setf (aref frame snum) newval)
                  (handle-set-inverse-slot frame framename newval slotname snum)
                  newval))))))

(defun handle-set-inverse-slot (frame framename newval sname snum)
  (let ((usname (assure-current sname)))
    (if (and newval (invertible-relation usname))
        (let ((invname (get-slot usname :inverse-name)))
          (cond ((consp newval)
                 (dolist (oneval newval)
                   (set-inverse-slot (or framename frame) oneval sname invname)))
                (T
                 (setf (aref frame snum) (list newval))
                 (set-inverse-slot (or framename frame) newval sname invname)))))))

(defun set-inverse-slot (frame val sname invname)
  (if (not (symbolp val)) (setq frame (assure-frame frame)))
  (if (not (framep val))
      (if (or (not (symbolp val))
              (and (not (get val :frame)) (not (get val :classp))))
          (paformat t :no-frame-but-relation
                    val frame sname))
      (multiple-value-bind (invval exist) (get-slot val invname)
        (if (not exist)
            (paformat t :inverse-slot
                      invname (pa-class-of val))
            (if (listp invval)
                (my-pushnew frame (get-slot val invname))
                (if (not (eq invval frame))
                    (set-slot val invname (list frame invval))))))))

;;; Can't propagate down since you don't know the depth.
;;; Can't fire demons because there are no facets to store demons on.
(defun set-slot (frame slotname newval)
  (set-instance-slot (assure-frame frame) frame slotname newval))

(defun set-cslot (frame slotname filler)
  (let* ((real-frame (assure-frame frame))
         (frame-type (if real-frame (frame-type-of real-frame)))
         (slot-plist (if frame-type (pa-frame-c-slots frame-type)))
         (real-slotname (assure-keyword slotname)))
    (when real-frame
      (cond ((not (classp real-frame))
             (ml-error :no-such-class frame))
            ((not-getf slot-plist real-slotname)
             (ml-cerror :ignore-set-cslot :no-cslot-in-class slotname frame))
            (T
             (setf (getf (pa-frame-c-slots frame-type) real-slotname)
                   filler))))))

;;; Utilities for set-facet...

(defun mark-classname (name)
  (putprop name T :marked)
  (push name *MARKED-CLASSES*))

(defun marked-p (name)
  (get name :marked))

(defun init-propagate ()
  (dolist (class *MARKED-CLASSES*)
    (putprop class NIL :marked))
  (setq *MARKED-CLASSES* NIL))

(defun num-instances-of-class (classname)
  (length (get classname :instances)))


;;; -----------------------------------------------------------------------
;;; MODIFY- & REMOVE-FRAME
;;; -----------------------------------------------------------------------

;;; Same syntax as make but don't make a new copy - just set the slots
;;; indicated in the 'newslots' plist.
;;; Doesn't propagate.
(defun modify-frame (frame newslots)
  (let* ((classname (pa-class-of frame))
         (frame-type (name-to-frame-type classname))
         (slot-index (pa-frame-index-plist frame-type)))
    (check-slot-args newslots (pa-frame-snames frame-type) classname)
    (do ((slot newslots (cddr slot)))
        ((null slot) T)
      (let* ((slot-spec (getf slot-index (assure-keyword (car slot))))
             (snum (car slot-spec)))
        (cond ((consp (cdr slot-spec))  ;;That means it's faceted
               (do ((facet (cadr slot) (cddr facet)))
                   ((null facet) T)
                 (let ((facetnum (getf (cdr slot-spec) (assure-keyword (car facet)))))
                   (setf (aref (aref frame snum) facetnum) (cadr facet)))))
              (T
               (setf (aref frame snum) (cadr slot)))))))
  frame)

;;; Added 12-Mar-87
;;; Can't fire demons for slots that are not faceted.
;;; Fires demons for only the first facet specified.
(defun modify-frame-demons (frame newslots)
  (init-propagate)
  (let* ((classname (pa-class-of frame))
         (frame-type (name-to-frame-type classname))
         (slot-index (pa-frame-index-plist frame-type))
         (framename (if (symbolp frame) frame))
         (frame (assure-frame frame)))
    (when frame
      (check-slot-args newslots (pa-frame-snames frame-type) classname)
      (do* ((slots newslots (cddr slots))
            (slot (car slots) (car slots)))
           ((null slots) T)
        (let ((slot-spec (getf slot-index slot)))
          (cond ((consp (cdr slot-spec))        ;;That means it's faceted
                 (let ((facet (cadr slots)))
                   (set-facet-demons0 framename frame slot (car facet)
                                      (cadr facet) 0 :slots-only)
                   (do ((facet (cddr facet) (cddr facet)))
                       ((null facet) T)
                     (set-facet-demons0 framename frame slot (car facet)
                                        (cadr facet) 0 NIL))))
                (T
                 (setf (aref frame (car slot-spec)) (cadr slots)))))))
    frame))

;;; Should also add frame to a frame freelist.
;;; Extended to handle frame classes too 23-June-87.
(defun remove-frame (oframe)
  (let ((frame (assure-frame oframe)))
    (when frame
      (cond ((classp frame)
             (remove-frame-class frame))
            (T (remove-frame-instance frame)))
      (release-frame frame)))
  T)

(defun remove-frame-class (frame)
  (let ((class (frame-class frame)))
    (dolist (parent (isas class))
      (setf (get parent :inverse-isas)
            (delete class (get parent :inverse-isas))))
    (dolist (child (inverse-isas class))
      (setf (get child :isas) (delete class (get child :isas)))
      (setf (get child :immediate-isas)
            (delete class (get child :immediate-isas))))
    (clear-instances class)
    (remprop class :isas)
    (remprop class :inverse-isas)
    (remprop class :classp)
    (remprop class :frame-type)
    (remprop class :frame)))

;;; Added 20-Aug-1987.
(defun clear-instances (class)
  (dolist (iname (instance-names-of class))
    (remprop iname :frame))
  (let ((class-frame (assure-frame class)))
    (dolist (instance (instances-of class))
      (when (and instance (not (eq instance class-frame)))
        (setf (aref instance 0) NIL)
        (release-frame instance))))
  (remprop class :instance-names)
  (remprop class :instances))

(defun remove-frame-instance (frame)
  (let ((class (frame-class frame)))
    (setf (aref frame 0) NIL)
    (setf (get class :instance-names)
          (delete frame (get class :instance-names)
                  :count 1
                  :test #'(lambda (frame fname)
                            (when (eq (get fname :frame) frame)
                              (remprop fname :frame)
                              T))))
    (setf (get class :instances)
          (delete frame (get class :instances) :count 1))))

;;; Added 22-Nov-1987 to make FRulekit's $modify more efficient.
;;; Before it was adding then removing the frame to/from Parmenides data structs.
(defun replace-frame (old new)
  (let* ((frame (assure-frame old))
         (class (and frame (frame-class frame))))
    (when frame
      (cond ((null class)
             (ml-cerror :ignore-replace-frame :no-such-frame-error old))
            ((classp frame)
             (ml-cerror :ignore-replace-frame :replace-doesnt-work))
            ((not (eq new old))
             (nsubst new old (get class :instances)))))))


;;; -----------------------------------------------------------------------
;;;             COPY-FRAME
;;; -----------------------------------------------------------------------
;;; Modified 24-Mar-87: If newslots is provided then for each slot in <newslots>,
;;; copies the data structure inside that slot in <frame>.
(defun copy-frame (frame &optional newslots)
  (setq frame (assure-frame frame))
  (when frame
    (if (adjustable-array-p frame)
        (copy-adjustable-array frame newslots)
        (copy-nested-structure frame newslots))))

(defmacro fast-copy-frame (frame &optional newslots)
  `(if (adjustable-array-p ,frame)
       (copy-adjustable-array ,frame ,newslots)
       (copy-nested-structure ,frame ,newslots)))

(defun copy-adjustable-array (frame newslots)
  (let* ((numslots (pa-frame-numslots (name-to-frame-type
                                       (frame-class frame))))
         (newframe (make-array numslots
                               :adjustable T)))
    (pa-copy-array frame newframe numslots newslots)))

(defun copy-nested-structure (frame newslots)
  (let* ((numslots (pa-frame-numslots (name-to-frame-type
                                       (frame-class frame))))
         (newframe (make-array numslots)))
    (pa-copy-array frame newframe numslots newslots)))

(defun pa-copy-array (a1 a2 numslots newslots)
  (if newslots
      (copy-array-slots a1 a2 newslots)
      (dotimes (i numslots)
        (setf (aref a2 i)
              (aref a1 i))))
  a2)
;;took out copy-generic-thing of (aref a1 i) 16-Mar-87.

(defun copy-array-slots (a1 a2 newslots)
  (let ((index (pa-frame-index-plist (name-to-frame-type (frame-class a1)))))
    (do ((slote index (cddr slote))
         (slotnum 0 (1+ slotnum)))
        ((null slote) a2)
      (cond ((not (consp (cdadr slote)))        ;;not faceted
             (setf (aref a2 slotnum)
                   (aref a1 slotnum)))
            ((getf newslots (current-to-keyword (car slote)))
             (setf (aref a2 slotnum)
                   (copy-generic-thing (aref a1 slotnum))))
            (T
             (setf (aref a2 slotnum)
                   (aref a1 slotnum)))))))


(defun dup-array (a1 a2)
  (pa-copy-array a1 a2 (length a1) NIL))

(defun copy-vector (a)
  (let* ((length (car (array-dimensions a)))
         (newv (make-array length :adjustable (adjustable-array-p a))))
    (pa-copy-array a newv length NIL)))

;;; array, symbol or list
(defun copy-generic-thing (thing)
  (cond ((arrayp thing)
         (copy-vector thing))
        ((consp thing)
         (copy-list thing))
        (T thing)))

;;; ---------------------------------------------------------------
;;;     BASIC TOP-LEVEL FRAME FUNCTIONS
;;; ---------------------------------------------------------------

;;; For internal frame printing.
(defun pa-frame-printer (frame stream depth)
  (declare (ignore depth))
  (format stream "Frame ~S:~%" (pa-frame-name frame))
  (format stream "Slots: ~S~%" (pa-frame-slots frame))
  )


(defun vector-eql (slot1 slot2)
  (do ((index (1- (length slot1)) (1- index)))
      ((zerop index) (eq (aref slot1 index) (aref slot2 index)))
    (if (not (eq (aref slot1 index) (aref slot2 index)))
        (return NIL))))

(defun maybe-quote (value)
  (if (constantp value) value
      (list 'quote value)))

;;; Pretty-prints a frame.  frame may be either a symbol or frame data
;;; structure, instance or class.
(defun pp-frame (frame &key (stream *standard-output*) (slots :*ALL*)
                         (all-slots T))
  (let ((real-frame (assure-frame frame)))
    (if real-frame
        (write-frame real-frame (if (symbolp frame) frame
                                    (maybe-get-frame-name frame))
                     (if (eq slots :*ALL*) (cdr (get-slot-names real-frame))
                         slots) NIL stream
                         :savep NIL
                         :all-slots-p all-slots))))

;;; Writes the frame out in Parmenides-readable form.
(defun save-frame (frame &key (stream *standard-output*) (all-slots NIL)
                           (slots :*ALL*))
  (let ((real-frame (assure-frame frame)))
    (if real-frame
        (write-frame real-frame (if (symbolp frame) frame
                                    (maybe-get-frame-name frame))
                     (if (eq slots :*ALL*) (cdr (get-slot-names real-frame))
                         slots) 'make-frame stream
                         :savep T
                         :all-slots-p all-slots))))

;;; General-purpose frame-writing function.  Used by save-frame and pp-frame
;;; to pretty-print or save frame classes or instances.
(defun write-frame (frame name snames make-fn stream
                    &key savep all-slots-p)
  (let ((index-plist (pa-frame-index-plist (frame-type-of frame)))
        (*PRINT-CIRCLE* T)
        (class (frame (pa-class-of frame)))
        (classp (classp frame))
        (facetp (getf (pa-frame-c-slots (frame-type-of frame)) :facets)))
    (write-header frame name savep make-fn class stream)
    (dolist (sname snames)
      (let* ((index (getf index-plist sname))
             (slot (aref frame (car index))))
        (if (printable-slot-p frame slot class index all-slots-p classp)
            (write-slot-value
             frame sname slot index all-slots-p savep classp stream facetp))))
    (if savep
        (format stream "\)~%"))))

(defun write-slot-value (frame sname slot index all-slots-p savep classp
                         stream facetp)
  (format stream " ~12S " sname)
  (cond ((or (eq facetp :NO) (atom (cdr index)))  ;;then the slot is facet-less
         (if savep
             (if (and classp (not (eq facetp :NO))  ;; Write it as faceted
                      (consp slot))                 ;; even though it's not.
                 (format stream "'\(:VALUE ~S\)~%" slot)
                 (format stream "~S~%" (if classp slot (maybe-quote slot))))
             (format stream "~S~%" slot)))
        (T
         (if (and (not classp) savep) (format stream "'"))
         (format stream "\(")
         (do ((facets (cdr index) (cddr facets)))
             ((null facets) T)
           (when (or all-slots-p (not (eq (car facets) :depth)))
             (format stream "~S ~S" (car facets)
                     (if (and classp savep)
                         (maybe-quote (aref slot (cadr facets)))
                         (aref slot (cadr facets))))
             (if (cddr facets) (format stream " "))))
         (format stream "\)~%"))))


(defun printable-slot-p (frame slot class index all-slots-p classp)
  (or all-slots-p
      (if classp
          (or (atom (cdr index))
              (let ((depth-facet (getf (cdr index) :depth)))
                (or (not depth-facet)
                    (zerop (aref slot depth-facet)))))
          (and (not (eq slot (aref class (car index))))  ;;local value
               (or (atom (cdr index))
                   (let ((depth-facet (getf (cdr index) :depth)))
                     (if (not depth-facet)
                         (not (vector-eql slot (aref class (car index))))
                         (zerop (aref slot depth-facet)))))))))

(defun write-header (frame name savep make-fn class-frame stream)
  (let ((classname (frame-class class-frame)))
    (cond ((and savep
                (eq frame class-frame))         ;; then it's a class
           (format stream "\(DEF-FRAME ~S " classname)
           (format stream " ~S" (pa-frame-c-slots (frame-type-of frame))))
          (savep                                ;; else it's an instance
           (format stream "\(~S ~S ~S" make-fn
                   (list 'quote classname) (if name (list 'quote name))))
          ((classp frame)
           (format stream "Frame Class ~S:" classname)
           (if (pa-frame-c-slots (frame-type-of frame))
               (format stream " ~S" (pa-frame-c-slots (frame-type-of frame)))))
          (name (format stream "Frame ~S of class ~S~%" name classname))
          (T (format stream "Frame of class ~S" classname))))
  (terpri stream))

(defun get-atomic-value (sname frame)
  (multiple-value-bind (slot found) (get-instance-slot frame sname)
    (cond (found
           (values
            (if (arrayp slot)
                (aref slot 0)
                slot)
            T))
          (T (values NIL NIL)))))


;;; Added 28-Mar-1988.
;;; slots and facets are either single names or a list of names.
(defmacro define-facet-getter (class slots facets)
  (define-facet-accessor0 class slots facets :get))

(defmacro define-facet-setter (class slots facets)
  (define-facet-accessor0 class slots facets :set))

(defmacro define-facet-accessors (class slots facets)
  (define-facet-accessor0 class slots facets :both))

(defun define-facet-accessor0 (class slots facets fn-type)
  (let ((class-prefix (smash class "-"))
        (index-plist (pa-frame-index-plist (name-to-frame-type class)))
        (*accessor-fns* NIL))
    (declare (special *accessor-fns*))
    (if (atom slots)
        (define-facet-getter-for-slot
            class-prefix index-plist (assure-keyword slots) facets fn-type)
        (dolist (slot slots)
          (define-facet-getter-for-slot
              class-prefix index-plist (assure-keyword slot) facets fn-type)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@*accessor-fns*)))

(defun define-facet-getter-for-slot (class-prefix index-plist slot facets fn-type)
  (let ((slot-prefix (smash class-prefix slot))
        (slot-info (getf index-plist slot)))
    (cond ((not (numberp (car slot-info)))
           (paformat t :no-slot-in-class slot class-prefix))
          ((atom (cdr slot-info))
           (paformat T :slot-no-facets slot class-prefix))
          (T
           (if (atom facets)
               (define-getter-for-facet
                   slot-prefix slot-info (assure-keyword facets) fn-type)
               (dolist (facet facets)
                 (define-getter-for-facet
                     slot-prefix slot-info (assure-keyword facet) fn-type)))))))

(defun define-getter-for-facet (slot-prefix slot-info facetname fn-type)
  (declare (special *accessor-fns*))
  (let ((fnum (getf (cdr slot-info) facetname)))
    (cond ((not (numberp fnum))
           (paformat t :facet-not-in-slot facetname slot-prefix))
          (T
           (if (or (eq fn-type :get) (eq fn-type :both))
               (push `(defun ,(smash slot-prefix "." facetname) (frame)
                        (aref (aref frame ,(car slot-info)) ,fnum))
                     *accessor-fns*))
           (if (or (eq fn-type :set) (eq fn-type :both))
               (push
                `(defun ,(smash "SET-" slot-prefix "." facetname) (frame value)
                   (setf (aref (aref frame ,(car slot-info)) ,fnum) value))
                *accessor-fns*))))))


;;; Parmenides-eval (sort of).  Instead of evaling things, it puts them into
;;; a list so that def-frame can return this list of things to be evaled.
(defun pa-eval (exp)
  (push exp *THINGS-TO-EVAL*))

;;; If the cslots plist contains an :IS-A property, ASSURE-ISA-LIST
;;; makes sure that the property value is a list.  It does this by
;;; modifying structure.
(defun assure-isa-list (cslots)
  (do ((cslot cslots (cddr cslot)))
      ((null cslot) cslots)
    (when (and (eq (car cslot) :is-a)
               (not (listp (cadr cslot))))
      (setf (cadr cslot) (list (cadr cslot)))
      (return cslots))))

(defun maybe-update-range-classes (name full-iplist full-cplist facetp)
  (do ((plist full-iplist (cddr plist)))
      ((null plist))
    (let* ((usname (assure-current (car plist)))
           (contents (cadr plist))
           (facets (and (not (eq facetp :NO)) (consp contents)))
           (value (if facets (eval (cadr contents)) contents)))
      (when (and value (invertible-relation usname))
        (let ((invsname (get-slot usname :inverse-name)))
          (if (consp value)     ;;multiple frames in range
              (dolist (range value)
                (update-range-class-value invsname range name facets))
              (update-range-class-value invsname value name facets))))))
  (do ((plist full-cplist (cddr plist)))
      ((null plist))
    (let* ((usname (assure-current (car plist)))
           (value (cadr plist)))
      (when (and value (invertible-relation usname))
        (let ((invsname (get-slot usname :inverse-name)))
          (if (consp value)     ;;multiple frames in range
              (dolist (range value)
                (update-range-class-cplist-value invsname range name))
              (update-range-class-cplist-value invsname value name)))))))

;;; Top-level, main class-definition function.  Returns the instance of the
;;; frame, which is created and filled in with the default values.
(defmacro def-frame (name cslots &rest slots)
  (setq *THINGS-TO-EVAL* NIL)
  ;; Operators like KEYWORDIZE-CPLIST, and ASSURE-ISA-LIST are
  ;; destructive, so make sure that cslots structure can be modified.
  (setf cslots (copy-tree cslots))
  (keywordize-cplist cslots)
  (assure-isa-list cslots)
  (let* ((parents (check-parents (getf cslots :is-a)))
         (relation-p (some #'(lambda (parent) (isa-p parent 'relation))
                           parents))
         (frame-cslots (find-frame-cslots parents))
         (parent-cplist (combine-slots frame-cslots))
         (local-cached  (getf cslots :cache *DEFAULT*)) ;cslots is mangled
         (local-setable (getf cslots :setable *DEFAULT*)) ;after next line
         (local-getters (getf cslots :getters *DEFAULT*))
         ;; FIXME: What's the point in having a destructive
         ;; PLIST-UNION-NO-PROPAGATE, which is only used here, if
         ;; we're just going to copy the result?
         (full-cplist (copy-list
                       (plist-union-no-propagate cslots parent-cplist)))
         (propagatep (getf full-cplist :propagate T))
         (facetp (getf full-cplist :facets T))
         (other-related-frames (find-relation-frames full-cplist))
         (slots (add-class slots name))
         (frame-slots (if local-cached (find-frame-slots parents)))
         (full-iplist (maybe-add-inverse-name
                       name relation-p (plist-union slots frame-slots
                                                    propagatep other-related-frames
                                                    facetp)))
         (iall-slot-namesk (property-names full-iplist))
         (index-plist (make-index-plist1 full-iplist facetp))
         (cached (combine-cache-slots
                  local-cached iall-slot-namesk frame-cslots parents))
         (setable (combine-setable-slots
                   local-setable iall-slot-namesk frame-cslots parents))
         (getters (combine-getter-slots
                   local-getters iall-slot-namesk frame-cslots parents))
         (theframe
           (make-pa-frame
            :propagatep propagatep
            :numslots (length iall-slot-namesk)
            :index-plist index-plist
            :c-slots full-cplist
            :slots full-iplist
            :name name
            :snames iall-slot-namesk
            :rel-slots (find-rel-slots full-iplist))))
    (setf (getf (pa-frame-c-slots theframe) :cache)
          cached)
    (if (or (atom setable)
            (not (and (null (car setable)) (null (cadr setable)))))
        (setf (getf (pa-frame-c-slots theframe) :setable)
              setable))
    (if (and getters (not (eq getters *DEFAULT*)))
        (setf (getf (pa-frame-c-slots theframe) :getters) getters))
    (putprop name theframe :frame-type)
    (write-accessor-functions
     theframe name full-iplist setable propagatep getters facetp)
    (write-maker-function name)
    (if (not (frame-class-p name))
        (putprop name :COMPILE-TIME :classp)) ; Is a class at compile time too!
    (maybe-define-inverse-relation-class name relation-p full-iplist
                                         full-cplist)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (announce-define ',name)
       (setf (get ',name :instances) NIL)
       (putprop ',name (make-pa-frame
                        :propagatep ,propagatep
                        :numslots ,(pa-frame-numslots theframe)
                        :index-plist ',index-plist
                        :c-slots ',full-cplist
                        :slots ',full-iplist
                        :name ',name
                        :snames ',iall-slot-namesk
                        :rel-slots ',(pa-frame-rel-slots theframe))
                :frame-type)
       (putprop ',name (make-default-frame ',name) :frame)
       (maybe-update-range-classes ',name ',full-iplist ',full-cplist ,facetp)
       (putprop ',name T :classp)
       (putprop ',name ',parents :immediate-isas)
       (compute-isas ',name ',parents)
       (compute-inverse-isas ',name ',parents)
       ,.*THINGS-TO-EVAL* ',name)))

(defmacro check-slot (slotlist)
  `(when (not (symbolp (car ,slotlist)))
     (ml-cerror :ignore-list :expecting-slot (car ,slotlist))
     (setq ,slotlist (cdr ,slotlist))))

(defun keywordize-plist (slots)
  (do ((slots slots (cddr slots)))
      ((null slots) T)
    (check-slot slots)
    (setf (car slots) (assure-keyword (car slots)))))

;;; Ensures that relation names are not keywords, and that all other class
;;; slots are keywords.
(defun keywordize-cplist (slots)
  (do ((slot slots (cddr slot)))
      ((null slot) slots)
    (if (isa-p (assure-current (car slot)) 'relation)
        (setf (car slot) (intern-current (car slot)))
        (setf (car slot) (assure-keyword (car slot))))))

;;; Added 28-Apr-87
;;; For every slot in full-iplist which is an invertible relation, add the
;;; inverse of that relation slot to the range (value of the slot) class,
;;; with value name.
;;; For example, if we had done a (def-frame leg () part-of table) and
;;; part-of was an invertible relation with inverse 'contains', then this
;;; routine would add (contains (leg)) to the table's definition if it didn't
;;; already have such a slot.
;;; If the slot already existed then it still has to add the domain frame to
;;; the range's inverse slot value.
;; (defun maybe-update-range-classes (name full-iplist full-cplist facetp)
;;   (do ((plist full-iplist (cddr plist)))
;;       ((null plist))
;;     (let* ((usname (assure-current (car plist)))
;;            (contents (cadr plist))
;;            (facets (and (not (eq facetp :NO)) (consp contents)))
;;            (value (if facets (eval (cadr contents)) contents)))
;;       (when (and value (invertible-relation usname))
;;         (let ((invsname (get-slot usname :inverse-name)))
;;           (if (consp value)  ;;multiple frames in range
;;               (dolist (range value)
;;                 (update-range-class-value invsname range name facets))
;;               (update-range-class-value invsname value name facets))))))
;;   (do ((plist full-cplist (cddr plist)))
;;       ((null plist))
;;     (let* ((usname (assure-current (car plist)))
;;            (value (cadr plist)))
;;       (when (and value (invertible-relation usname))
;;         (let ((invsname (get-slot usname :inverse-name)))
;;           (if (consp value)  ;;multiple frames in range
;;               (dolist (range value)
;;                 (update-range-class-cplist-value invsname range name))
;;               (update-range-class-cplist-value invsname value name)))))))


;;; Should do this at load time too!
(defun update-range-class-value (invsname range name facets)
  (if (not (symbolp range))
      (setq name (assure-frame name)))
  (if range
      (cond ((not (memq (assure-keyword invsname) (get-slot-names range)))
             (add-slot range invsname (if facets (list 'value
                                                       (list 'quote (list name)))
                                          NIL))
             (if (not facets) (maybe-add-to-slot range invsname name)))
            (facets
             (add-to-value range invsname name))
            (T
             (maybe-add-to-slot range invsname name)))))

(defun update-range-class-cplist-value (invsname range name)
  (if (not (get-cslot range (assure-keyword invsname)))
      (add-cslot range invsname (list name))
      (add-to-cslot range (assure-keyword invsname) name)))

;;; If relation-p, and has-inverses then define the inverse frame as
;;; a relation (if it doesn't already exist).
(defun maybe-define-inverse-relation-class (name relation-p iplist cslots)
  (if (and relation-p
           (getf iplist :has-inverses))
      (let ((inverse-classname (getf iplist :inverse-name)))
        (when (not (classp inverse-classname))
          (pa-eval
           `(def-frame ,inverse-classname ,cslots
              :has-inverses T
              :inverse-name ,name))))))

(defun maybe-add-inverse-name (name relation-p plist)
  (if (and relation-p (getf plist :has-inverses)
           (not (getf plist :inverse-name)))
      (nconc plist (list :inverse-name (smash "INVERSE-" name))))
  plist)


;;; Assure that the given value is a list by coercing value to a list if it's not a list.
(defun assure-list (value)
  (if (listp value) value
      (list value)))

(defun announce-define (classname)
  (cond ((frame-class-p classname)
         (paformat t :redefining-class classname))
        (T (paformat t :defining-class classname))))

;;; Filter out parent names which aren't classes.
(defun check-parents (fnames)
  (remove-if #'(lambda (fname)
                 (when (not (classp fname))
                   (ml-format t :parent-not-class fname)
                   T))
             fnames))

(defun write-maker-function (name)
  (pa-eval
   `(defun ,(smash "MAKE-" name) (iname &rest plist)
      (make-frame0 ',name iname plist))))

;;; Non-macro version of def-frame.
(defun def-frame* (name cplist slots)
  (eval `(def-frame ,name ,cplist ,@slots)))

(defun eval-plist (plist)
  (let ((pl (copy-list plist)))
    (do ((thunk pl (cddr thunk)))
        ((null thunk) pl)
      (if (cadr thunk)
          (setf (cadr thunk) (eval (cadr thunk)))))))

;;; Added 1-Apr-87
(defun invertible-relation (framename)
  (and (isa-p framename 'relation)
       (get-slot framename :has-inverses)))

;;; Added 20-July-86.  Returns a list of the slots in plist which are
;;; appending relations.
(defun find-rel-slots (plist)
  (do ((relations plist (cddr relations))
       (res NIL
            (if (frame-class-p (keyword-to-current (car relations)))
                (cons (car relations) res)
                res)))
      ((null relations) res)))

;;; Added 14-July-86.  From a plist, returns a plist of (relation framename),
;;; the relations naming frames which are relations.
(defun find-relation-frames (plist)
  (do ((plist plist (cddr plist))
       (res NIL (if (frame-class-p (car plist))
                    (cons (assure-list (cadr plist)) (cons (car plist) res))
                    res)))
      ((null plist) (nreverse res))))

(defun compute-isas (name parents)
  (putprop name (all-grandparents parents) :isas))

;;; Return a list of all the parents of all the given parents, and the parents
;;; themselves, eliminating redundancies.
(defun all-grandparents (parents)
  (do ((parents parents (cdr parents))
       (res (copy-list parents)
            (ordered-union res (get (car parents) :isas))))
      ((null parents) res)))

(defun compute-inverse-isas (name parents)
  (dolist (parent parents)
    (if (not (memq name (get parent :inverse-isas)))
        (push name (get parent :inverse-isas)))))

;;; Adds the given name as a default of the %class to the slots list of a def-frame
(defun add-class (slots name)
  (append `(:%class ,name) slots))

(defun name-to-frame (frame-name)
  (get frame-name :frame))

;;; Same thing, just has a better name and is documented.
(defun frame (frame-name)
  (get frame-name :frame))

(defun isa-p (name1 name2)
  (and (frame-class-p name1)
       (or (eq name1 name2)
           (memq name2 (isas name1)))))


;;; Returns T iff the given name names a frame class (at compile or run time).
(defun classp (generic-frame)
  (if (symbolp generic-frame)
      (get generic-frame :classp)
      (eq (name-to-frame (frame-class generic-frame)) generic-frame)))

;;; Returns T iff the given name names a frame class (at run time, so internal)
(defun frame-class-p (generic-frame)
  (if (symbolp generic-frame)
      (eq (get generic-frame :classp) T)
      (eq (name-to-frame (frame-class generic-frame)) generic-frame)))

(defun name-to-frame-type (class-name)
  (or (get class-name :frame-type)
      (get (frame-class (assure-frame class-name)) :frame-type)))

(defun get-slot-names (frame)
  (pa-frame-snames (frame-type-of frame)))

(defun slotp (class slotname)
  (not (null (memq (assure-keyword slotname) (get-slot-names class)))))

(defmacro do-facets ((facet-name facet-val slot frame) &body forms)
  (let ((do-var (gentemp "DO-VAR"))
        (real-frame-var (gentemp "REAL-FRAME"))
        (frame-type-var (gentemp "FRAME-TYPE"))
        (slot-var (gentemp "SLOT"))
        (index-plist-var (gentemp "INDEX-PLIST")))
    `(let* ((,real-frame-var (assure-frame ,frame))
            (,frame-type-var (if ,real-frame-var (frame-type-of ,real-frame-var))))
       (when ,real-frame-var
         (let* ((,index-plist-var
                  (getf (pa-frame-index-plist ,frame-type-var)
                        (assure-keyword ,slot)))
                (,slot-var (if ,index-plist-var
                               (aref ,real-frame-var (car ,index-plist-var)))))
           (if (not (consp (cdr ,index-plist-var)))
               (ml-format T :slot-no-facets ,slot ,frame)
               (do* ((,do-var (cdr ,index-plist-var) (cddr ,do-var))
                     (,facet-name (car ,do-var) (car ,do-var))
                     (,facet-val (aref ,slot-var (cadr ,do-var))
                                 (if ,do-var (aref ,slot-var (cadr ,do-var)))))
                    ((null ,do-var) T)
                 ,@forms)))))))

;;; The name of the class is always kept in the 0th position by Parmenides.
(defun frame-class (frame)
  (aref frame 0))

;;; Given the class name return an ordered list classes that it is-a.
(defun isas (frame)
  (let ((class (pa-class-of frame)))
    (and class (get class :isas))))

(defun immediate-isas (frame)
  (let ((class (pa-class-of frame)))
    (and class (get class :immediate-isas))))

;;; Only works for classes.
(defun inverse-isas (frame)
  (let ((class (and (classp frame) (pa-class-of frame))))
    (and class (get class :inverse-isas))))

(defun set-inverse-isas (class invisas)
  (setf (get class :inverse-isas) invisas))

;;; Returns T iff the given object is a frame (instance or class).
;;; (or (and (symbolp frame) (get frame :classp)))
(defun framep (frame)
  (let ((frame (maybe-assure-frame frame)))
    (if (and frame (arrayp frame))
        (let ((class (frame-class frame)))
          (and (symbolp class)
               (frame-class-p class))))))

;;; Return T iff the instance frame is an instance of classname, though not
;;; necessarily an immediate instance.
(defun isa-instance (frame classname)
  (setq frame (maybe-assure-frame frame))
  (and (framep frame)
       (frame-class-p classname)
       (or (eq classname (frame-class frame))
           (memq classname (isas (pa-class-of frame))))))

(defun isa-or-instance (frame class)
  (if (frame-class-p frame)
      (isa-p (pa-class-of frame) class)
      (isa-instance frame class)))

(defun frame-instance-p (frame)
  (setq frame (maybe-assure-frame frame))
  (and frame
       (not (frame-class-p frame))
       (frame-class frame)
       (memq frame (instances-of (frame-class frame)))
       T))

(defun instance-names-of (framename)
  (when (assure-frame framename)
    (get framename :instance-names)))

(defun instances-of (framename)
  (when (assure-frame framename)
    (get framename :instances)))

(defun set-instances-of (framename value)
  (when (assure-frame framename)
    (setf (get framename :instances) value)))

;;; Removes the class instance from the given class.
(defun delete-class-instance (class)
  (when (assure-frame class)
    (setf (get class :instances)
          (delete (frame class) (get class :instances)))))

(defun call-form-on-subinstances (class lambda-form)
  (dolist (frame (instances-of class))
    (funcall lambda-form frame))
  (dolist (subclass (inverse-isas class))
    (call-form-on-subinstances subclass lambda-form)))

(defmacro with-all-subinstances-of (class lambda-form)
  `(cond ((not (classp ,class))
          (ml-cerror :ignore-frame :no-such-class ,class))
         (T
          (call-form-on-subinstances ,class ,lambda-form))))

(defun call-form-on-subclasses (class lambda-form)
  (funcall lambda-form class)
  (dolist (subclass (inverse-isas class))
    (call-form-on-subclasses subclass lambda-form)))

(defmacro with-all-subclasses-of (class lambda-form)
  `(cond ((not (classp ,class))
          (ml-cerror :ignore-frame :no-such-class ,class))
         (T
          (call-form-on-subclasses ,class ,lambda-form))))

;;; Used internally.  Returns NIL if framename is not a class, instead
;;; of generating a cerror like instances-of does.
(defun maybe-instances-of (framename)
  (when (maybe-assure-frame framename)
    (get framename :instances)))

;;; Same as get-frame-name, except that if the frame doesn't have a name
;;; then this function returns NIL instead of generating a new name.
(defun maybe-get-frame-name (frame)
  (cond ((symbolp frame) frame)
        (T
         (let* ((class (frame-class frame))
                (name (member-if #'(lambda (name) (eq (frame name) frame))
                                 (instance-names-of class))))
           (if name (car name))))))

;;; Added 18-Aug-87.  Returns the name of the given frame instance.
;;; If the frame doesn't have a name, then it gives it one and returns that.
(defun get-frame-name (frame)
  (cond ((symbolp frame) frame)
        (T
         (let* ((class (frame-class frame))
                (name (member-if #'(lambda (name) (eq (frame name) frame))
                                 (instance-names-of class))))
           (if name (car name)
               (name-frame frame))))))

;;; Also added 18-Aug-87.  Gives frame a unique name and returns that name.
(defun name-frame (frame &optional name)
  (let* ((class (frame-class frame))
         (name (or name (unique-instance-name class))))
    (putprop name frame :frame)
    (my-pushnew name (get class :instance-names))
    name))

(defun unique-instance-name (class)
  (smash class "-" (length (instances-of class))))


;;;; ---------------------------------------------------------------------
;;;; ADD-SLOT (for classes), implemented 28-Apr-1987
;;;; ---------------------------------------------------------------------

;;; Syntax: (add-slot <frame> <slotname> <slot-contents> &key <cache>). Adds the
;;; slot and its description to the given frame class.

;;; Notes: internal data structures to be updated: pa-snames, pa-index-plist,
;;; pa-slots. Update rel-slots if it's a relation slot.  Increment pa-numslots.
;;; Also, update all instances of the frame.

(defun add-slot (frame slotname contents &key cache)
  (if (not (classp frame))
      (ml-cerror :ignore-add-slot :only-slots-to-classes)
      (let* ((frame (assure-frame frame))
             (classname (frame-class frame))
             (pftype (frame-type-of classname))
             (slotname (assure-keyword slotname))
             (propagatep (pa-frame-propagatep pftype))
             (facetp (getf (pa-frame-c-slots pftype) :facets))
             (contents (maybe-propagate-init-facet-plist
                        propagatep (maybe-copy-list contents) facetp))
             (slotnum (incf (pa-frame-numslots pftype)))
             (facet-index-plist (make-new-plist (1- slotnum) contents
                                                facetp))
             (slot-data-structure (make-new-default-slot contents)))
        (if (consp contents) (keywordize-plist contents))
        (cond
          ((member slotname (pa-frame-snames pftype))
           (ml-format T :slot-exists slotname))
          (T
           (my-maybe-nconc slotname (pa-frame-snames pftype))
           (setf (pa-frame-index-plist pftype)
                 (nconc (pa-frame-index-plist pftype)
                        (list slotname facet-index-plist)))
           (setf (pa-frame-slots pftype)
                 (nconc (pa-frame-slots pftype) (list slotname contents)))
           (if (isa-p (assure-current slotname) 'relation)
               (setf (pa-frame-rel-slots pftype)
                     (nconc (pa-frame-rel-slots pftype) (list slotname))))
           (let (new-ins name (new-instances NIL))
             (dolist (instance (instances-of classname))
               (setf name (maybe-get-frame-name instance))
               (setf new-ins (adjust-array instance slotnum
                                           :initial-element NIL))
               (when name (putprop name new-ins :frame))
               (push new-ins new-instances)
               (let ((newval (copy-generic-thing slot-data-structure)))
                 (if (and propagatep (arrayp newval) (not (eq instance frame)))
                     (incf (aref newval 1)))            ;;depth facet
                 (setf (aref new-ins (1- slotnum)) newval)))
             (set-instances-of classname (nreverse new-instances)))
           (when cache  ;;propagate to sub-classes and add to cache class slot
             (let ((c-slots (pa-frame-c-slots pftype))
                   (subclasses (inverse-isas classname)))
               (my-maybe-nconc slotname (getf c-slots :cache))
               (setf (pa-frame-c-slots pftype) c-slots)
               (when (and (consp contents) (getf contents :depth) subclasses)
                 (setq contents (copy-list contents))
                 (incf (getf contents :depth)))
               (dolist (subclass subclasses)
                 (add-slot subclass slotname contents :cache cache))))
           slotname)))))

(defun make-new-default-slot (contents)
  (if (consp contents)
      (make-default-frame-slot (eval-plist contents))
      contents))

(defun add-cslot (frame slotname contents)
  (if (not (classp frame))
      (ml-cerror :ignore-add-cslot :only-cslots-to-classes)
      (let* ((pftype (frame-type-of frame))
             (slotname (correct-package slotname)))
        (if (getf (pa-frame-c-slots pftype) slotname)
            (ml-format T :cslot-exists slotname)
            (setf (pa-frame-c-slots pftype)
                  (nconc (pa-frame-c-slots pftype) (list slotname contents))))
        slotname)))

;;;; -----------------------------------------------------------------------
;;;; THE INSTANCE-MAKING FUNCTIONS.  (revised 9-Sep-86)
;;;; -----------------------------------------------------------------------

;;; General slot making function.  A little slower than defining a make
;;; function for each frame/slot pair, but saves lots of space,
;;; and makes compiling frame files much faster.  Added 7-Sep-86.
;;; Revised again 13-Feb-87 for speed.
(defun make-frame-slot (class sname fillers)
  (keywordize-plist fillers)
  (let* ((newframe (copy-seq (get-slot class sname))))
    (if fillers
        (do ((slots (getf (pa-frame-slots (name-to-frame-type class)) sname)
                    (cddr slots))
             (fnum 0 (1+ fnum)))
            ((null slots))
          (let ((filler (getf fillers (car slots) *DEFAULT*)))
            (if (not (eq filler *DEFAULT*))
                (setf (aref newframe fnum) filler)))))
    newframe))

(defun maybe-make-frame-slot (facetp class sname fillers)
  (if facetp
      (make-frame-slot class sname fillers)
      fillers))

;;; Makes a default faceted slot (array) given fillers (plist of facets).
;;; Split off from make-frame-slot 14-Feb-87
(defun make-default-frame-slot (fillers)
  (let* ((newslot (get-free-frame (/ (length fillers) 2) :adjustable T)))
    (do ((facet fillers (cddr facet))
         (fnum 0 (1+ fnum)))
        ((null facet) newslot)
      (setf (aref newslot fnum) (cadr facet)))))

;;; General frame-making function.
(defun make-frame (class name &rest plist)
  (make-frame0 class name plist))

;;; Helper function for make-frame0.
(defun do-slot (sname facetp)
  (declare (special pclass fi-alist af-alist plist default-frame pftype
                    mf-newframe name))
  (let* ((snum (get-slot-num fi-alist sname))
         (sval (getf plist sname *DEFAULT*))
         (faceted (and (not (eq facetp :NO)) (faceted-p2 fi-alist sname))))
    (if (not (eq sname :%class))
        (setq sval (generic-inherit sname sval af-alist pclass
                                    (pa-frame-propagatep pftype) faceted)))
    (when (eq sval *DEFAULT*)
      (setq sval (maybe-copy-vector faceted (aref default-frame snum)))
      (if (and faceted (pa-frame-propagatep pftype))
          (incf (aref sval 1))))
    (setf (aref mf-newframe snum) sval)
    (if faceted (aref sval 0) sval)))

(defun maybe-copy-vector (boolean value)
  (if (and boolean (not *READ-ONLY*)) (copy-vector value) value))

;;((not faceted)
;; (if (eq sval *DEFAULT*)                           ;;if not provided then
;;     (setq sval (aref default-frame snum))))       ;;use default

;;; Added 27-Apr-87
;;; Calls handle-set-inverse-facet or handle-set-inverse-slot, depending
;;; on whether the mother is faceted.  Assumes the value facet is being set
;;; for faceted slots.
;;; Also, for faceted slots, val is the whole slot data structure.
;;; Name is optional (i.e., could be NIL).
(defun handle-set-inverse-val (frame name sval sname snum faceted)
  (if faceted
      (handle-set-inverse-facet frame name (aref sval 0) sname 'value snum 0)
      (handle-set-inverse-slot frame name sval sname snum)))

;;;  Plist is a list in this case.
(defun make-frame0 (pclass name plist)
  (declare (special plist pclass name))
  (if (not (classp pclass))
      (ml-error :no-such-class pclass))
  (cond
    ((frame-class-p name)
     (ml-format T :different-names name)
     (terpri)
     (ml-format T :dont-make-instance))
    (T
     (let* ((pftype (name-to-frame-type pclass))
            (default-frame (name-to-frame pclass))
            (mf-newframe (get-free-frame (pa-frame-numslots pftype)
                                         :adjustable T))
            (fi-alist (pa-frame-index-plist pftype))
            (relations (pa-frame-rel-slots pftype))
            (facetp (getf (pa-frame-c-slots pftype) :facets))
            af-alist af-alist2)
       (declare (special fi-alist af-alist default-frame mf-newframe pftype))
       (check-slot-args plist (pa-frame-snames pftype) pclass)
       (dolist (rel-slot relations)
         (if (getf plist rel-slot)
             (push (create-sf-entry rel-slot (do-slot rel-slot facetp))
                   af-alist)
             (push (create-sf-entry rel-slot (do-slot rel-slot facetp))
                   af-alist2)))
       (setf af-alist (nconc af-alist af-alist2))
       (when name
         (putprop name mf-newframe :frame)
         (my-pushnew name (get pclass :instance-names)))
       (dolist (sname (pa-frame-snames pftype))
         (if (not (memq sname relations))
             (do-slot sname facetp)
             (let ((snum (get-slot-num fi-alist sname)))
               (handle-set-inverse-val
                mf-newframe name (aref mf-newframe snum) sname snum
                (and (not (eq facetp :NO)) (faceted-p2 fi-alist sname))))))
       (push mf-newframe (get pclass :instances))
       mf-newframe))))

;;; This version is faster because it doesn't take a keyword plist
;;; argument like make-frame0, but instead takes an ordered list of
;;; all the slot values.
;;; Also, it doesn't check to see that the given slots are correct.
(defun fast-make-frame0 (pclass name plist)
  (declare (special plist pclass name))
  (if (not (classp pclass))
      (ml-error :no-such-class pclass))
  (when (frame-class-p name)
    (ml-format T :different-names name)
    (terpri)
    (ml-format T :dont-make-instance))
  (let* ((pftype (name-to-frame-type pclass))
         (default-frame (name-to-frame pclass))
         (mf-newframe (get-free-frame (pa-frame-numslots pftype)
                                      :adjustable T))
         (fi-alist (pa-frame-index-plist pftype))
         (relations (pa-frame-rel-slots pftype))
         (facetp (getf (pa-frame-c-slots pftype) :facets))
         af-alist af-alist2)
    (declare (special fi-alist af-alist default-frame mf-newframe pftype))
    (dolist (rel-slot relations)
      (if (getf plist rel-slot)
          (push (create-sf-entry rel-slot (do-slot rel-slot facetp)) af-alist)
          (push (create-sf-entry rel-slot (do-slot rel-slot facetp))
                af-alist2)))
    (setf af-alist (nconc af-alist af-alist2))
    (when name
      (putprop name mf-newframe :frame)
      (my-pushnew name (get pclass :instance-names)))
    (dolist (sname (pa-frame-snames pftype))
      (if (not (memq sname relations))
          (do-slot sname facetp)
          (let ((snum (get-slot-num fi-alist sname)))
            (handle-set-inverse-val
             mf-newframe name (aref mf-newframe snum) sname snum
             (faceted-p2 fi-alist sname)))))
    (push mf-newframe (get pclass :instances))
    mf-newframe))

(defun check-slot-args (plist frame-snames class)
  (do ((plist plist (cddr plist)))
      ((null plist) T)
    (if (not (memq (car plist) frame-snames))
        (ml-cerror :continue :slot-make-modify (car plist) class))))


;;; Extended to work with unfaceted slots 20-May-87.
(defun generic-inherit (usname sval fi-alist class propagatep facetp)
  (let*
      ((appendp :no)
       (related-frames
         (if fi-alist
             (some
              #'(lambda (fi-entry)
                  (cond
                    ((eq (car fi-entry) :*ALL*)
                     (setq appendp (assure-keyword (cddr fi-entry)))
                     (cadr fi-entry))
                    (T
                     (dolist (sname (car fi-entry) NIL)
                       (cond ((consp sname)
                              (when
                                  (eq (assure-keyword (car sname)) usname)
                                (if (memq (assure-keyword (cadr sname))
                                          '(:APPEND :NCONC))
                                    (setq appendp T))
                                (return (cadr fi-entry))))       ;;from dolist
                             ((eq (assure-keyword sname) usname)
                              (setq appendp
                                    (assure-keyword (cddr fi-entry)))
                              (return (cadr fi-entry))))))))     ;;from dolist
              fi-alist))))
    (cond ((and related-frames (or (consp related-frames)
                                   (arrayp related-frames)
                                   (frame related-frames)))
           (cond ((eq appendp T)
                  (if (eq sval *DEFAULT*) (setq sval NIL))
                  (setq sval (maybe-make-frame-slot
                              facetp class usname
                              (ia-facetplist-append
                               sval (get-slot-ref usname related-frames
                                                  facetp
                                                  ;;                                                      (arrayp related-frames)
                                                  NIL))))
                  (if (and facetp propagatep) (setf (aref sval 1) 0)))
                 ((and (not (eq sval *DEFAULT*))
                       appendp)
                  (setq sval (maybe-make-frame-slot facetp class usname sval)))
                 (T
                  (if (eq sval *DEFAULT*)
                      (setq sval
                            (or
                             (if related-frames
                                 (maybe-make-slot-from-val
                                  facetp class usname propagatep
                                  (find-propagatep related-frames)
                                  (get-slot-ref usname related-frames
                                                facetp
                                                ;;                      (arrayp related-frames)
                                                :FIRST)))
                             sval))))))
          ((not (eq sval *DEFAULT*))
           (setq sval (maybe-make-frame-slot facetp class usname sval))
           (if (and facetp propagatep) (setf (aref sval 1) 0)))))
  sval)

(defun find-propagatep (rel-frames)
  (cond ((or (arrayp rel-frames) (frame rel-frames))
         (pa-frame-propagatep
          (name-to-frame-type
           (pa-class-of (if (consp rel-frames) (car rel-frames) rel-frames)))))
        (T NIL)))

;;; Re-written 22-Jan-87
(defun create-sf-entry (relname sval)
  (let ((urelname (intern (symbol-name relname))))
    (list* (get-inherited-slot-names urelname)
           sval
           (get-appendp urelname))))

(defun ia-facetplist-append (pl1 l1)
  (cond (pl1
         (list* (first pl1)
                (append (cadr pl1) l1)
                (cddr pl1)))
        (T (list :value l1))))

;;; If one propagates (prop1 or prop2) and the other doesn't, then
;;; special care must be taken when copying the vector.
(defun maybe-make-slot-from-val (facetp class sname prop1 prop2 val)
  (if val
      (if facetp
          (cond
            ((arrayp val)
             (cond ((not (eq prop1 prop2))
                    (setq val
                          (let ((newval nil))
                            (cond (prop1
                                   (setq newval (get-free-frame
                                                 (1+ (length val))
                                                 :adjustable T))
                                   (setf (aref newval 0) (aref val 0))
                                   (setf (aref newval 1) 1)
                                   (replace newval val :start1 2
                                                       :start2 1))
                                  (T
                                   (setq newval (get-free-frame
                                                 (1- (length val))
                                                 :adjustable T))
                                   (setf (aref newval 0) (aref val 0))
                                   (replace newval val :start1 1 :start2 2))))))
                   (T
                    (setq val (copy-vector val))
                    (if prop1 (incf (aref val 1)))
                    val)))
            (T
             (make-frame-slot class sname (list :value val :depth 1))))
          val)))

;;; Like make-frame except used by def-frame, which passes it a plist
;;; of non-keyword slots in a real list, and the frame type pftype.
;;; Makes a default frame (the same kind make-frame would return) but it
;;; assumes that all slots and facets are provided.
;;; The facet-plist for slots that have them are evaluated here.
;;; 7-Jan-87: Took eval-plist out since now eval is done at make time.
;;; 25-Jan-87: Put eval-plist back in here since it causes too many
;;; semantic and implementation hassles when you interpret class and instance
;;; slot values differently (e.g., you can inherit from and fire demons on
;;; both classes and instances).
(defun make-default-frame (class)
  (let* ((pftype (name-to-frame-type class))
         (plist (pa-frame-slots pftype))
         (newframe (get-free-frame (pa-frame-numslots pftype)
                                   :adjustable T))
         (fi-plist (pa-frame-index-plist pftype))
         (facetp (getf (pa-frame-c-slots pftype) :facets))
         sval snum)
    (dolist (sname (pa-frame-snames pftype))
      (setq snum (get-slot-num fi-plist sname))
      (cond ((or (not (faceted-p2 fi-plist sname)) (eq facetp :NO))
             (setq sval (getf plist sname *DEFAULT*))
             (if (eq sval *DEFAULT*)
                 (ml-error :not-all-values)))
            (T (setq sval (getf plist sname *DEFAULT*))
               (if (eq sval *DEFAULT*)
                   (ml-error :not-all-values)
                   (setq sval (make-default-frame-slot (eval-plist sval))))))
      (setf (aref newframe snum) sval))
    (setf (aref newframe 0) class)
    (push newframe (get class :instances))
    newframe))

;;; If the facet plist has no facets, then add the default one (value).
(defun parm-valueify (fplist)
  (or fplist
      '(:value NIL)))


;;;; -----------------------------------------------------------------------
;;;; RELATION CODE
;;;; -----------------------------------------------------------------------

;;; Nondestructive. Returns the union of l1 and l2, guaranteeing that the
;;; order of the elts in the result is the same as the order in (append l1 l2).
(defun ordered-union (l1 l2)
  (do ((l2 l2 (cdr l2))
       (res NIL (if (memq (car l2) l1) res (cons (car l2) res))))
      ((null l2) (append l1 (nreverse res)))))

(defun make-index-plist1 (plist facetp)
  (do ((plist plist (cddr plist))
       (snum 0 (1+ snum))
       (new-plist NIL
                  (cons (make-new-plist snum (cadr plist) facetp)
                        (cons (assure-keyword (car plist)) new-plist))))
      ((null plist) (nreverse new-plist))))

(defun make-new-plist (snum facets facetp)
  (cons snum (make-new-facets 0 facets facetp)))

;;; EX: (1 (foo 'val1 bar 'val2)) ==> (foo 1 bar 2)
(defun make-new-facets (origin facets facetp)
  (if (or (atom facets) (eq facetp :NO))
      facets
      (do ((fnum origin (1+ fnum))
           (facets facets (cddr facets))
           (res NIL
                (cons fnum (cons (assure-keyword (car facets)) res))))
          ((null facets) (nreverse res)))))

(defun combine-cache-slots (lcached all-slot-names full-cslots parents)
  (when (not (eq lcached NIL))
    (if (not (eq lcached *DEFAULT*))
        (if (consp lcached)
            (do ((lcached lcached (cdr lcached)))
                ((null lcached))
              (setf (car lcached) (assure-keyword (car lcached))))))
    (let ((all '(:%class)))
      (do ((cslot full-cslots (cdr cslot))
           (parent parents (cdr parent)))
          ((null cslot)
           (ordered-union all
                          (translate-all2 lcached all-slot-names)))
        (setq all (ordered-union all (translate-all (getf (car cslot) :cache)
                                                    (car parent))))))))

(defun combine-getter-slots (lgetters all-slot-names full-cslots parents)
  (when (not (eq lgetters NIL))
    (if (not (eq lgetters *DEFAULT*))
        (if (consp lgetters)
            (do ((lgetters lgetters (cdr lgetters)))
                ((null lgetters))
              (setf (car lgetters) (assure-keyword (car lgetters))))))
    (let ((all *DEFAULT*))
      (do* ((cslot full-cslots (cdr cslot))
            (parent parents (cdr parent))
            (pgetters (getf (car cslot) :getters *DEFAULT*)
                      (getf (car cslot) :getters *DEFAULT*)))
           ((null cslot)
            (if (eq all *DEFAULT*)
                (translate-all3 lgetters all-slot-names)
                (ordered-union all (translate-all2 lgetters all-slot-names))))
        (if (not (eq pgetters *DEFAULT*))
            (if (eq all *DEFAULT*)
                (setq all (translate-all3
                           pgetters (get-slot-names (car parent))))
                (setq all
                      (ordered-union
                       all (translate-all pgetters (car parent))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro get-set-type (setable)
    `(if (consp ,setable) (car ,setable) ,setable))

  (defmacro get-setable-slots (setable)
    `(if (listp ,setable) (cadr ,setable) :*ALL*)))

;;; lsetable is *DEFAULT* if it wasn't specified in the def-frame.
(defun combine-setable-slots (lsetable all-slot-names full-cslots parents)
  (when (not (eq lsetable NIL))
    (if (not (eq lsetable *DEFAULT*))
        (if (consp lsetable)
            (do ((slots (cadr lsetable) (cdr slots)))
                ((null slots))
              (setf (car slots) (assure-keyword (car slots))))
            (setq lsetable (if lsetable (assure-keyword lsetable)))))
    (let ((all '())
          (setp (get-set-type lsetable)))
      (do* ((cslot full-cslots (cdr cslot))
            (parent parents (cdr parent))
            (psetable (getf (car cslot) :setable) (getf (car cslot) :setable)))
           ((null cslot)
            (list setp (ordered-union all (translate-all2
                                           (get-setable-slots lsetable)
                                           all-slot-names))))
        (if (and psetable (not setp))
            (setq setp (get-set-type psetable)))
        (if (consp psetable)
            (setq all (ordered-union all (translate-all (cadr psetable)
                                                        (car parent)))))))))


(defun translate-all (val parent)
  (cond ((eq val :*ALL*)
         (cdr (get-slot-names parent)))         ;;Avoid %class
        ((eq val *DEFAULT*)
         NIL)
        (T val)))

(defun translate-all2 (val snames)
  (cond ((or (eq val :*ALL*) (eq val T))
         snames)
        ((eq val *DEFAULT*)
         NIL)
        (T val)))

(defun translate-all3 (val snames)
  (cond ((eq val :*ALL*)
         snames)
        (T val)))

;;; Modified on 5-Dec-86 to only inherit slots specified by the 'cache
;;; class slot, instead of all of them as before.
(defun find-frame-slots (framenames)
  (mapcar
   #'(lambda (fname)
       (let ((cached-slots (get-cslot fname :CACHE))
             (all-specs (all-frame-slots fname)))
         (if (eq cached-slots :*ALL*)
             all-specs
             (find-specs cached-slots all-specs fname))))
   framenames))

;;; Return the slot spec for every slot named in cached-slots.
(defun find-specs (cached-slots all-specs fname)
  (let (res)
    (dolist (cached-slot cached-slots (nreverse res))
      (let ((find (getf all-specs cached-slot *DEFAULT*)))
        (cond ((not (eq find *DEFAULT*))
               (push cached-slot res)
               (push find res))
              (T (ml-format T :cached-slot cached-slot fname)))))))

(defun all-frame-slots (framename)
  (pa-frame-slots (name-to-frame-type framename)))

(defun find-frame-cslots (framenames)
  (mapcar
   #'(lambda (fname)
       (pa-frame-c-slots (name-to-frame-type fname)))
   framenames))

;;; Returns a plist associating the slots in the parents with the list
;;; of facets, making sure there is only one occurrence of each slot.
(defun combine-slots (frame-slots)
  (let (res)
    (dolist (frame-slot frame-slots (nreverse res))
      (do ((plist frame-slot (cddr plist)))
          ((null plist))
        (when (not (memq (car plist) res))
          (push (car plist) res)
          (push (cadr plist) res))))))

;;; Since p2 implicitly has an is-a relation to p1, appendp is NIL in
;;; the first call to plist-union*
(defun plist-union (p1 p2 propagatep relations facetp)
  (do ((relations relations (cddr relations))
       (p2 (plist-union* p1 p2 propagatep NIL facetp)
           (plist-union-2* p2 (classify (find-inherited-slots relations))
                           propagatep (car relations) facetp)))
      ((null relations) p2)))

;;; Need to add a dummy class slot to the first slot plist so that the
;;; plist-union functions are guaranteed to have a class in the first
;;; position.
(defun classify (splist)
  (if (not (memq ':%CLASS (car splist)))
      (cons (cons ':%CLASS (cons NIL (car splist)))
            (cdr splist))
      splist))

(defun get-appendp (relation)
  (car (memq (get-slot relation :combination-type)
             '(:append :nconc append nconc))))

;;; Return the slot plist indicated by the relation (car relations) and related
;;; class (cadr relation).
(defun find-inherited-slots (relations)
  (get-slot-specs (car relations) (cadr relations)))

;;; relvalue is the frame from which a value is being inherited.
;;; It may be an atom, a frame array or a slot array whose value
;;; facet holds the frame.
(defun get-slot-ref (slot-inherited relvalue facetp firstp)
  (let ((relvalue (if (consp relvalue) (car relvalue) relvalue)))
    (if facetp
        (get-slot-faceted slot-inherited relvalue firstp)
        (get-slot-unfaceted slot-inherited relvalue))))

;;; For unfaceted classslot
(defun get-slot-unfaceted (sname classslot)
  (get-generic-value classslot sname))

(defun get-slot-faceted (sname frame firstp)
  (if firstp
      (get-first-slot-spec sname frame)
      (get-slot-spec0 sname frame)))

(defun get-first-slot-spec (sname framenames)
  (let ((framename (if (consp framenames) (car framenames) framenames)))
    (if framename
        (get-slot framename sname))))

(defun get-slot-spec0 (sname framenames)
  (if (consp framenames)
      (mapcan #'(lambda (framename)
                  (copy-list (get-value framename sname)))
              framenames)
      (get-value framenames sname)))

;;; EX: (get-slot-specs 'part-of '(table1 table2))
;;; Return a list of the slot plists in the classes, using relation as the guide.
(defun get-slot-specs (relation classes)
  (let* ((slot-spec (get-inherited-slot-names relation)))
    (mapcar #'(lambda (class)
                (let ((pftype (name-to-frame-type class)))
                  (if (not pftype)
                      (ml-format T :got-but-not-class relation class class)
                      (get-specified-slots
                       slot-spec
                       (pa-frame-slots (name-to-frame-type class))))))
            classes)))

;;; Like get-slot-specs except only returns a list of the slot names that
;;; are inherited through the relation; i.e., no plist since this is for
;;; the make-function writer, which doesn't know at writing time what the
;;; values are.
(defun get-inherited-slot-names (relation)
  (get-generic-value relation :slots-inherited))

;;; Return the slots specified in the list slot-spec, that are in the
;;; plist slots.
(defun get-specified-slots (slot-specs slots)
  (if (or (eq slot-specs :*ALL*)
          (and (consp slot-specs) (equal slot-specs '(:*ALL*))))
      slots
      (let (sfiller sname res)
        (dolist (slot-spec slot-specs (nreverse res))
          (setq sname (assure-keyword (maybe-car slot-spec)))
          (when (setq sfiller (getf slots sname))
            (push sname res) (push sfiller res))))))

(defun maybe-car (spec)
  (if (atom spec) spec (car spec)))

;;; Creates a plist with properties from P1 and P2.  If a property
;;; exists in both, then the property value in P1 is used.
;;; Destructive with regards to P1.
(defun plist-union-no-propagate (p1 p2)
  (do ((plist2 p2 (cddr plist2))
       (res NIL) (cached NIL))
      ((null plist2)
       ;; FIXME: Reimplements NRECONC.
       (nconc (nreverse res) p1))
    (let ((sname (assure-keyword (car plist2))))
      (push sname res)
      ;; FIXME: Should use GETF instead of MEMQ.
      (cond ((not (setq cached (memq sname p1)))
             (push (cadr plist2) res))
            (T (push (cadr cached) res)
               (setf p1 (delete-slot sname p1)))))))

;;; Destructive version of combine-slots.  Added 18-Jan-87.
(defun combine-slots2 (frame-slots)
  (cond ((< (length frame-slots) 2)
         (car frame-slots))
        (T
         (let* ((orig (nreverse (apply #'nconc frame-slots)))
                (prev orig))
           (do ((plist orig (cddr plist)))
               ((null plist) (nreverse orig))
             (if (memq (cadr plist) (cdddr plist))
                 (cond ((eq prev plist)
                        (setf orig (cddr orig))
                        (setq prev orig))
                       (T
                        (setf (cddr prev) (cddr plist))
                        (setq plist prev)))
                 (if (not (eq prev plist)) (setq prev (cddr prev)))))))))

;;; Return the plist which is the plist-union of p1 & p2, the values of p1
;;; taking priority.  The position of p2 also has priority.  Mangles p1 and p2.
;;; If propagatep is true then marker propagation is desired, so the depth and
;;; breadth of slots must be kept track of.
;;; If append is true then the common slot values from p1 & p2 will be appended
;;; to make the final slot description; otherwise the slot descriptions in p1
;;; will take precedence.
;;; With slot appending, propagating is not allowed [it's not clear that
;;; it would be useful, and it seems like a pain (if not impossible)
;;; to implement].
(defun plist-union* (p1 p2 propagatep relation facetp)
  (keywordize-plist p1)
  (let ((combotype (if (null relation) :FIRST
                       (assure-keyword
                        (get-slot relation :combination-type))))
        (slotsin (if (null relation) :*ALL*
                     (get-generic-value relation :slots-inherited))))
    (plist-combine p1 (combine-slots2 p2) combotype slotsin propagatep
                   facetp)))

;;; Like plist-union* but the position of p1 AND the values in p1
;;; have priority.
(defun plist-union-2* (p1 p2 propagatep relation facetp)
  (keywordize-plist p1)
  (let ((combotype (if (null relation) :FIRST
                       (assure-keyword
                        (get-slot relation :combination-type))))
        (slotsin (if (null relation) :*ALL*
                     (get-generic-value relation :slots-inherited))))
    (simple-plist-combine p1 (combine-slots2 p2) combotype slotsin
                          propagatep facetp)))

;;; Like plist-union-no-propagate except when both p1 and p2 contain
;;; a description of a slot, the result is to append the descriptions
;;; instead of using only p1's description.
;;; Re-written 18-Jan-87.  Only appends for the value facet.
;;; This is a merging of plist-append and plist-union-no-propagate
;;; (19-Jan-87).
;;; Modified 25-Jan-87 to also do propagate bookkeeping iff propagatep.
(defun plist-combine (p1 p2 combotype slotsin propagatep facetp)
  (do ((plist2 p2 (cddr plist2))
       (res NIL)
       (cached NIL))
      ((null plist2)
       (nreconc res (propagate-init-plist propagatep p1 facetp)))
    (let ((sname (assure-keyword (car plist2))))
      (push sname res)
      (cond ((not (setq cached (memq sname p1)))
             (push (propagate-update
                    propagatep
                    (propagate-init propagatep (cadr plist2) facetp))
                   res))
            (T
             (push (propagate-init
                    propagatep (maybe-fplist-append
                                (cadr cached) (cadr plist2) sname
                                combotype slotsin facetp)
                    facetp)
                   res)
             (setf p1 (delete-slot sname p1)))))))

;;; Like plist-combine, but p1's values AND positions have priorities.
(defun simple-plist-combine (p1 p2 combotype slotsin propagatep facetp)
  (do ((plist2 p2 (cddr plist2))
       (res NIL)
       (cached NIL))
      ((null plist2)
       (nconc (propagate-init-plist propagatep p1 facetp) (nreverse res)))
    (let ((sname (assure-keyword (car plist2))))
      (cond ((not (setq cached (memq sname p1)))
             (push sname res)
             (push (propagate-update
                    propagatep (propagate-init propagatep (cadr plist2)
                                               facetp)) res))
            (T
             (let ((maybe-append (maybe-fplist-append
                                  (cadr cached) (cadr plist2) sname combotype
                                  slotsin facetp)))
               (when (not (eq maybe-append (cadr cached)))  ;; then we appended
                 (setf (getf p1 sname)
                       (propagate-init propagatep maybe-append facetp)))))))))


;;; Added 19-Jan-87.  Like facetplist-append if slotsin specifies :append
;;; for the given slot sname, or if slotsin doesn't specify anything for
;;; sname and combotype is :append; else uses :first.
;;; Modified 25-Jan-87 to initialize the depth facet if necessary.
(defun maybe-fplist-append (fp1 fp2 sname default-combo slotsin facetp)
  (cond ((eq facetp :NO)
         (or fp1 fp2))
        (T
         (if (consp fp1) (keywordize-plist fp1))
         (let ((combo (get-combotype default-combo sname slotsin)))
           (cond ((and (or (eq combo :append) (eq combo :nconc))
                       (consp fp1) (consp fp2))
                  `(:VALUE (,(intern-current combo)
                            ,(fplist-assure-list (cadr fp1))
                            ,(fplist-assure-list (cadr fp2)))
                           ,@(cddr fp1)
                           ,@(find-extra-facets fp1 fp2)))
                 ((and (consp fp1) (consp fp2))
                  (add-facets fp1 (find-extra-facets fp1 fp2)))
                 (T fp1))))))

;;; Returns the facets from <extra> nconc'ed onto fp1.  If <extra> has
;;; a :value or :depth facet and fp1, then those facet descriptions are put
;;; onto the front of the result.  The rest of the <extra> facets are put
;;; at the end.
(defun add-facets (fp1 extra)
  (when (and (consp fp1) (consp extra))
    (cond ((and (not (eq (car fp1) :VALUE))
                (eq (car extra) :VALUE))
           (setq fp1 (append `(:VALUE ,(second extra)) fp1))
           (setq extra (cddr extra))
           (when (and (not (eq (third fp1) :DEPTH))
                      (eq (first extra) :DEPTH))
             (setf (cddr fp1) (append `(:DEPTH ,(incf (second extra)))
                                      (cddr fp1)))
             (setq extra (cddr extra))))
          (T (remf extra :DEPTH))))
  (nconc fp1 extra))

;;; Added 9-Mar-1988.  Returns a facet-plist of any facets in fp2 not in fp1.
(defun find-extra-facets (fp1 fp2)
  (when (consp fp2)
    (let ((extra-fp NIL))
      (do* ((fp2 fp2 (cddr fp2)))
           ((null fp2) (nreverse extra-fp))
        (when (and (consp fp1) (not-getf fp1 (car fp2)))
          ;;               (not (eq (car fp2) :DEPTH))
          ;;               (not (eq (car fp2) :VALUE))
          (push (car fp2) extra-fp)
          (push (cadr fp2) extra-fp))))))

;;; This needs to be written correctly.
(defun fplist-assure-list (thang)
  thang)

(defun get-combotype (default sname slotsin)
  (assure-keyword
   (or (and (not (eq slotsin :*ALL*))
            (cadar (member-if #'(lambda (sentry)
                                  (and (consp sentry)
                                       (eq
                                        ;; FIXME: This should likely
                                        ;; be (assure-keyword (car
                                        ;; sentry)) but needs a
                                        ;; failing test case first.
                                        (car sentry)
                                        sname)
                                       (cadr sentry)))
                              slotsin)))
       default)))

;;; Modified 25-Jan-87 to work with plist-combine.
(defun propagate-init (propagatep facet-plist facetp)
  (when (and (consp facet-plist) (not (eq facetp :NO)))
    (if (not (eq (car facet-plist) :VALUE))
        (setq facet-plist (append '(:VALUE NIL) facet-plist)))
    (if (and propagatep (not (eq (third facet-plist) :DEPTH)))
        (setf (cddr facet-plist)
              (append '(:DEPTH 0) (cddr facet-plist)))))
  facet-plist)

;;; Modified 25-Jan-87 to work with plist-combine.
(defun propagate-init-plist (propagatep plist facetp)
  (if (and plist (not (keywordp (car plist))))
      (setf (car plist) (current-to-keyword (car plist))))
  (let ((orig plist))
    (do ((plist (cdr plist) (cddr plist)))
        ((null plist) orig)
      (if (and (cadr plist) (not (keywordp (cadr plist))))
          (setf (cadr plist) (current-to-keyword (cadr plist))))
      (setf (car plist)
            (maybe-propagate-init-facet-plist propagatep (car plist)
                                              facetp))))
  plist)

(defun maybe-propagate-init-facet-plist (propagatep fplist facetp)
  (when (and (consp fplist) (not (eq facetp :NO)))
    (do ((facets fplist (cddr facets)))
        ((null facets) T)
      (if (not (keywordp (car facets)))
          (setf (car facets) (assure-keyword (car facets)))))
    (if (not (memq :value fplist))
        (setf fplist (append '(:value NIL) fplist)))
    (if propagatep
        (setf fplist (propagate-init T fplist facetp))))
  fplist)

;;; Modified 25-Jan-87 to work with plist-combine.
;;; evenp and memq checks are sort of hacks.  The consp, evenp and memq
;;; try to see if the plist is in fact a facet plist rather than a slot
;;; value which happens to be a list.  It should check the slot-index of
;;; the frame class type, but it doesn't always know this. (19-June-87)
(defun propagate-update (propagatep facet-plist)
  (cond ((not propagatep)
         (if (consp facet-plist) (parm-valueify facet-plist) facet-plist))
        (T (if (consp facet-plist)
               (let ((newplist (copy-list facet-plist)))
                 (if (and (memq :depth newplist) (evenp (length newplist)))
                     (incf (getf newplist :depth)))
                 newplist)
               facet-plist))))

;;; Destructively deletes the slot with the given name from the plist
;;; and returns the new plist.
(defun delete-slot (sname plist)
  ;; FIXME: This seems to reimplement REMF.
  (let ((orig plist))
    (do ((plist plist (cddr plist))
         (backone NIL plist))
        ((eq sname (car plist))
         (if backone
             (setf (cddr backone) (cddr plist))
             (setf orig (cddr orig)))
         orig)
      (if (null plist) (return orig)))))


;;;; -----------------------------------------------------------------------
;;; SLOT ACCESSOR FUNCTIONS, BOOK-KEEPING.
;;;; -----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro getablep (sname getters)
    `(or (eq ,getters *DEFAULT*)
         (memq ,sname ,getters))))

;;; Define the frame-slot accessors and storers (setf methods) since
;;; the frame is adjustable and aref is needed.
;;; Also writes the maker function, with same syntax as defstruct make
;;;   fns.
;;; Added 27-Sep-86:  setable parameter.  Only write the setf methods
;;; if setable is :setf.
(defun write-accessor-functions (theframe name plist setable propagatep
                                 getters facetp)
  (do* ((slot-names plist (cddr slot-names))
        (sname (car slot-names) (car slot-names))
        (ssetable (memq sname (cadr setable)) (memq sname (cadr setable)))
        (sgetable (getablep sname getters) (getablep sname getters))
        (frame-slot (if (or ssetable sgetable)
                        (intern (concatenate 'string (symbol-name name) "-"
                                             (symbol-name sname))))
                    (if (or ssetable sgetable)
                        (intern (concatenate 'string (symbol-name name) "-"
                                             (symbol-name sname)))))
        (slotnum 0 (1+ slotnum)))
       ((null slot-names)
        (setf (pa-frame-numslots theframe) slotnum))
    (when sgetable
      (cond ((or (atom (cadr slot-names)) (eq facetp :NO))
             (define-aref-slot-accessor frame-slot slotnum))
            (T
             (write-facet-refs frame-slot slotnum (cadr slot-names)
                               (car setable) ssetable propagatep))))
    (when ssetable
      (let ((set-slot-fname (smash "SET-" frame-slot)))
        (define-slot-setter set-slot-fname slotnum)
        (if (eq (car setable) :setf)
            (define-slot-setf-method frame-slot set-slot-fname))))))

(defun dynamic-define-generic-slot-accessor (snum fnum access)
  `(,access (,access frame ,snum) ,fnum))

;;; Dynamic programming technique. Given snum, fnum, check to see if the
;;; access function for slot snum, facet fnum has yet been defined.  If
;;; not, then define it and store it so the next time it won't have to be
;;; redefined.
;;; plist is passed by name so this function can push onto it.
(defun get-generic-slot-accessor (fn-name snum fnum plist access)
  (let ((fnum-plist (memq snum (symbol-value plist))))
    (cond (fnum-plist
           (let ((slot-facet-fn (getf (cadr fnum-plist) fnum)))
             (cond (slot-facet-fn
                    (pa-eval
                     `(defun ,fn-name (frame)
                        ,slot-facet-fn)))
                   (T
                    (let ((new-fn
                            (dynamic-define-generic-slot-accessor snum fnum access)))
                      (push new-fn (cadr fnum-plist))
                      (push fnum (cadr fnum-plist))
                      (pa-eval
                       `(defun ,fn-name (frame)
                          ,new-fn)))))))
          (T (let ((new-fn (dynamic-define-generic-slot-accessor snum fnum access)))
               (push (list fnum new-fn) (symbol-value plist))
               (push snum (symbol-value plist))
               (pa-eval `(defun ,fn-name (frame)
                           ,new-fn)))))))

(defun get-aref-slot-accessor (fn-name snum fnum)
  (get-generic-slot-accessor fn-name snum fnum '*AREF-SLOT-FN-PLIST* 'aref))

(defun define-slot-accessor (frame-slot slotnum)
  (define-aref-slot-accessor frame-slot slotnum))

(defun define-aref-slot-accessor (frame-slot slotnum)
  (if (<= slotnum 15)
      (pa-eval `(setf (symbol-function ',frame-slot)
                      (symbol-function ',(aref *AREF-FN-MAP* slotnum))))
      (pa-eval
       `(defun ,frame-slot (frame)
          (aref frame ,slotnum)))))

(defun define-ref-slot-facet-accessor (fname snum fnum)
  (define-aref-slot-facet-accessor fname snum fnum))

(defun define-aref-slot-facet-accessor (fn-name snum fnum)
  (cond ((and (= fnum 0) (<= snum 15))
         (pa-eval `(setf (symbol-function ',fn-name)
                         (symbol-function ',(aref *AREF-FACET1-FN-MAP* snum)))))
        (T
         (get-aref-slot-accessor fn-name snum fnum))))

(defun define-slot-setter (set-slot-fname slotnum)
  (pa-eval
   `(defun ,set-slot-fname (frame value)
      (setf (aref frame ,slotnum) value))))


;;; Example of what this function would write given house-height and 1:
;;; (defsetf house-height (frame) (newval)
;;;       `(setf (aref ,frame 1) ,newval))
(defun define-slot-setf-method (frame-slot set-slot-fname)
  (pa-eval
   `(defsetf ,frame-slot ,set-slot-fname)))

;;; Add: maybe write frame-slot-facet accessor given a slot.
;;; Sample facet accessor: given house, size, 1, (value depth):
;;; (defun house-size.value (frame) (aref (aref frame 1) 0))
;;; (defun house-size.depth (frame) (aref (aref frame 1) 1))
;;; (defsetf house-height.value (frame) (newval)
;;;       `(setf (aref (aref ,frame 1) 0) ,newval))
;;; (defsetf house-height.depth (frame) (newval)
;;;       `(setf (aref (aref ,frame 1) 1) ,newval))
;;; As of 7-Sep-86, it only defines functions and setf methods for the 0th
;;; facet because it's very expensive to do so for all facets.
(defun write-facet-refs (frame-slot snum facet-names set-type ssetable propagatep)
  (let* ( ;; (facet-prefix (smash frame-slot "."))
         ;; (slot-prefix (smash frame-slot "-"))
         (facet-name (car facet-names))
         (fn-name (intern (concatenate 'string (symbol-name frame-slot) "."
                                       (symbol-name facet-name)))))
    ;; (define-aref-slot-accessor (smash slot-prefix facet-name) 0)
    (define-aref-slot-facet-accessor fn-name snum 0)
    (when ssetable
      (let ((set-fn-name (smash "SET-" fn-name)))
        (define-facet-setter-fn set-fn-name snum 0 propagatep)
        (if (eq set-type :setf)
            (define-facet-setf-method fn-name set-fn-name))))))

(defun define-facet-setter-fn (set-fn-name snum fnum propagatep)
  (pa-eval
   (if (and propagatep (= fnum 0))
       `(defun ,set-fn-name (frame value)
          (setf (aref (aref frame ,snum) ,fnum) value)
          (setf (aref (aref frame ,snum) 1) 0)) ;;set depth facet to 0
       `(defun ,set-fn-name (frame value)
          (setf (aref (aref frame ,snum) ,fnum) value)))))

(defun define-facet-setf-method (fn-name set-fn-name)
  (pa-eval
   `(defsetf ,fn-name ,set-fn-name)))

(defun rename-frame (oldname newname)
  (let ((frame-class (if (framep oldname) (frame-class oldname))))
    (cond ((not frame-class)
           (ml-cerror :skip-rename :no-such-frame-error oldname))
          ((frame newname)
           (ml-cerror :skip-rename :frame-exists newname))
          (T
           (nsubst newname oldname (instance-names-of frame-class))
           (setf (get newname :frame) (frame oldname))
           (remprop oldname :frame)
           newname))))

(defun init-parmenides ()
  (setq *AREF-SLOT-FN-PLIST* NIL)
  (format T "~&Parmenides 1.5, Copyright (c) 1985, 1988 Carnegie Mellon.~%"))

(init-parmenides)

;;; Has-inverses and inverse-name slots added 4-1-87 to support inverse
;;; relations.
(eval-when (:load-toplevel :execute)
  (let ((*parmenides-terse* t))
    (def-frame* 'relation ()
      '(:combination-type :FIRST
        :slots-inherited (value :*ALL*)
        :has-inverses NIL ;; Indicates if the relation has inverses.
        :inverse-name NIL))))

;;;  (Def-frame inverse-relation (:is-a (relation)))

;;; The IS-A and PART-OF relations
;;; (def-frame is-a (is-a (relation) propagate NIL)
;;;  combination-type first
;;;  slots-inherited (value :*ALL*)
;;;  has-inverses T)
