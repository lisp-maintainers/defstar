;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- ;;;;;;;;;;;;;;;;;80

(in-package :cl-user)


;;; Note: documentation generated with CLDOC, using the following form:
#-(cldoc:extract-documentation 'cldoc:html
      "/home/paul/lisp/defstar/html" 
      (asdf:find-system :defstar)
      :table-of-contents-title
      "DEFSTAR"
      :section-names '("Arguments:" "Returns:" "Examples:" "See Also:"
                       "See also:"
                       "Example:" "Examples:"  "Notes:" "Description:"
                       "Limitations:"
                       "Examples of DEFVAR* and DEFPARAMETER*:"
                       "Examples of DEFUN* and DEFMETHOD* usage:"
                       "Type DECLARATION versus type CHECKING:"))


(defpackage :defstar
  (:use :cl)
  (:export #:defun*
           #:defmethod*
           #:defgeneric*
           #:defvar*
           #:defparameter*
           #:flet*
           #:labels*
           #:lambda*
           #:returns
           #:return-value
           #:*check-argument-types-explicitly?*
           #:->)
  (:documentation
   " * Description:

DEFSTAR is a collection of macros that can be used in place
of DEFUN, DEFMETHOD, DEFGENERIC, DEFVAR, DEFPARAMETER, FLET, LABELS and LAMBDA. Each
macro has the same name as the form it replaces, with a star added at the
end (e.g. DEFUN*).

DEFSTAR's home is at:
- {http://bitbucket.org/eeeickythump/defstar/}

Installation requires ASDF. DEFSTAR does not depend on any other libraries.

The macros allow:
- easy inclusion of type declarations within lambda lists
- easy inclusion of return type declarations in function and method definitions
- easy inclusion of assertions for each argument and for the function's
return value

See {defmacro defstar:defun*} and {defmacro defstar:defvar*} for a detailed
description of syntax. See also the examples below.

* Type DECLARATION versus type CHECKING:

Technically, DECLARE, DECLAIM and the like do not actually check that
values stored in the associated variables conform to the declared type.
They merely constitute a promise BY THE PROGRAMMER that only values of
the specified type will be stored there. The consequences of storing
a string in a variable that is declared to be of type integer, are
technically 'undefined'.

In practice, most modern Common Lisp implementations perform type-checking
based on declaration information, especially when SAFETY is high.

DEFSTAR allows you to force lisp to perform type checking based on
declarations. If you set the global variable
{defvar defstar:*CHECK-ARGUMENT-TYPES-EXPLICITLY?*}
to non-nil, CHECK-TYPE forms will included in the body of each function
or method, causing an error to be raised if a value does not match
its declared type.

* Examples of DEFUN* and DEFMETHOD* usage:
;;; ;; Define a simple function that adds two numbers, both of which
;;; ;; are declared to be real.
;;; (defun* sum ((a real) (b real))
;;;    (+ a b))

;;; ;; Now also declare that the function returns a real.
;;; (defun* (sum -> real) ((a real) (b real))
;;;    (+ a b))

;;; ;; Another way of declaring the function's return type.
;;; (defun* sum ((a real) (b real))
;;;    (returns real)
;;;    (+ a b))

;;; ;; We want to ensure that a and b are never negative.
;;; ;; One way is to alter the type declarations:
;;; (defun* (sum -> (real 0)) ((a (real 0)) (b (real 0)))
;;;    (+ a b))

;;; ;; Another way is to define a new type:
;;; (deftype natural () '(real 0))
;;; (defun* (sum -> natural) ((a natural) (b natural))
;;;    (+ a b))

;;; ;; Another way is to use assertions:
;;; (defun* (sum -> real (>= return-value 0)) ((a real (>= a 0)) (b real (>= b 0)))
;;;    (+ a b))

;;; ;; Or:
;;; (defun* sum ((a real (>= a 0)) (b real (>= b 0)))
;;;    (returns real (>= return-value 0))
;;;    (+ a b))

;;; ;; Or, using the feature that the names of single-argument predicate
;;; ;; functions can be used as assertions:
;;; (defun* (naturalp -> boolean) ((x real))
;;;    (not (minusp x)))
;;; ...
;;; (defun* (sum -> real naturalp) ((a real naturalp) (b real naturalp))
;;;    (+ a b))

;;; ;; A function that returns multiple values.
;;; (defun* (floor -> (values integer integer)) ((n real) (d real))
;;;    (cl:floor n d))

;;; ;; It is possible to use assertions with functions that return
;;; ;; multiple values. When a function is declared to return multiple
;;; ;; values, RETURN-VALUE will be bound to a LIST of those values.
;;; (defun* floor ((n real) (d real))
;;;    (returns (values integer integer)
;;;             (< (second return-value) (first return-value)))
;;;    (cl:floor n d))

;;; ;; To declare that a function returns an unspecified number of
;;; ;; values, of unspecified types:
;;; (defun* (floor -> (values)) ((n real) (d real))
;;;    ...)

;;; ;; The type of a &REST argument can be declared. The declaration
;;; ;; refers to the types of each element in the list of arguments
;;; ;; stored in the &REST argument.
;;; (defun* (+ -> real) (&rest (numbers real))
;;;    (apply #'cl:+ numbers))

;;; ;; More complicated lambda list.
;;; ;; Note that the function and its first argument do not have type
;;; ;; declarations.
;;; ;; Also note the syntax of typed keyword arguments:
;;; ;; ((var TYPE [ASSERTION]) DEFAULT [SUPPLIEDP])
;;; ;; Note that &OPTIONAL arguments use the same syntax.
;;; (defun* my-find (item (seq sequence) &key (from-end boolean)
;;;                  ((test (function (t))) nil) ((test-not (function (t))) nil)
;;;                  ((key (function (t))) nil) (start fixnum) (end fixnum))
;;;    ...function body...)

;;; ;; Example of method definition. All the arguments in the arglist are
;;; ;; normal 'specialised' arguments like you would usually find in a
;;; ;; method definition. The form still allows you to include an assertion
;;; ;; with each argument, however ('plusp' in this case).
;;; (defmethod* (cell-value -> real) :around ((sheet <Sheet>)
;;;                                           (x integer plusp) (y integer plusp))
;;;    ...)

;;; ;; Note that when you declare a return type for a method, the method
;;; ;; body will perform type-checking, but no toplevel DECLAIM form will
;;; ;; be generated.

;;; ;; CLOS function dispatch based on classes is limited; you cannot specialise
;;; ;; on user-defined types unless they are proper classes, for example.
;;; ;; You may therefore sometimes want to declare that a method's argument
;;; ;; is of a particular type, as well as declaring its class for specialisation
;;; ;; as you normally would.
;;; ;; Here is an example. Note the similarity to the syntax for keyword
;;; ;; arguments.
;;; (defmethod* (cell-value -> real) :around ((sheet <Sheet>)
;;;                                           ((x natural plusp) integer)
;;;                                           ((y natural plusp) integer))
;;;    ...)

;;; ;; Example of DEFGENERIC*, mainly useful to declare the return type
;;; ;; of a set of methods.
;;; (defgeneric* (cell-value -> real) (sheet x y))

;;; ;; DEFGENERIC* can also be used to declare types of arguments. Be careful
;;; ;; that these don't clash with specialisers in method definitions.
;;; (defgeneric* (cell-value -> real) (sheet (x natural) (y natural)))

* Examples of DEFVAR* and DEFPARAMETER*:
;;; (defvar* (*user-name* string) \"Bob\")
;;; (defparameter* (*file-position* (integer 0)) 0)

* Limitations:
- Definitions of SETF methods cannot include return type declarations in the
method 'header'. The return type can still be declared using a (RETURNS ...)
form. For example:
;;; (defmethod (setf (foo -> integer)) (...args...)   ; illegal
;;;    ...)

;;; (defmethod (setf foo) (...args...)
;;;    (returns integer)                  ; legal
;;;    ...)
"))

(in-package :defstar)


(declaim (optimize (speed 0) (safety 3) (debug 3)))


;;;; (@> "Utility functions") =================================================


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstar/split-defun-body (body)
    "* Arguments:
- BODY: the body of a DEFUN form or similar, such as might be received
by a macro.

* Returns:
Three values:
- PREAMBLE: list of declaration forms at the start of the body
- DOCSTRING: Documentation string, if present in BODY.
- TRUE-BODY: Actual function body with the above items removed.

* Description:
Internal utility function.
Divide the 'preamble' of a function body from its actual body.
The preamble consists of declarations and a docstring."
    (let ((docstring nil)
          (preamble nil)
          (true-body nil))
      (loop for form on body do
           (cond
             ((and (listp (car form))
                   (member (caar form) '(declare declaim proclaim)))
              (push (car form) preamble))
             ((and (stringp (car form))
                   (null docstring)
                   (cdr form))
              (setf docstring (car form)))
             (t
              (setf true-body form)
              (return))))
      (values (reverse preamble) docstring true-body))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstar/make-keyword (&rest parts)
    "Like MKSYMBOL, but intern the symbol in the KEYWORD package,
creating a keyword called :SYMBOL."
    (intern (string-upcase (format nil "~{~A~}" parts)) :keyword)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstar/ampersand-symbol? (sym)
    "Predicate. Does the symbol SYM begin with an ampersand, such as &ANY,
&REST and so on?"
    (and (symbolp sym)
	 (eql #\& (char (format nil "~A" sym) 0)))))



;;;; (@> "Internal functions and macros") =====================================


(defconstant +DEFUN*-ARROW-SYMBOL+ '->
  "The symbol that separates function name from type declaration
in DEFUN* forms and the like. See {defmacro defstar:defun*}.")

(defvar *check-argument-types-explicitly?* nil
  "* Description:
If non-nil, insert 'CHECK-TYPE' clauses in the preamble of functions,
to force the function's arguments to be type-checked.

Technically, DECLARE, DECLAIM and the like do not actually check that
values stored in the associated variables conform to the declared type.
They merely constitute a promise BY THE PROGRAMMER that only values of
the specified type will be stored there. The consequences of storing
a string in a variable that is declared to be of type integer, are
undefined.

In practise, essentially all modern lisps do perform type checking
based on declarations, especially when the SAFETY setting is high. ")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defun*-term (term last-amp-kwd &key (def-type 'defun))
    "* Arguments:
- TERM: any member of an ordinary lambda list.
- LAST-AMP-KWD: Symbol or nil.
- DEF-TYPE: Symbol denoting the type of toplevel form that is being created.
The default is 'DEFUN.
* Returns:
Four values:
- 1. The term as it should be included in the final argument list for
the toplevel form
- 2. The declaration clause that should be included in the
DECLARE statement within the toplevel form's body
- 3. The type of the term, for inclusion in the argument-list of
the (DECLAIM (FTYPE (FUNCTION arglist RETURN-TYPE) ...)) form for a
function definition.
- 4. The assertion clause.

* Description:
Internal function, used by {defmacro defstar:defun*} to parse lambda list terms.

* See also:
- {defmacro defstar:defun*}"
    (flet ((check-clause (check var)
             (if (and check (symbolp check))
                 (list check var)
                 check)))
      (cond
        ((null last-amp-kwd)
         (cond
           ((listp term)
            (cond
              ((and (eql 'defmethod def-type)
                    (listp (car term)))
               (destructuring-bind ((var vartype &optional check) varclass) term
                 (values (list var varclass) (list 'type vartype var) varclass
                         (check-clause check var))))               
              ((eql 'defmethod def-type)
               (destructuring-bind (var varclass &optional check) term
                 (values (list var varclass) nil varclass
                         (check-clause check var))))
              ((eql 'defgeneric def-type)
               (destructuring-bind (var vartype) term
                 (values var (list 'type vartype var) vartype
                         nil)))
              (t
               (destructuring-bind (var vartype &optional check) term
                 (values var (list 'type vartype var) vartype
                         (check-clause check var))))))
           (t
            (values term nil t nil))))
        ((eql '&rest last-amp-kwd)
         (cond
           ((listp term)
            (destructuring-bind (var vartype &optional check) term
              (values var nil vartype (check-clause check var))))
           (t
            (values term nil t nil))))
        ((or (eql '&optional last-amp-kwd)
             (eql '&key last-amp-kwd))
         (cond  
           ((and (listp term)
                 (eql 'defgeneric def-type))
            (destructuring-bind (var vartype) term
              (values var nil
                      (if (eql '&key last-amp-kwd)
                          (list (defstar/make-keyword var) vartype)
                          vartype)
                      nil)))
           ((and (listp term) (listp (car term)))
            (destructuring-bind ((var vartype &optional check)
                                 default &optional supplied-p) term
              (values (if supplied-p
                          (list var default supplied-p)
                          (list var default))
                      (list 'type vartype var)
                      (if (eql '&key last-amp-kwd)
                          (list (defstar/make-keyword var) vartype)
                          vartype)
                      (check-clause check var))))
           ((listp term)
            (values term nil (if (eql '&key last-amp-kwd)
                                 (list (defstar/make-keyword (car term)) t)
                                 t)
                    nil))
           (t
            (values term nil (if (eql '&key last-amp-kwd)
                                 (list (defstar/make-keyword term) t)
                                 t)
                    nil))))
        ((eql '&aux last-amp-kwd)
         (cond  
           ((and (listp term) (listp (car term)))
            (destructuring-bind ((var vartype &optional check) default) term
              (values (list var default)
                      (list 'type vartype var)
                      nil (check-clause check var))))
           (t
            (values term nil nil nil))))
        ((eql '&allow-other-keys last-amp-kwd)
         (error
          "Malformed lambda list: &ALLOW-OTHER-KEYS must be last term"))
        (t
         (error "Unknown keyword in lambda list: ~S"
                last-amp-kwd))))))
 


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun safe-define (toplevel-form-name fname arglist body)
    "* Arguments:
- TOPLEVEL-FORM-NAME: Symbol denoting the type of toplevel form being defined.
Currently handles 'DEFUN, 'DEFMETHOD, 'FLET, 'LABELS, 'LAMBDA, 'DEFGENERIC.
- FNAME, ARGLIST, BODY: see DEFUN*.

* Returns:
A `defun', `defmethod', `defgeneric' or `lambda' form, or `flet' or
`labels' subclause, with appropriate declarations.

* Description:
Internal function. The workhorse for the macros DEFUN*, DEFMETHOD*,
LAMBDA*, FLET*, and LABELS*.
"
    (let ((method-combo-keywords nil)
          (form-args nil)
          (ftype-args nil)
          (declarations nil)
          (checks nil)
          (returns-clause (find 'returns body
                                :key #'(lambda (x)
                                         (if (listp x) (car x) x))))
          (returns-type t)
          (returns-check nil)
          (final-form nil)
          (amp nil))
      (when (and (eql 'defmethod toplevel-form-name)
                 (not (listp arglist)))
        ;; Extract the qualifiers (eg :AFTER, :AROUND etc)
        (push arglist method-combo-keywords)
        (loop for term in body
             while (not (listp term))
           do (push term method-combo-keywords)
             (pop body))
        (setf arglist (car body))
        (setf body (cdr body)))
      (dolist (term arglist)
        (cond
          ((defstar/ampersand-symbol? term)
           (setf amp term)
           (push term form-args)
           (unless (eql '&aux term)
             (push term ftype-args)))
          (t
           (multiple-value-bind (form-term decl ftype-term check)
               (defun*-term term amp :def-type toplevel-form-name)
             (push form-term form-args)
             (if decl (push decl declarations))
             (if ftype-term (push ftype-term ftype-args))
             (if check (push check checks))))))
      (when returns-clause
        (destructuring-bind (rtype &optional rcheck) (cdr returns-clause)
          (setf returns-type rtype)
          (if rcheck (setf returns-check rcheck))
          (if (and rcheck (symbolp rcheck))
              (setf returns-check `(,rcheck return-value))))
        (setf body (remove returns-clause body :test #'eql)))
      (when (and fname (listp fname)
                 (not (eql 'setf (car fname))))
        (when returns-clause
          (error "DEFUN* ~A ... also contains 'RETURNS' clause in body"
                 fname))
        (destructuring-bind (fun-name arrow rtype &optional rcheck) fname
          (unless (eql arrow +DEFUN*-ARROW-SYMBOL+)
            (error "Malformed DEFUN* header: found ~S, expected arrow symbol (~S)"
                   arrow +DEFUN*-ARROW-SYMBOL+))
          (setf fname fun-name)
          (setf returns-type rtype)
          (if rcheck (setf returns-check rcheck))
          (if (and rcheck (symbolp rcheck))
              (setf returns-check `(,rcheck return-value)))))
      (multiple-value-bind (preamble docstring true-body)
          (defstar/split-defun-body body)
        (setf preamble 
              `(,@(if declarations `((declare ,@declarations)) nil)
                  ,@preamble))
        (if *check-argument-types-explicitly?*
            (setf preamble
                  (append
                   preamble
                   (mapcar #'(lambda (decl)
                               `(check-type ,(third decl) ,(second decl)))
                           declarations))))
        (setf true-body
              `(,@(if (and checks (not (eql 'defgeneric toplevel-form-name)))
                      (mapcar #'(lambda (check)
                                  `(assert ,check)) checks)
                      nil)
                  ,@true-body))
        (setf form-args (reverse form-args)
              ftype-args (reverse ftype-args)
              checks (reverse checks))
        (if (and returns-check (symbolp returns-check))
            (setf returns-check `(,returns-check return-value)))
        (setf final-form
              `(,toplevel-form-name ,@(if fname (list fname) nil)
                                    ,@method-combo-keywords
                                    ,form-args
                                    ,@(if docstring (list docstring) nil)
                                    ,@(if (eql 'defgeneric toplevel-form-name)
                                          nil preamble)
                                    ,@(cond
                                       ((eql 'defgeneric toplevel-form-name)
                                        true-body)
                                       ((and returns-check
                                             (listp returns-type)
                                             (eq 'values (car returns-type)))
                                        `((the ,returns-type
                                            (let ((return-value
                                                   (multiple-value-list
                                                    (progn ,@true-body))))
                                              (assert ,returns-check)
                                              (values-list return-value)))))
                                       (returns-check
                                        `((the ,returns-type
                                            (let ((return-value
                                                   (progn ,@true-body)))
                                              (assert ,returns-check)
                                              return-value))))
                                       (returns-type
                                        `((the ,returns-type
                                            ,(if (cdr true-body)
                                                 `(progn ,@true-body)
                                                 (car true-body)))))
                                       (t
                                        true-body))))
        (cond
          ((and (or declarations returns-type)
                (not (member toplevel-form-name '(defmethod flet labels
                                                  lambda))))
           `(progn
              (declaim (ftype (function ,ftype-args ,returns-type) ,fname))
              ,final-form))
          ((member toplevel-form-name '(flet labels))
           (cdr final-form))
          (t
           final-form))))))


(defmacro defvar/param (toplevel-form-name var value &optional docstring)
  "Internal macro, used by {defmacro defstar:defvar*} and
{defmacro defstar:defparameter*}."
  (cond
    ((listp var)
     (destructuring-bind (varname vartype) var
       `(progn
          (declaim (type ,vartype) ,varname)
          (,toplevel-form-name ,varname ,value
                               ,@(if docstring (list docstring) nil)))))
    (t
     `(,toplevel-form-name ,var ,value
        ,@(if docstring (list docstring) nil)))))




;;;; (@> "Exported macros") ===================================================


(defmacro defun* (fname arglist &body body)
  "* Arguments:
- FNAME: either the name of the function to be created, or a list as specified
below:
;;; fname =   FUNCTION-NAME
;;;         | (FUNCTION-NAME -> TYPE [assertion])
;;; assertion =       FORM
;;;                 | PREDICATE-SYMBOL
Where:
-- TYPE is any valid type specifier
-- FORM is any form, which must return non-nil if the assertion is satisfied,
nil otherwise. Within the form, the symbol RETURN-VALUE is bound to the
value that is about to be returned by the function.
-- PREDICATE-SYMBOL is a symbol, the name of a function that accepts a single
arguments. Equivalent to the form `(PREDICATE-SYMBOL RETURN-VALUE)'.

Note that if the latter (list) form for fname is used, the DEFUN* body may NOT
also contain a `returns' form. Also note that the latter form cannot currently
be used when defining a `(setf ...)' function or method.

- ARGLIST: a DEFUN* LAMBDA LIST, which takes the form:
;;; arglist =   var-term*
;;;           | (var-term* [&optional opt-term+])
;;;           | (var-term* [&key opt-term+])
;;;           | (var-term* [&rest rest-term])
;;; var-term =        VARNAME
;;;                 | (VARNAME TYPE/CLASS [assertion])
;;; rest-term =       VARNAME
;;;                 | (VARNAME ELEMENT-TYPE)
;;; opt-term =        VARNAME
;;;                 | (var-term DEFAULT [SUPPLIEDP])

- BODY: Body of the function form. This may contain a docstring in the usual
place, and may also a single special form beginning with `returns':

;;; returns-form = (RETURNS TYPE [assertion])
If the `returns' form contains an assertion, then within that assertion,
the symbol `RETURN-VALUE' is bound to the value that the function is
about to return.

* Description:
Equivalent to (DEFUN fname arglist . body), but:
- All type declarations within the lambda list will be turned into (DECLARE...)
forms within the function body
- If a return type is declared for the function itself, this will be turned
into a global DECLAIM form that precedes the function
- All assertions within the lambda list will be checked before the function body
is entered.
- Any assertion within a `returns' form will be checked before the function
returns a value.

* Examples:
;;; ;; Very simple example
;;; (defun* (add -> real) ((a real) (b real))
;;;   (+ a b))

;;; ;; Example with assertion for 'b' argument, checked before the
;;; ;; body of the function is entered.
;;; (defun* div ((a real) (b real (/= b 0)))
;;;    (returns real)
;;;    (/ a b))

;;; (defun* sum (&rest (nums real))  ; type of 'rest' var refers to
;;;    (returns real)                ; the type of each list element, ie
;;;    (apply #'+ nums))             ; nums must be a list of REALs

;;; (defun* (sum -> real) (&rest (nums real))  ; alternative form
;;;    (apply #'+ nums))                       ; for above example

;;; ;; This function and first argument have no type declarations.
;;; ;; Keyword argument 'test' accepts a function that takes
;;; ;; two arguments of any type.
;;; (defun* find-in-tree (item (tree cons)
;;;                       &key ((test (function (t t))) #'equal))
;;;  (or (funcall test item tree)
;;;      (and (consp tree)
;;;           (or (find-in-tree item (car tree))
;;;               (find-in-tree item (cdr tree))))))
"
  (safe-define 'defun fname arglist body))


(defmacro defmethod* (fname method-arglist &body body)
  "* Arguments:

Usage is exactly the same as {defmacro defstar:defun*}, except that within
METHOD-ARGLIST, any list in a non-optional position is assumed to be a
specialised lambda list term of the form (VARNAME CLASS [assertion]), rather than a
DEFUN* type-declaring term.

The syntax of METHOD-ARGLIST is therefore:
;;; arglist =   method-term*
;;;           | (method-term* [&optional opt-term+])
;;;           | (method-term* [&key opt-term+])
;;;           | (method-term* [&rest rest-term])
;;; method-term = VARNAME
;;;               | (VARNAME CLASS [assertion])
;;;               | ((VARNAME TYPE/CLASS [assertion]) CLASS)
The rest of the syntax is the same as for DEFUN*.

* Description:
Equivalent to (DEFMETHOD FNAME METHOD-ARGLIST . body) with type declarations
and assertions as per {defmacro defstar:defun*}.

* Examples:
;;; (deftype positive-integer () `(integer 1))
;;; (defmethod (make-coords -> (cons positive-integer positive-integer))
;;;                (((x positive-integer) integer)
;;;                 ((y positive-integer) integer))
;;;    (cons x y))
"
  (safe-define 'defmethod fname method-arglist body))


(defmacro defgeneric* (fname generic-arglist &body options)
  "* Arguments:
Usage is exactly the same as {defmacro defstar:defun*}, except that:
- Assertions are ignored.
- &REST, &KEY and &OPTIONAL arguments must be of the form:
;;; arg =   VARNAME
;;;       | (VARNAME TYPE)

* Examples:
;;; (defgeneric* (length -> integer) (seq &key start) ...options...)

;;; ;; Note that you can declare types for arguments in the generic function
;;; ;; argument list. Be careful that these do not clash with method definitions.
;;; ;; Type declarations for generic function arguments will only be used to
;;; ;; make a toplevel DECLAIM form that will then apply to all methods of
;;; ;; the generic function.
;;; (defgeneric* (length -> integer) ((seq sequence) &key (start integer))
;;;    ...options...)
"
  (safe-define 'defgeneric fname generic-arglist options))


(defmacro defvar* (var value &optional docstring)
  "* Arguments:
- VAR can be either:
-- 1. A variable name: in this case DEFVAR* has exactly the same effect as
DEFVAR.
-- 2. (VARNAME TYPE) where VARNAME is a variable name and TYPE is a type
declaration.
- VALUE: A form which is evaluated when the variable is first created.
- DOCSTRING: Documentation.

* Returns: The name of the variable as a symbol.

* Description:
Creates the global special variable VAR, initialises it to VALUE,
and declares it to be of type TYPE, if given.

* Examples:
;;; (defvar* (*file-name* string) \"~/log.txt\") "
  `(defvar/param defvar ,var ,value ,docstring))


(defmacro defparameter* (var value &optional docstring)
  "Like {defmacro defstar:defvar*}, but equivalent to DEFPARAMETER rather than DEFVAR.
See {defmacro defstar:defvar*} for usage."
  `(defvar/param defparameter ,var ,value ,docstring))


(defmacro flet* (clauses &body body)
  "* Description:
Like FLET, but within each clause the function name, arglist and body have the
same syntax as for {defmacro defstar:defun*}."
  `(flet ,(mapcar
           #'(lambda (clause)
               (destructuring-bind (fname arglist &rest clause-body) clause
                 (safe-define 'flet fname arglist clause-body)))
           clauses)
     ,@body))


(defmacro labels* (clauses &body body)
  "* Description:
Like LABELS, but within each clause the function name, arglist and body have the
same syntax as for {defmacro defstar:defun*}."
  `(labels ,(mapcar
             #'(lambda (clause)
                 (destructuring-bind (fname arglist &rest clause-body) clause
                   (safe-define 'labels fname arglist clause-body)))
             clauses)
     ,@body))



(defmacro lambda* (arglist &body body)
  "* Description:

Like LAMBDA, but the arglist and body have the same syntax as for {defmacro
defstar:defun*}.  A `returns' form can be used within the function body to
declare its return type."
  (safe-define 'lambda nil arglist body))

;;;; End of DEFSTAR
