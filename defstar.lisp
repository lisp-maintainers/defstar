;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; coding: utf-8-unix -*- ;;;;;;;;80
;;;;
;;;;    This file is part of DEFSTAR, by Paul Sexton
;;;;    Released under the Gnu Public License version 3
;;;;
;;;;    DEFSTAR is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    DEFSTAR is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with DEFSTAR.  If not, see <http://www.gnu.org/licenses/>.
;;;;

(in-package :cl-user)

;;; Note: documentation generated with CLOD
;;; [[http://bitbucket.org/eeeickythump/clod/]]
;;; (clod:document-package :defstar "doc/defstar.org"
;;;                        :internal-symbols? nil :author "Paul Sexton"
;;;                        :email "eeeickythump@gmail.com")

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
           #:*let
           #:nlet
           ;;#:returns
           #:result
           #:*check-argument-types-explicitly?*
           #:->)
  (:documentation
   "* Description

DEFSTAR is a collection of macros that can be used in place of =DEFUN,
DEFMETHOD, DEFGENERIC, DEFVAR, DEFPARAMETER, FLET, =LABELS=, =LET*= and =LAMBDA=.
Each macro has the same name as the form it replaces, with a star added at the
end, e.g. =DEFUN*=. (The exception is the =LET*= replacement, which is called
=*LET=).

The macros allow:
- easy inclusion of type declarations within lambda lists
- easy inclusion of return type declarations in function and method definitions
- easy declaration of variables as 'ignored', by the use of '_' as a placeholder
  in argument lists.
- easy inclusion of assertions for each argument and for the function's
  return value, thus allowing simple programming by contract.

See [[defun*]] and [[defvar*]] for a detailed description of syntax. See also
the examples below.

DEFSTAR's home is at:
- [[http://bitbucket.org/eeeickythump/defstar/]]

Installation requires [[http://common-lisp.net/project/asdf/][ASDF]]. DEFSTAR
does not depend on any other libraries.

* Examples of DEFUN* and DEFMETHOD* usage
;;; ;; Define a simple function that adds two numbers, both of which
;;; ;; are declared to be real.
;;; (defun* sum ((a real) (b real))
;;;    (+ a b))
;;;
;;; ;; Now also declare that the function returns a real.
;;; (defun* (sum -> real) ((a real) (b real))
;;;    (+ a b))
;;;
;;; ;; Another way of declaring the function's return type.
;;; (defun* sum ((a real) (b real))
;;;    (:returns real)
;;;    (+ a b))
;;;
;;; ;; We want to ensure that a and b are never negative.
;;; ;; One way is to alter the type declarations:
;;; (defun* (sum -> (real 0)) ((a (real 0)) (b (real 0)))
;;;    (+ a b))
;;;
;;; ;; Another way is to define a new type:
;;; (deftype natural () '(real 0))
;;; (defun* (sum -> natural) ((a natural) (b natural))
;;;    (+ a b))
;;;
;;; ;; Another way is to use assertions:
;;; (defun* (sum -> real (>= result 0)) ((a real (>= a 0)) (b real (>= b 0)))
;;;    (+ a b))
;;;
;;; ;; Or:
;;; (defun* sum ((a real (>= a 0)) (b real (>= b 0)))
;;;    (:returns real (>= result 0))
;;;    (+ a b))
;;;
;;; ;; Or, using the feature that the names of single-argument predicate
;;; ;; functions can be used as assertions:
;;; (defun* (naturalp -> boolean) ((x real))
;;;    (not (minusp x)))
;;; ...
;;; (defun* (sum -> real naturalp) ((a real naturalp) (b real naturalp))
;;;    (+ a b))
;;;
;;; ;; Another approach is to use :pre and :post clauses. Each contains one
;;; ;; more forms, ALL of which must evaluate to non-nil. Within :post
;;; ;; forms, result is bound to the value that the function or form
;;; ;; is about to return.
;;; (defun* (sum -> real) ((a real) (b real))
;;;    (:pre (>= a 0) (>= b 0))
;;;    (:post (>= result 0))
;;;    (+ a b))
;;;
;;; ;; A function that returns multiple values.
;;; (defun* (floor -> (values integer integer)) ((n real) (d real))
;;;    (cl:floor n d))
;;;
;;; ;; Example of ignoring arguments
;;; (*let (((top . _) list))
;;;    (print top))
;;;
;;; ;; It is possible to use assertions with functions that return
;;; ;; multiple values. When a function is declared to return multiple
;;; ;; values, RESULT will be bound to a LIST of those values.
;;; (defun* floor ((n real) (d real))
;;;    (:returns (values integer integer)
;;;             (< (second result) (first result)))
;;;    (cl:floor n d))
;;;
;;; ;; To declare that a function returns an unspecified number of
;;; ;; values, of unspecified types:
;;; (defun* (floor -> (values)) ((n real) (d real))
;;;    ...)
;;;
;;; ;; The type of a &REST argument can be declared. The declaration
;;; ;; refers to the types of each element in the list of arguments
;;; ;; stored in the &REST argument.
;;; (defun* (+ -> real) (&rest (numbers real))
;;;    (apply #'cl:+ numbers))
;;;
;;; ;; More complicated lambda list.
;;; ;; Note that the function and its first argument do not have type
;;; ;; declarations.
;;; ;; Also note the syntax of typed keyword arguments:
;;; ;; ((var TYPE [ASSERTION]) DEFAULT [SUPPLIEDP])
;;; ;; Note that &OPTIONAL arguments use the same syntax.
;;; (defun* my-find (item (seq sequence) &key (from-end boolean)
;;;                  ((test (or null (function (t)))) nil)
;;;                  ((test-not (or null (function (t)))) nil)
;;;                  ((key (or null (function (t)))) nil)
;;;                  (start fixnum) (end fixnum))
;;;    ...function body...)
;;;
;;; ;; Example of method definition. All the arguments in the arglist are
;;; ;; normal 'specialised' arguments like you would usually find in a
;;; ;; method definition. The form still allows you to include an assertion
;;; ;; with each argument, however ('plusp' in this case).
;;; (defmethod* (cell-value -> real) :around ((sheet <Sheet>)
;;;                                           (x integer plusp) (y integer plusp))
;;;    ...)
;;;
;;; ;; Note that when you declare a return type for a method, the method
;;; ;; body will perform type-checking, but no toplevel DECLAIM form will
;;; ;; be generated.
;;;
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
;;;
;;; ;; Example of DEFGENERIC*, mainly useful to declare the return type
;;; ;; of a set of methods. Note the documentation string can appear after
;;; ;; the argument list, similar to DEFUN.
;;; (defgeneric* (cell-value -> real) (sheet x y)
;;;   \"Return the value of the cell at coordinates X,Y in SHEET.\")
;;;
;;; ;; DEFGENERIC* can also be used to declare types of arguments. Be careful
;;; ;; that these don't clash with specialisers in method definitions.
;;; (defgeneric* (cell-value -> real) (sheet (x natural) (y natural)))

* Examples of DEFVAR* and DEFPARAMETER* usage

;;; (defvar* (*user-name* string) \"Bob\")
;;; (defparameter* (*file-position* (integer 0)) 0)

* Type DECLARATION versus type CHECKING

Technically, =DECLARE=, =DECLAIM= and the like do not actually check that
values stored in the associated variables conform to the declared type.
They merely constitute a promise /by the programmer/ that only values of
the specified type will be stored there. The consequences of storing
a string in a variable that is declared to be of type integer, are
technically 'undefined'.

In practice, most modern Common Lisp implementations perform type-checking
based on declaration information, especially when the =SAFETY= setting is high.

DEFSTAR allows you to force lisp to perform type checking based on
declarations. If you set the global variable
[[*check-argument-types-explicitly?*]] to non-nil, =CHECK-TYPE= forms will
included in the body of each function or method, causing an error to be raised
if a value does not match its declared type.

* Limitations

- Definitions of =SETF= methods cannot include return type declarations in the
  method 'header'. The return type can still be declared using a =(:RETURNS ...)=
  form. For example:
;;; (defmethod (setf (foo -> integer)) (...args...)   ; illegal
;;;    ...)
;;;
;;; (defmethod (setf foo) (...args...)
;;;    (:returns integer)                  ; legal
;;;    ...)

* Syntax highlighting of DEFSTAR macros in Emacs

Put the following code in your =.emacs= if you want =DEFVAR*= and other
forms to appear in the same face as their normal counterparts, and if
you want their docstrings to also be correctly identified as docstrings
rather than normal strings.

;;; ;; fontify doc strings in correct face
;;; ;; lisp-mode already fontifies 'defun*' correctly
;;; (put 'defvar*   'doc-string-elt 3)
;;; (put 'defparameter*   'doc-string-elt 3)
;;; (put 'lambda*   'doc-string-elt 2)
;;;
;;; (defvar *lisp-special-forms*
;;;       (regexp-opt '(\"defvar*\"
;;;                     \"defconstant*\"
;;;                     \"defparameter*\"
;;;                     \"defgeneric*\"
;;;                     \"defmethod*\"
;;;                     \"lambda*\"
;;;                     \"flet*\"
;;;                     \"labels*\") 'words))
;;; (font-lock-add-keywords 'lisp-mode
;;;  `((,*lisp-special-forms* . font-lock-keyword-face)))
"))

(in-package :defstar)


(declaim (optimize (speed 0) (safety 3) (debug 3)))


;;;; <<Utility functions>> ====================================================


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstar/split-defun-body (body &optional (force-docstring? nil))
    "* Arguments
- BODY :: the body of a =DEFUN= form or similar, such as might be received
  by a macro.
- FORCE-DOCSTRING? :: if true, and the body is a list whose only element is
  a string, that string will still be interpreted as a docstring rather
  than a constant return value for the body.

* Returns
Three values:
- PREAMBLE :: list of declaration forms at the start of the body
- DOCSTRING :: Documentation string, if present in =BODY=.
- TRUE-BODY :: Actual function body with the above items removed.

* Description
Internal utility function.
Divide the 'preamble' of a function body from its actual body.
The preamble consists of declarations and a docstring.
"
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
                   (or (cdr form) force-docstring?))
              (setf docstring (car form)))
             (t
              (setf true-body form)
              (return))))
      (values (reverse preamble) docstring true-body))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstar/make-keyword (&rest parts)
    "* Arguments
- PART :: Any lisp value; usually a string or symbol.

* Return Value
A keyword.

* Description
Concatenates the printed representations of =PARTs= together into a single
string, then makes a symbol from that string, and interns the symbol in the
=KEYWORD= package. Returns the new keyword.

* See Also
[[mksymbol]]"
    (intern (string-upcase (format nil "~{~A~}" parts)) :keyword)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstar/ampersand-symbol? (sym)
    "* Arguments
- SYM :: A symbol.
* Return Value
Boolean.
* Description
Predicate. Does the symbol =SYM= begin with an ampersand, such as =&ANY=,
=&REST= and so on?"
    (and (symbolp sym)
	 (eql #\& (char (format nil "~A" sym) 0)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstar/varnames-in-arglist (arglist)
    (loop for arg in arglist
       when (and (symbolp arg) (not (defstar/ampersand-symbol? arg)))
       collect arg
       when (listp arg)
       collect (car arg))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ignored-variable? (var)
    (and (symbolp var)
         (string= (string var) "_"))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-tree (fn tree)
    "Like MAPCAR but operates on a tree. FN must take one argument. It is called
for every atom in TREE; a structural copy of TREE is returned in which every atom
is replaced by the value of (FN ATOM)."
    (cond
      ((null tree) nil)
      ((and (consp tree)
            (null (car tree)))
       ;; Special case where node is actually NIL.
       (cons (funcall fn (car tree))
             (map-tree fn (cdr tree))))
      ((consp tree)
       (cons (map-tree fn (car tree))
	     (map-tree fn (cdr tree))))
      (t
       (funcall fn tree)))))



;;;; <<Internal functions and macros>> ========================================


(defconstant +DEFUN*-ARROW-SYMBOL+ '->
  "The symbol that separates function name from type declaration
in =DEFUN*= forms and the like. See [[defun*]].")

(defvar *check-argument-types-explicitly?* nil
  "If non-nil, insert =CHECK-TYPE= clauses in the preamble of functions,
to force the function's arguments to be explicitly type-checked.

Technically, =DECLARE, DECLAIM= and the like do not actually check that
values stored in the associated variables conform to the declared type.
They merely constitute a promise /by the programmer/ that only values of
the specified type will be stored there. The consequences of storing
a string in a variable that is declared to be of type integer, are
undefined.

In practise, essentially all modern lisps do perform type checking
based on declarations, especially when the =SAFETY= setting is high. ")

(defvar *use-contextl* nil
  "Bound to true if the ContextL package is currently loaded.")

(defun assert-precondition (fname clause varnames)
  (let* ((fname (or fname "anonymous function"))
         (msg (with-output-to-string (s)
                (format s "A call to ~A violated the precondition: ~A.~%"
                        fname clause)
                (dolist (var-name varnames)
                  (format s "~&~A = ~~S" var-name)))))
    `(assert ,clause ,varnames ,msg ,@varnames)))


(defun assert-postcondition (fname clause varnames)
  (let* ((fname (or fname "anonymous function"))
         (msg (with-output-to-string (s)
                (format s "When returning from ~A, postcondition violated: ~A.~%"
                        fname clause)
                (format s "Return value: ~~S~%")
                (dolist (var-name varnames)
                  (format s "~&~A = ~~S" var-name)))))
    `(assert ,clause (result) ,msg result ,@varnames)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defun*-term (term last-amp-kwd &key (def-type 'defun))
    "* Arguments
- TERM :: any member of an ordinary lambda list.
- LAST-AMP-KWD :: Symbol or nil.
- DEF-TYPE :: Symbol denoting the type of toplevel form that is being created.
The default is ='DEFUN=.
* Returns
Four values:
1. The term as it should be included in the final argument list for
   the toplevel form (symbol or list)
2. The declaration clause that should be included in the
   =DECLARE= statement within the toplevel form's body
3. The type of the term, for inclusion in the argument-list of
   the =(DECLAIM (FTYPE (FUNCTION arglist RETURN-TYPE) ...))= form for a
   function definition.
4. The assertion clause.

* Description
Internal function, used by [[defun*]] to parse lambda list terms.

* See Also
- [[defun*]]"
    (let ((layered-defmethod
            (if *use-contextl*
                (find-symbol "DEFINE-LAYERED-METHOD" :contextl)
                (gensym)))
          (layered-defgeneric
            (if *use-contextl*
                (find-symbol "DEFINE-LAYERED-FUNCTION" :contextl)
                (gensym))))
      (flet ((check-clause (check var)
               (if (and check (symbolp check))
                   (list check var)
                   check)))
        (cond
          ((null last-amp-kwd)
           (cond
             ((listp term)
              (cond
                ((or (eql def-type 'defmethod)
                     (eql def-type layered-defmethod))
                 (if (listp (car term))
                     (destructuring-bind ((var vartype &optional check) varclass)
                         term
                       (values (list var varclass)
                               (list 'type vartype var)
                               varclass
                               (check-clause check var)))
                     ;; else
                     (destructuring-bind (var varclass &optional check) term
                       (values (list var varclass)
                               nil
                               varclass
                               (check-clause check var)))))
                ((or (eql def-type 'defgeneric)
                     (eql def-type layered-defgeneric))
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
                   (or (eql def-type 'defgeneric)
                       (eql def-type layered-defgeneric)))
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
                  last-amp-kwd)))))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun safe-define (toplevel-form-name fname arglist body
                      &key generic-function-name)
    "* Arguments
- TOPLEVEL-FORM-NAME :: Symbol denoting the type of toplevel form being defined.
Currently handles ='DEFUN, 'DEFMETHOD, 'FLET, 'LABELS, 'LAMBDA, 'DEFGENERIC=.
- FNAME, ARGLIST, BODY :: see [[defun*]].
- GENERIC-FUNCTION-NAME :: supplied for methods defined via a `:method'
clause inside a `defgeneric*' form. A symbol naming the generic function to
which the methods belong.

* Returns
A =defun, defmethod, defgeneric= or =lambda= form, or =flet= or
=labels= subclause, containing appropriate declarations.

* Description
Internal function. The workhorse for the macros [[DEFUN*]], [[DEFMETHOD*]],
[[LAMBDA*]], [[FLET*]], and [[LABELS*]].
"
    (let* ((*use-contextl* (find :contextl *features*))
           (layered-defgeneric
             (if *use-contextl*
                 (find-symbol "DEFINE-LAYERED-FUNCTION" :contextl)
                 (gensym)))
           (layered-defmethod
             (if *use-contextl*
                 (find-symbol "DEFINE-LAYERED-METHOD" :contextl)
                 (gensym)))
           (defgeneric? (or (eql 'defgeneric toplevel-form-name)
                            (eql layered-defgeneric toplevel-form-name)))
           (method-combo-keywords nil)
           (layer nil)
           (form-args nil)
           (ftype-args nil)
           (declarations nil)
           (checks nil)
           (returns-clause (find :returns body
                                 :key #'(lambda (x)
                                          (if (listp x) (car x) nil))))
           (pre-clause (find :pre body
                             :key #'(lambda (x)
                                      (if (listp x) (car x) nil))))
           (post-clause (find :post body
                              :key #'(lambda (x)
                                       (if (listp x) (car x) nil))))
           (returns-type t)
           (returns-check nil)
           (final-form nil)
           (amp nil)
           (name-for-block (let ((basename (or generic-function-name fname)))
                             (cond
                               ((symbolp basename)
                                 basename)
                               ((listp basename) ; (setf foo)
                                 (second basename))
                               (t
                                (error "Don't know how to get block name from function name ~S" basename))))))

      ;; Extract method qualifiers (eg :AFTER, :AROUND etc)
      (when (and (eql 'defmethod toplevel-form-name)
                 (not (listp arglist)))
        (push arglist method-combo-keywords)
        (loop for term in body
              while (not (listp term))
              do (push term method-combo-keywords)
                 (pop body))
        (setf method-combo-keywords (reverse method-combo-keywords))
        (when (and *use-contextl*
                   (member (car method-combo-keywords) '(:in :in-layer)))
          (setf layer (second method-combo-keywords))
          (setf toplevel-form-name layered-defmethod)
          (setf method-combo-keywords (cddr method-combo-keywords)))
        (setf arglist (car body))
        (setf body (cdr body)))

      ;; Extract (:layered ...) clause from defgeneric* options
      (when (and *use-contextl*
                 (eql 'defgeneric toplevel-form-name)
                 (assoc :layered (remove-if-not #'listp body)))
        (let ((clause (assoc :layered (remove-if-not #'listp body))))
          (when (or (null (cdr clause))
                    (second clause))
            (setf toplevel-form-name layered-defgeneric)
            (setf body (remove clause body :test #'equal)))))

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
             (when (ignored-variable? form-term)
               (let ((new-term (gensym "IGNORED-")))
                 (if decl (setf decl (subst new-term form-term decl)))
                 (setf form-term new-term)
                 (push (list 'ignore form-term) declarations)))
             (push form-term form-args)
             (if decl (push decl declarations))
             (if ftype-term (push ftype-term ftype-args))
             (if check (push check checks))))))

      (when returns-clause
        (destructuring-bind (rtype &optional rcheck) (cdr returns-clause)
          ;;(break)
          (setf returns-type rtype)
          (if rcheck (setf returns-check rcheck))
          (if (and rcheck (symbolp rcheck))
              (setf returns-check `(,rcheck result)))
          (if returns-check
              (setf returns-check (list returns-check))))
        (setf body (remove returns-clause body :test #'eql)))

      (when post-clause
        (setf returns-check (cdr post-clause))
        (setf body (remove post-clause body :test #'eql)))

      (when pre-clause
        (setf body (remove pre-clause body :test #'eql)))

      (when (and fname (listp fname)
                 (not (eql 'setf (car fname))))
        (when returns-clause
          (error "DEFUN* ~A ... also contains ':RETURNS' clause in body"
                 fname))
        (destructuring-bind (fun-name arrow rtype &optional rcheck) fname
          (unless (eql arrow +DEFUN*-ARROW-SYMBOL+)
            (error "Malformed DEFUN* header: found ~S, expected arrow symbol (~S)"
                   arrow +DEFUN*-ARROW-SYMBOL+))
          (setf fname fun-name)
          (setf returns-type rtype)
          (if rcheck (setf returns-check rcheck))
          (if (and rcheck (symbolp rcheck))
              (setf returns-check `(,rcheck result)))))
      (multiple-value-bind (preamble docstring true-body)
          (defstar/split-defun-body body defgeneric?)
        (when (and defgeneric?
                   (find-if #'stringp true-body))
          (setf docstring (format nil "~{~&~A~}"
                                  (remove-if-not #'stringp true-body)))
          (setf true-body (remove-if #'stringp true-body)))

        (setf preamble
              `(,@(if declarations `((declare ,@declarations)) nil)
                ,@preamble))
        (if *check-argument-types-explicitly?*
            (setf preamble
                  (append
                   preamble
                   (remove nil
                           (mapcar #'(lambda (decl)
                                       (unless (eql (car decl) 'ignore)
                                         `(check-type ,(third decl) ,(second decl))))
                                   declarations)))))
        (setf true-body
              `(,@(if (and checks (not defgeneric?))
                      (mapcar #'(lambda (check)
                                  (assert-precondition
                                   fname check
                                   (defstar/varnames-in-arglist form-args)))
                              checks)
                      nil)
                ,@true-body))
        (setf form-args (reverse form-args)
              ftype-args (reverse ftype-args)
              checks (reverse checks))
        (if (and returns-check (symbolp returns-check))
            (setf returns-check `(,returns-check result)))
        (setf final-form
              `(,@(if (eql :method fname) nil `(,toplevel-form-name))
                ,@(if fname (list fname) nil)
                ,@(if layer `(:in-layer ,layer) nil)
                ,@method-combo-keywords
                ,form-args
                ,@(cond
                    ((and docstring defgeneric?)
                     `((:documentation ,docstring)))
                    (docstring
                     (list docstring))
                    (t nil))
                ,@(if defgeneric? nil preamble)
                ,@(if pre-clause
                      (mapcar (lambda (check)
                                (assert-precondition
                                 fname check
                                 (defstar/varnames-in-arglist
                                     form-args)))
                              (cdr pre-clause)))
                ,@(cond
                    (defgeneric?
                        true-body)
                    ((and returns-check
                          (listp returns-type)
                          (eq 'values (car returns-type)))
                     `((the ,returns-type
                            (let ((result
                                    (multiple-value-list
                                     (block ,name-for-block
                                       ,@true-body))))
                              ,@(mapcar
                                 (lambda (check)
                                   (assert-postcondition
                                    (or generic-function-name fname) check
                                    (defstar/varnames-in-arglist
                                        form-args)))
                                 returns-check)
                              (values-list result)))))
                    (returns-check
                     (format t "returns-check: ~S~%" returns-check)
                     `((the ,returns-type
                            (let ((result
                                    (block ,name-for-block
                                      ,@true-body)))
                              ,@(mapcar
                                 (lambda (check)
                                   (assert-postcondition
                                    (or generic-function-name fname) check
                                    (defstar/varnames-in-arglist
                                        form-args)))
                                 returns-check)
                              result))))
                    (returns-type
                     `((the ,returns-type
                            ,(if (cdr true-body)
                                 `(block ,name-for-block
                                    ,@true-body)
                                 (car true-body)))))
                    (t
                     true-body))))
        (cond
          ((and (or declarations returns-type)
                (not (eql :method (car final-form)))
                (not (member toplevel-form-name `(defmethod
                                                     defgeneric
                                                     ,layered-defgeneric
                                                   ,layered-defmethod
                                                   flet labels
                                                   lambda))))
           `(progn
              (declaim (ftype (function ,ftype-args ,returns-type) ,fname))
              ,final-form))
          ((member toplevel-form-name '(flet labels))
           (cdr final-form))
          (t
           final-form))))))


(defmacro defvar/param (toplevel-form-name var value &optional docstring)
  "* Arguments
- TOPLEVEL-FORM-NAME :: Symbol denoting the type of toplevel form being defined.
  For example, ='DEFUN=.
- VAR :: Symbol or list.
- VALUE :: Form that will be evaluated to initialise the variable being
  defined.
- DOCSTRING :: String used as documentation.
* Return Value
A symbol.
* Description
Internal macro, used by [[defvar*]] and
[[defparameter*]]."
  (cond
    ((listp var)
     (destructuring-bind (varname vartype) var
       `(progn
          (declaim (type ,vartype ,varname))
          (,toplevel-form-name ,varname ,value
                               ,@(if docstring (list docstring) nil)))))
    (t
     `(,toplevel-form-name ,var ,value
        ,@(if docstring (list docstring) nil)))))




;;;; <<Exported macros>> ======================================================


;;; <<defun*>>
(defmacro defun* (fname arglist &body body)
  "* Arguments
- FNAME :: either the name of the function to be created, or a list with the
  following grammar:
  : fname =   FUNCTION-NAME
  :         | (FUNCTION-NAME -> TYPE [assertion])
  : assertion =       FORM
  :                 | PREDICATE-SYMBOL
  Where:
  - =TYPE= is any valid type specifier
  - =FORM= is any form, which must return non-nil if the assertion is satisfied,
    nil otherwise. Within the form, the symbol =RESULT= is bound to the
    value that is about to be returned by the function.
  - =PREDICATE-SYMBOL= is a symbol, the name of a function that accepts a single
    argument. Equivalent to the form =(PREDICATE-SYMBOL RESULT)=.

    /Note:/ if the latter (list) form for fname is used, the =DEFUN*= body may
    /not/ also contain a =:returns= form. Also note that the latter form cannot
    currently be used when defining a =(setf ...)= function or method.
- ARGLIST :: a =DEFUN*= LAMBDA LIST, which uses the following grammar:
  : arglist =   var-term*
  :           | (var-term* [&optional opt-term+])
  :           | (var-term* [&key opt-term+])
  :           | (var-term* [&rest rest-term])
  : var-term =        VARNAME
  :                 | (VARNAME TYPE/CLASS [assertion])
  : rest-term =       VARNAME
  :                 | (VARNAME ELEMENT-TYPE)
  : opt-term =        VARNAME
  :                 | (var-term DEFAULT [SUPPLIEDP])
  Where:
  - =VARNAME= is a symbol that will name the variable bound to the function
    argument.
  - =TYPE/CLASS= and =ELEMENT-TYPE= are forms that are legal type
    declarations. For example, the name of a simple type or class, or a list if
    the type declaration is more complex.
  - =DEFAULT= and =SUPPLIED-P= are the default value, and a variable that will
    indicate whether the argument was supplied.
- BODY :: Body of the function form. This may contain a docstring in the usual
  place, and may also contain:
  - a single special form beginning with =:returns=:
    : returns-form = (:RETURNS TYPE [assertion])
    If the =:returns= form contains an assertion, then within that assertion,
    the symbol =RESULT= is bound to the value that the function is
    about to return.
  - a single special form beginning with =:pre= followed by one or more
    expressions, which will be evaluated before any other code in the body.
    All of the expressions must evaluate to non-nil, or an error is signalled.
    : pre-form = (:PRE [assertion] [assertion]*)
  - a single special form beginning with =:post= followed by one or more
    expressions, which will be evaluated just prior to the function returning.
    All of the expressions must evaluate to non-nil, or an error is signalled.
    Within the :post clause, =result= is bound to the return value of the
    function.
    : post-form = (:POST [assertion] [assertion]*)

* Description
Equivalent to =(DEFUN fname arglist . body)=, but:
- All type declarations within the lambda list will be turned into =(DECLARE...)=
  forms within the function body
- If a return type is declared for the function itself, this will be turned
  into a global =DECLAIM= form that immediately precedes the function.
- Any variables whose names are '_' are renamed with unique symbols
  and declared 'ignored' within the function body. This provides a quick way
  to ignore arguments or parts of arguments.
- All assertions within the lambda list or =:pre= form will be checked before
  the function body is entered.
- Any assertions within a =:returns= form or =:post= form will be checked
  before the function returns a value.

* Examples
;;; ;; Very simple example
;;; (defun* (add -> real) ((a real) (b real))
;;;   (+ a b))

;;; ;; Example with assertion for 'b' argument, checked before the
;;; ;; body of the function is entered.
;;; (defun* div ((a real) (b real (/= b 0)))
;;;    (:returns real)
;;;    (/ a b))

;;; ;; Similar to above example but using :pre clause.
;;; (defun* div ((a real) (b real))
;;;    (:returns real)
;;;    (:pre (/= b 0))
;;;    (/ a b))

;;; (defun* sum (&rest (nums real))  ; type of 'rest' var refers to
;;;    (:returns real)                ; the type of each list element, ie
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


;;; <<defmethod*>>
(defmacro defmethod* (fname method-arglist &body body)
  "* Arguments

Usage is exactly the same as [[defun*]], except that within =METHOD-ARGLIST=,
any list in a non-optional position (prior to any =&key, &rest,= or =&optional=
keyword) is assumed to be a specialised lambda list term of the form =(VARNAME
CLASS [assertion])=, rather than a DEFUN* type-declaring term.

The syntax of METHOD-ARGLIST is therefore:
: arglist-and-qualifiers =   [qualifier]* method-arglist
: qualifier =  :in-layer LAYER
:            | :in LAYER
:            | :around
:            | :before
:            | :after  (etc)
: method-arglist = method-term*
:           | (method-term* [&optional opt-term+])
:           | (method-term* [&key opt-term+])
:           | (method-term* [&rest rest-term])
: method-term = VARNAME
:               | (VARNAME CLASS [assertion])
:               | ((VARNAME TYPE/CLASS [assertion]) CLASS)
The rest of the syntax is the same as for DEFUN*.

If the :in or :in-layer qualifier is present (they are synonymous), this
form will generate a ContextL `define-layered-method' form rather than a
`defmethod'.

* Description
Equivalent to =(DEFMETHOD FNAME METHOD-ARGLIST . body)= with type declarations
and assertions as per [[defun*]].

* Examples
;;; (deftype positive-integer () `(integer 1))
;;;
;;; (defmethod (make-coords -> (cons positive-integer positive-integer))
;;;                (((x positive-integer) integer)
;;;                 ((y positive-integer) integer))
;;;    (cons x y))
"
  (safe-define 'defmethod fname method-arglist body))


;;; <<defgeneric*>>
(defmacro defgeneric* (fname generic-arglist &body options)
  "* Arguments
- FNAME :: Name of the generic function. Handles names of the form (SETF X)
  correctly.
- GENERIC-ARGLIST :: Follows the same grammar the arglist for [[defun*]]
  forms, except that =&REST, &KEY= and =&OPTIONAL= arguments must be of
  the form:
  : arg =   VARNAME
  :       | (VARNAME TYPE)
- OPTIONS :: Options to DEFGENERIC. Any of these may be simple strings,
  which will be concatenated together and the resulting string treated as
  equivalent to =(:documentation STRING)=.
  One extra option is allowed -- (:layered BOOL). If this is present and BOOL
  is a non-nil constant, the form will generate a ContextL
  `define-layered-function' rather than `defgeneric'.

* Description
Usage is exactly the same as [[defun*]], except that value-checking assertions
are ignored.

If you define any methods inside the form using `:method' clauses, they can
use [[defmethod*]]-style argument lists, :pre and :post clauses, and so on.

Note that you can declare types for arguments in the generic function
argument list. Be careful that these do not clash with method definitions.
Type declarations for generic function arguments will only be used to
make a toplevel =DECLAIM= form that will then apply to all methods of
the generic function.

* Examples:
;;; (defgeneric* (length -> integer) (seq &key start)
;;;    \"Return the length of the sequence SEQ.\"
;;;    ...options...)
;;;
;;; (defgeneric* (length -> integer) ((seq sequence) &key (start integer))
;;;    ...options...)
"
  (safe-define 'defgeneric fname
               generic-arglist
               (mapcar (lambda (option)
                         (if (and (listp option)
                                  (eql :method (first option)))
                             (safe-define 'defmethod :method
                                          (second option) (cddr option)
                                          :generic-function-name
                                          (if (listp fname)
                                              (car fname) fname))
                             ;; else
                             option))
                       options)))


;;; <<defvar*>>
(defmacro defvar* (var value &optional docstring)
  "* Arguments
- VAR :: either:
  1. A variable name: in this case =DEFVAR*= has exactly the same effect as
     =DEFVAR=.
  2. =(VARNAME TYPE)= where =VARNAME= is a variable name and =TYPE= is a type
     declaration.
- VALUE :: A form which is evaluated when the variable is first created.
- DOCSTRING :: Documentation string.

* Returns
The name of the variable as a symbol.

* Description
Creates the global special variable =VAR=, initialises it to =VALUE=,
and declares it to be of type =TYPE=, if given.

* Examples
;;; (defvar* (*file-name* string) \"~/log.txt\") "
  `(defvar/param defvar ,var ,value ,docstring))


;;; <<defparameter*>>
(defmacro defparameter* (var value &optional docstring)
  "* Description
Like [[defvar*]], but expands to =DEFPARAMETER= rather than =DEFVAR=.
See [[defvar*]] for more details."
  `(defvar/param defparameter ,var ,value ,docstring))


;;; <<flet*>>
(defmacro flet* (clauses &body body)
  "* Arguments
- CLAUSES :: List of clauses. Takes the following grammar:
  : clauses = clause*
  : clause  = (FNAME ARGLIST ...body...)
  See [[defun*]] for a description of the grammar of =FNAME= and =ARGLIST=.
- BODY :: Body of the form.

* Description
Like =FLET=, but within each function-definition clause the function name,
arglist and body have the same syntax as for [[defun*]].

* Examples
;;; (defun foo (name x y)
;;;    (flet* (((area -> integer) ((x integer) (y integer))
;;;              (* x y)))
;;;       (format t \"Area of ~A is ~D.~%\" name (area x y))))
"
  `(flet ,(mapcar
           #'(lambda (clause)
               (destructuring-bind (fname arglist &rest clause-body) clause
                 (safe-define 'flet fname arglist clause-body)))
           clauses)
     ,@body))


;;; <<labels*>>
(defmacro labels* (clauses &body body)
  "* Arguments
See [[flet*]].

* Description
Like =LABELS=, but within each clause the function name, arglist and body have
the same syntax as for [[defun*]].

See [[flet*]] for more details."
  `(labels ,(mapcar
             #'(lambda (clause)
                 (destructuring-bind (fname arglist &rest clause-body) clause
                   (safe-define 'labels fname arglist clause-body)))
             clauses)
     ,@body))


;;; <<lambda*>>
(defmacro lambda* (arglist &body body)
  "* Description
Like =LAMBDA=, but =ARGLIST= and body have the same syntax as for [[defun*]].
 A =:returns= form can be used within the function body to
declare its return type."
  (safe-define 'lambda nil arglist body))



(defun named-*let-aux (name clauses body)
  (let ((dec nil)
        (wrappers nil)
        (arglist nil)
        (vals nil))
    (dolist (clause clauses)
      (cond
        ((atom clause)
         (push clause arglist)
         (push nil vals))
        ((and (listp (car clause))
              (eql :values (caar clause)))
         ;; (:values a b) -- bind multiple values =============================
         (let* ((arg (gensym "MVARG"))
                (decls nil)
                (vars (mapcar
                       (lambda (var)
                         (cond
                           ((listp var)
                            (push `(,(second var) ,(first var)) decls)
                            (first var))
                           ((ignored-variable? var)
                            (let ((new-term (gensym "IGNORED-")))
                              (push `(ignore ,new-term) decls)
                              new-term))
                           (t var)))
                       (cdr (car clause)))))
           (push arg arglist)
           (push `(destructuring-bind ,vars ,arg
                    ,@(if decls `((declare ,@decls))))
                 wrappers)
           (push `(multiple-value-list ,(second clause)) vals)))
        ((listp (first clause))
         ;; destructuring =====================================================
         (let* ((arg (gensym "DARG"))
                (decls nil)
                (lhs
                  (map-tree (lambda (term) (cond
                                        ((ignored-variable? term)
                                         (let ((new-term (gensym "IGNORED-")))
                                           (push `(ignore ,new-term) decls)
                                           new-term))
                                        (t term)))
                            (first clause))))
           (push arg arglist)
           (push `(destructuring-bind ,lhs ,arg
                    ,@(if decls `((declare ,@decls))))
                 wrappers)
           (push (second clause) vals)))
        ((= 3 (length clause))
         ;; variable with type ================================================
         (push (first clause) arglist)
         (push `(,(nth 1 clause) ,(nth 0 clause)) dec)
         (push (nth (1- (length clause)) clause) vals))
        (t
         (push (first clause) arglist)
         (push (second clause) vals))))
    (if dec
        (push `(declare ,@dec) body))
    (dolist (wrapper wrappers)
      (setf body (list (append wrapper body))))
    (cond
      (name
       `(labels ((,name ,(reverse arglist)
                   ,@body))
          (,name ,@(reverse vals))))
      (t
       ;; no name -- just make a let*
       `(let* ,(reverse (mapcar #'list arglist vals))
          ,@body)))))



;;; <<*let>>
(defmacro *let ((&rest clauses) &body body)
  "* Arguments
- CLAUSES :: A series of zero or more clauses taking the form:
: clause =   VARNAME
:          | (VARNAME FORM)
:          | (LAMBDA-LIST FORM)
:          | ((:values VAR...) FORM)
:          | (VARNAME TYPE FORM)
- BODY :: The body of the form (implicit =progn=).
* Description
Behaves like LET*, but:
- When types are given between the variable name and expression, these
  are converted to declarations within the scope of the LET form.
- When the form to be bound is a list or cons cell, behaves like
  DESTRUCTURING-BIND.
- When the form to be bound is a list whose first element is :values,
  behaves like MULTIPLE-VALUE-BIND, using the rest of the elements in
  the form as the variables to be bound. Those elements may be symbols,
  or lists of the form (SYMBOL TYPE), in which case the bound symbol will
  be declared to be of the given type.
- Any variables whose names are '_', either bare or inside a form to be
  destructured, will be renamed with unique symbols and declared 'ignored'
  within the body. This provides a quick way to ignore arguments or parts
  of arguments.
* Example
;;; (*let ((name \"Bob\")
;;;        (age integer 40)
;;;        (sex (member :male :female) :male)
;;;        ((num street &optional suburb) address)
;;;        ((:values (day fixnum) month year) birthday))
;;;    ...body...)
Expands to:
;;; (let ((name \"Bob\"))
;;;   (let ((age 40))
;;;     (declare (integer age))
;;;     (let ((sex :male))
;;;       (declare ((member :male :female) sex))
;;;       (destructuring-bind
;;;           (num street &optional suburb) address
;;;         (multiple-value-bind (day month year) birthday
;;;           (declare (fixnum day))
;;;           ...body...)))))
"
  (named-*let-aux nil clauses body))


(defmacro nlet (name (&rest bindings) &body body)
  "Identical to *LET, but recursion can occur by calling (NAME ARGS...)
within BODY, where each argument in ARGS matches one binding.
The same as Scheme's named LET. Note that the macro does NOT perform tail
call optimisation. (All modern lisp compilers will perform TCO of the generated
code however.)
* Example
;;; (nlet fact ((n 5))
;;;    (if (= 1 n)
;;;       n
;;;       (* n (fact (1- n)))))"
  (named-*let-aux name bindings body))



;;;; defstar.lisp ends here ===================================================
