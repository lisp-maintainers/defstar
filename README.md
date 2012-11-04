# The DEFSTAR library


Defstar is a collection of Common Lisp macros that can be used in place of
`defun`, `defmethod`, `defgeneric`, `defvar`, `defparameter`, `flet`, `labels`,
`let*` and `lambda`. Each macro has the same name as the form it replaces, with
a star added at the end, e.g. `defun*`. (the exception is the `let*`
replacement, which is called `*let`).

The macros allow:
- easy inclusion of type declarations within lambda lists
- easy inclusion of return type declarations in function and method definitions
- easy declaration of variables as 'ignored', by the use of '_' as a
  placeholder variable in argument lists.
- easy inclusion of assertions for each argument and for the function's
  return value, thus allowing simple programming by contract.

See the [full documentation](doc/defstar.html) for a detailed description of
syntax. See also the examples below.

Defstar's home is at http://bitbucket.org/eeeickythump/defstar/.

Installation requires [ASDF][] or [Quicklisp][]. Defstar does not depend on
any other libraries.


[ASDF]: http://common-lisp.net/project/asdf/
[Quicklisp]: http://quicklisp.org


## Example usage


### defun*

Define a simple function that adds two numbers, both of which
are declared to be real.

    :::cl
    (defun* sum ((a real) (b real))
       (+ a b))

Now also declare that the function returns a real.

    :::cl
    (defun* (sum -> real) ((a real) (b real))
       (+ a b))

Another way of declaring the function's return type.

    :::cl
    (defun* sum ((a real) (b real))
       (:returns real)
       (+ a b))

We want to ensure that a and b are never negative.
One way is to alter the type declarations:

    :::cl
    (defun* (sum -> (real 0)) ((a (real 0)) (b (real 0)))
       (+ a b))

Another way is to define a new type:

    :::cl
    (deftype natural () '(real 0))
    (defun* (sum -> natural) ((a natural) (b natural))
     (+ a b))

Another way is to use assertions:

    :::cl
    (defun* (sum -> real (>= result 0)) ((a real (>= a 0)) (b real (>= b 0)))
       (+ a b))

    ;; Or:
    :::cl
    (defun* sum ((a real (>= a 0)) (b real (>= b 0)))
       (:returns real (>= result 0))
       (+ a b))

Or, using the feature that the names of single-argument predicate
functions can be used as assertions:

    :::cl
    (defun* (naturalp -> boolean) ((x real))
       (not (minusp x)))
    ...
    (defun* (sum -> real naturalp) ((a real naturalp) (b real naturalp))
       (+ a b))

Another approach is to use :pre and :post clauses. Each contains one
more forms, ALL of which must evaluate to non-nil. Within :post
forms, result is bound to the value that the function or form
is about to return.

    :::cl
    (defun* (sum -> real) ((a real) (b real))
       (:pre (>= a 0) (>= b 0))
       (:post (>= result 0))
       (+ a b))

A function that returns multiple values.

    :::cl
    (defun* (floor -> (values integer integer)) ((n real) (d real))
       (cl:floor n d))

### *let

Example of ignoring arguments

    :::cl
    (*let (((top . _) list))
       (print top))

It is possible to use assertions with functions that return
multiple values. When a function is declared to return multiple
values, RESULT will be bound to a LIST of those values.

    :::cl
    (defun* floor ((n real) (d real))
       (:returns (values integer integer)
                (< (second result) (first result)))
       (cl:floor n d))

To declare that a function returns an unspecified number of
values, of unspecified types:

    :::cl
    (defun* (floor -> (values)) ((n real) (d real))
       ...)

The type of a `&rest` argument can be declared. The declaration
refers to the types of each element in the list of arguments
stored in the `&rest` argument.

    :::cl
    (defun* (+ -> real) (&rest (numbers real))
       (apply #'cl:+ numbers))

A more complicated lambda list. Note that the function and its first argument
do not have type declarations. Also note the syntax of typed keyword arguments:

    ((var TYPE [ASSERTION]) DEFAULT [SUPPLIEDP])

Note that `&optional` arguments use the same syntax.

    :::cl
    (defun* my-find (item (seq sequence) &key (from-end boolean)
                     ((test (or null (function (t)))) nil)
                     ((test-not (or null (function (t)))) nil)
                     ((key (or null (function (t)))) nil)
                     (start fixnum) (end fixnum))
       ...function body...)


### defmethod* and defgeneric*


Example of method definition. All the arguments in the arglist are
normal 'specialised' arguments like you would usually find in a
method definition. The form still allows you to include an assertion
with each argument, however (`plusp` in this case).

    :::cl
    (defmethod* (cell-value -> real) :around ((sheet <Sheet>)
                                              (x integer plusp) (y integer plusp))
        ...)

Note that when you declare a return type for a method, the method
body will perform type-checking, but no toplevel `declaim` form will
be generated.

CLOS function dispatch based on classes is limited; you cannot specialise
on user-defined types unless they are proper classes, for example.
You may therefore sometimes want to declare that a method's argument
is of a particular type, as well as declaring its class for specialisation
as you normally would.

Here is an example. Note the similarity to the syntax for keyword
arguments.

    :::cl
    (defmethod* (cell-value -> real) :around ((sheet <Sheet>)
                                              ((x natural plusp) integer)
                                              ((y natural plusp) integer))
       ...)

An example of `defgeneric*`, mainly useful to declare the return type
of a set of methods. Note the documentation string can appear after
the argument list, similar to `defun`.

    :::cl
    (defgeneric* (cell-value -> real) (sheet x y)
      "Return the value of the cell at coordinates X,Y in SHEET.")

`defgeneric*` can also be used to declare types of arguments. Be careful
that these don't clash with specialisers in method definitions.

    :::cl
    (defgeneric* (cell-value -> real) (sheet (x natural) (y natural)))


### defvar*


    :::cl
    (defvar* (*user-name* string) "Bob")


### defparameter*


    :::cl
    (defparameter* (*file-position* (integer 0)) 0)


## Type declaration versus type checking


Technically, `declare`, `declaim` and the like do not actually check that
values stored in the associated variables conform to the declared type.
They merely constitute a promise /by the programmer/ that only values of
the specified type will be stored there. The consequences of storing
a string in a variable that is declared to be of type integer, are
technically 'undefined'.

In practice, most modern Common Lisp implementations perform type-checking
based on declaration information, especially when the `safety` setting is high.

Defstar allows you to force lisp to perform type checking based on
declarations. If you set the global variable
`*check-argument-types-explicitly?*` to non-nil, `check-type` forms will
included in the body of each function or method, causing an error to be raised
if a value does not match its declared type.


## Limitations


Definitions of `setf` methods cannot include return type declarations in the
method 'header'. The return type can still be declared using a `(:returns ...)`
form. For example:

    :::cl
    (defmethod (setf (foo -> integer)) (...args...)   ; illegal
       ...)

    (defmethod (setf foo) (...args...)
       (:returns integer)                  ; legal
       ...)


## Syntax highlighting of `defstar` macros in Emacs


Put the following code in your `.emacs` if you want `defvar*` and other
forms to appear in the same face as their normal counterparts, and if
you want their docstrings to also be correctly identified as docstrings
rather than normal strings.


    :::cl
    ;; fontify doc strings in correct face
    ;; lisp-mode already fontifies 'defun*' correctly
    (put 'defvar*   'doc-string-elt 3)
    (put 'defparameter*   'doc-string-elt 3)
    (put 'lambda*   'doc-string-elt 2)

    (defvar *lisp-special-forms*
    (regexp-opt '("defvar*"
                  "defconstant*"
                  "defparameter*"
                  "defgeneric*"
                  "defmethod*"
                  "lambda*"
                  "flet*"
                  "labels*") 'words))
    (font-lock-add-keywords 'lisp-mode
      `((,*lisp-special-forms* . font-lock-keyword-face)))
