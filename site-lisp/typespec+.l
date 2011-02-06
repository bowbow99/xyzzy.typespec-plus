;;; -*- mode: lisp; package: typespec+ -*-
;;;
;;; typespec+.l --- type specifier ‚ð‚à‚Á‚ÆŽg‚¢‚â‚·‚­
;;;
;;; Author: bowbow99 <bowbow99@gmail.com>
;;; Version: 0.00.00
;;; Category: Lisp Libraries
;;; Depends:
;;; License: MIT (see COPYING)
;;; Commentary: (see README.md)
;;; ChangeLog: (see ChangeLog)

;;; Code:

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require "cmu_loop"))

(defpackage :typespec+
  (:use :lisp))

(in-package :typespec+)

(defconstant +original-typep-function+ #'typep)

(export '(+original-typep-function+
          optimize-typep
          optimize-typep-1
          typepred
          ;; configuration variables
          *typep-warn-on-undefined-typespec*
          *optimize-typep-warn-on-undefined-typespec*
          ))

#| Original Definition of `typep`
(defun typep (object type)
  (let (r)
    (unless (atom type)
      (setq r (cdr type))
      (setq type (car type)))
    (let ((f (get type 'type-predicate)))
      (when f
        (return-from typep (funcall f object))))
    (let ((f (get type 'type-specifier-list)))
      (when f
        (return-from typep (funcall f object r))))
    (let ((f (get type 'structure-definition)))
      (when (and f (*structurep object))
        (return-from typep
          (*structure-subtypep (*structure-definition object) f)))))
  nil)
|#

;;;;
;;;; * User Configuration

(defparameter *typep-warn-on-undefined-typespec* nil
  "If non-nil, warn when undefined typespec used on `type`.")

(defparameter *optimize-typep-warn-on-undefined-typespec* t
  "If non-nil, warn when undefined typespec given to `optimize-typep` or `optimize-typep-1`.")

;;;;
;;;; * Utilities

(defmacro awhen (test-form &body body)
  `(let ((it ,test-form))
     (if it (progn ,@body))))
#+xyzzy
(setf (get 'awhen 'ed:lisp-indent-hook) 1)

(defmacro acond (&rest clauses)
  (labels ((expand-clauses (clauses)
             (cond ((null clauses) nil)
                   ((and (null (cdr clauses))
                         (eql (caar clauses) t))
                    (if (null (cdar clauses))
                      nil
                      `(progn ,@(cdar clauses))))
                   (t
                    `(if (setq it ,(caar clauses))
                       (progn ,@(cdar clauses))
                       ,(expand-clauses (cdr clauses)))))))
    `(let ((it nil))
       ,(expand-clauses clauses))))
#+xyzzy
(setf (get 'acond 'ed:lisp-indent-hook) 0)

(defun single? (object)
  (and (consp object) (null (cdr object))))

(defun *let (binding &rest body)
  (let ((var (first binding))
        (object (second binding)))
    (if (or (symbolp object)
            (constantp object))
      (labels ((rec (form)
                 (cond ((eql form var) object)
                       ((atom form) form)
                       (t (cons (rec (car form))
                                (rec (cdr form)))))))
        (if (single? body) (rec (car body)) `(progn ,@(mapcar #'rec body))))
      `(let ((,var ,object)) ,@body))))
#+xyzzy
(setf (get '*let 'ed:lisp-indent-hook) 1)

(defun *and (tests)
  (let ((tests% (remove-if (lambda (x) (and (constantp x) x)) tests)))
    (case (length tests%)
      (0 t)
      (1 (car tests%))
      (t `(and ,@tests%)))))

(defun *or (&rest tests)
  (cond ((single? tests) (car tests))
        (t `(or ,@tests))))

(defun both (a b &optional (pred #'identity))
  (and (funcall pred a) (funcall pred b)))

;;;;
;;;; * Funciton typep
#|
Implementation Note
===================

    (typep OBJECT TYPE)
    (typep OBJECT (TYPE . ARGS))

`typep` decide how to check whether OBJECT is type of TYPE from propery of
symbol given as TYPE.


si::type-predicate
------------------
If TYPE has this property, it must be a function which takes one argument,
OBJECT, and return true if the OBJECT is type of TYPE.

    (typep OBJECT TYPE) == (PREDICATE OBJECT)


si::type-specifier-list
-----------------------
If TYPE has this property, it must be a function which takes two arguments,
OBJECT and ARGS, and return true if the OBJECT is type of TYPE and satisfies
requirement specified by ARGS.

    (typep OBJECT (TYPE . ARGS)) == (<function> OBJECT ARGS)

NOET: These are defined via `si::defpred` in lisp/typespec.l.

NOTE: Some compound type specifiers accepts `nil` as ARGS. In other words,
all arguments are optional. In most cases, it can be just a symbol (with-
out parenthesis) if there's no argument. But some type specifiers like `and`
doesn't allowed to be just a symbol.


si::deftype-definition
----------------------
A type specifier defined via `deftype` has this property. It is a function
which takes arguments specified in `deftype` form, and return expansion of
defined typespec. Returned expansion should be a valid typespec.

    (typep OBJECT (TYPE . ARGS)) == (typep OBJECT (apply <expander> ARGS))

NOTE: xyzzy's default `typep` doesn't support these typespecs, and you need
to use `si:canonicalize-type` to expand them.


si::structure-definition
------------------------
Structure name defined via `defstruct` has this property, and its value is
a object called "structure-definition". It can be compared with "structure-
object".

    (type OBJECT STRUCTURE)
    == (and (si:*structurep OBJECT)
            (si::*structure-subtypep (si:*structure-definition OBJECT)
                                     #<structure-definition: STRUCTURE>))


|#

(defun lisp:typep (object typespec)
  "Return true if OBJECT is type of TYPESPEC."
  (multiple-value-bind (type args)
      (if (consp typespec)
        (values (car typespec) (cdr typespec))
        (values typespec nil))
    (acond
      ((get type 'si::type-predicate)
       (funcall it object))
      ((get type 'si::type-specifier-list)
       ;; TODO: some typespecs (ie. `and`, `or`) are not allowed to be a symbol
       (funcall it object args))
      ((get type 'si::structure-definition)
       (and (si:*structurep object)
            (si:*structure-subtypep (si:*structure-definition object) it)))
      ((get type 'si::deftype-definition)
       (typep object (si:canonicalize-type typespec)))
      (t
       (when *typep-warn-on-undefined-typespec*
         (warn "type specifier ‚¶‚á‚È‚¢‚Ý‚½‚¢: ~S" typespec))
       nil))))

;;;;
;;;; * Optimize `typep` form

(defparameter *typep-optimizer-table* (make-hash-table))

(defmacro define-typep-optimizer (name (&rest args) &body body)
  `(setf (gethash ',name *typep-optimizer-table*)
         (lambda (,@args) (block ,name ,@body))))


;;; typespecs defined via `defpred` in lisp/typespec.l has their predicate
;;; function, but can be transformed into more straightforward form.

;; t, nil
(define-typep-optimizer t   (object) (*let `(#:object ,object) t))
(define-typep-optimizer nil (object) (*let `(#:object ,object) nil))

;; satisfies
(define-typep-optimizer satisfies (object pred)
  ;; TODO: CLHS explicitly denying use of lambda-expression as pred
  `(,pred ,object))

;; member
(define-typep-optimizer member (object &rest items)
  `(member ,object '(,@items) :test #'eql))

;; eql
(define-typep-optimizer eql (actual expected)
  `(eql ,actual ',expected))

;; not
(define-typep-optimizer not (object typespec)
  (*let `(#1=#:object ,object)
    (let ((optimized (optimize-typep-1 '#1# typespec)))
      (cond ((eql optimized t) nil)
            ((eql optimized nil) t)
            (t `(not ,optimized))))))

;; and
(define-typep-optimizer and (object &rest typespecs)
  (if (null typespecs)
    ;; `*let` removes OBJECT form if its a constant or a variable (not necessary to evaluate)
    (*let `(#:object ,object) t)
    (*let `(#1=#:object ,object)
      (*and (mapcar (lambda (typespec)
                      (optimize-typep-1 '#1# typespec))
              typespecs)))))

;; or
(define-typep-optimizer or (object &rest typespecs)
  (if (null typespecs)
    (*let `(#:object ,object) nil)
    (*let `(#1=#:object ,object)
      (apply #'*or
             (mapcar (lambda (typespec)
                       (optimize-typep-1 '#1# typespec))
               typespecs)))))

;; standard-char
(define-typep-optimizer standard-char (object)
  (*let `(#1=#:object ,object)
    `(and (characterp #1#)
          (standard-char-p #1#))))

;; extended-character
(define-typep-optimizer extended-character (object)
  (*let `(#:object ,object) nil))


;;; Numbers

(defun expand-number-range-check (object low high)
  (cond ((both low high #'numberp)
         `((<= ,low ,object ,high)))
        ((both low high #'consp) ; should be a singleton list of number
         `((< ,(car low) ,object ,(car high))))
        (t
         `(,(cond ((eql low '*)   t)
                  ((numberp low)  `(<= ,low ,object))
                  ((consp low)    `(< ,(car low) ,object)))
           ,(cond ((eql high '*)   t)
                  ((numberp high) `(<= ,object ,high))
                  ((consp high)   `(< ,object ,(car high))))))))

(defmacro defopt-number (name predicate)
  `(define-typep-optimizer ,name (object &optional (low '*) (high '*))
     (if (both low high (lambda (x) (eql x '*)))
       `(,',predicate ,object)
       (*let `(#1=#:object ,object)
         (*and (list* `(,',predicate #1#)
                      (expand-number-range-check '#1# low high)))))))

(defopt-number integer integerp)
(defopt-number float floatp)
(defopt-number rational rationalp)
(defopt-number short-float short-float-p)
(defopt-number single-float single-float-p)
(defopt-number long-float long-float-p)
(defopt-number double-float double-float-p)

;; complex
(define-typep-optimizer complex (object &optional (typespec '*))
  (if (eql typespec '*)
    `(complexp ,object)
    (*let `(#1=#:object ,object)
      (*and (list '(complexp #1#)
                  (if (eql typespec '*) t
                    (optimize-typep-1 '(realpart #1#) typespec)))))))


;;; Arrays
(defun expand-array-element-type-check (object element-type)
  (if (eql element-type '*) t
    `(eql (array-element-type ,object)
          ',(upgraded-array-element-type element-type))))


(defun expand-array-dimension-check (object dimension-spec)
  (cond ((eql dimension-spec '*) nil)
        ((atom dimension-spec) `((= (array-rank ,object) ,dimension-spec)))
        (t `((= (array-rank ,object) ,(length dimension-spec))
             ,@(loop for n in dimension-spec
                     for i = 0 then (1+ i)
                 collect (if (eql n '*) t
                           `(= (array-dimension object ,i) ,n)))))))

(defun expand-vector-size-check (object size)
  (if (eql size '*) t
    `(= (array-dimension ,object 0) ,size)))

;; array [element-type] [dimension-spec]
(define-typep-optimizer array (object &optional (element-type '*) (dimension-spec '*))
  (*let `(#1=#:object ,object)
    (*and (list* '(arrayp #1#)
                 (expand-array-element-type-check '#1# element-type)
                 (expand-array-dimension-check '#1# dimension-spec)))))


;; simple-array [element-type] [dimension-spec]
(define-typep-optimizer simple-array (object &optional (element-type '*) (dimension-spec '*))
  (*let `(#1=#:object ,object)
    (*and (list* '(si:*simple-array-p #1#)
                 (expand-array-element-type-check '#1# element-type)
                 (expand-array-dimension-check '#1# dimension-spec)))))

;; vector [element-type] [size]
(define-typep-optimizer vector (object &optional (element-type '*) (size '*))
  (*let `(#1=#:object ,object)
    (*and (list* '(vectorp #1#)
                 (expand-array-element-type-check '#1# element-type)
                 (expand-vector-size-check '#1# size)))))

;; simple-vector [size]
(define-typep-optimizer simple-vector (object &optional (size '*))
  (*let `(#1=#:object ,object)
    (*and (list* '(simple-vector-p #1#)
                 (expand-vector-size-check '#1# size)))))

;; string [size]
(define-typep-optimizer string (object &optional (size '*))
  (*let `(#1=#:object ,object)
    (*and (list* '(stringp #1#)
                 (expand-vector-size-check '#1# size)))))

;; simple-string [size]
(define-typep-optimizer simple-string (object &optional (size '*))
  (*let `(#1=#:object ,object)
    (*and (list* '(simple-string-p #1#)
                 (expand-vector-size-check '#1# size)))))

;;; * Function optimize-typep

(defun optimize-typep-1 (object-form typespec
                         &optional (no-warn (null *optimize-typep-warn-on-undefined-typespec*)))
  (multiple-value-bind (type args)
      (if (consp typespec)
        (values (car typespec) (cdr typespec))
        (values typespec nil))
    (acond
      ((gethash type *typep-optimizer-table*)
       (apply it object-form args))
      ((get type 'si::type-predicate)
       `(,it ,object-form))
      ((get type 'si::type-specifier-list)
       `(,it ,object-form '(,@args)))
      ((get type 'si:structure-definition)
       `(let ((#1=#:object ,object-form))
          (and (si:*structurep #1#)
               (si:*structure-subtypep (si:*structure-definition #1#) ,it))))
      ((get type 'si::deftype-definition)
       (optimize-typep-1 object-form (si:canonicalize-type typespec)))
      (t
       (unless no-warn
         (warn "type specifier ‚¶‚á‚È‚¢‚Ý‚½‚¢: ~S" typespec))
       `(typep ,object-form ',typespec)))))

(defun optimize-typep (form)
  "Take a `typep` form and return a form that do same thing but in more efficiently."
  (unless (and (consp form)
               (eq (car form) 'typep)
               (= (length form) 3))
    (error "Invalid typep form: ~S" form))
  (if (and (consp (third form))
           (eql (car (third form)) 'quote))
    (optimize-typep-1 (second form) (cadr (third form)) t)
    form))

;;;;
;;;; * Function `typepred`

(defun typepred (typespec)
  "Return a function which takes one argument OBJECT and return true if the
OBJECT is type of TYPESPEC."
  (let ((form (macroexpand (optimize-typep-1 'x typespec))))
    (cond
     ((eql form t)   (lambda (x) t))
     ((eql form nil) (lambda (x) nil))
     ((and (= (length form) 2)
           (eql (second form) 'x))
      (car form))
     (t
      (coerce `(lambda (x) ,form) 'function)))))

(provide "typespec+")
;;; typespec+.l ends here.
