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

(defpackage :typespec+
  (:use :lisp))

(in-package :typespec+)

(defconstant +original-typep-function+ #'typep)

(export '(+original-typep-function+
          optimize-typep))


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
      ;; TODO: if `type` has `si::deftype-definition` property, it can be
      ;; expands into proper typespec via `si:canonicalize-type`.
      ((get type 'si::deftype-definition)
       (typep object (si:canonicalize-type typespec)))
      (t (warn "type specifier ‚¶‚á‚È‚¢‚Ý‚½‚¢: ~S" typespec)))))


;;;;
;;;; * Optimize `typep` form

(defparameter *typep-optimizer-table* (make-hash-table))

(defmacro define-typep-optimizer (name (&rest args) &body body)
  `(setf (gethash ',name *typep-optimizer-table*)
         (lambda (,@args) (block ,name ,@body))))

(defun optimize-typep (form)
  "Take a `typep` form and return a form that do same thing but in more efficiently."
  (unless (and (consp form)
               (eq (car form) 'typep)
               (= (length form) 3))
    (error "Invalid typep form: ~S" form))
  (let ((object-form (second form))
        (typespec    (third form)))
    (if (and (consp typespec)
             (eq (car typespec) 'quote))
      (progn
        (setf typespec (second typespec))
        (multiple-value-bind (type args)
            (if (consp typespec)
              (values (car typespec) (cdr typespec))
              (values typespec nil))
          (acond
            ((gethash type *typep-optimizer-table*)
             (apply it object-form args))
            ((get type 'si::type-predicate)
             (list it object-form))
            ((get type 'si::type-specifier-list)
             ;; TODO: these functions can be transformed into more effective form.
             (list it object-form `',args))
            ((get type 'si:structure-definition)
             `(let ((#1=#:object ,object-form))
                (and (si:*structurep #1#)
                     (si:*structure-subtypep (si:*structure-definition #1#) ,it))))
            ((get type 'si::deftype-definition)
             (optimize-typep `(typep ,object-form ,(si:canonicalize-type typespec))))
            (t form))))
      form)))


;;; typespec+.l ends here.
