;;; -*- mode: lisp; package: bench.typespec+ -*-
;;;
;;; tests/benchmark.l --- benchmark for typespec+
;;;

;;; Code:

(require "typespec+")
(require "profile") ; I haven't put this on public...

(defpackage :bench.typespec+
  (:use :lisp :typespec+ :profile))

(in-package :bench.typespec+)

(defstruct example foo bar)

(defparameter *example-typespec-list*
  '(t nil atom function hash-table keyword readtable list compiled-function package cons random-state
    symbol sequence character base-character standard-char extended-character
    ;; array
    array (array *) (array * *) (array t *) (array character *) (array t 6) (array character 6)
    simple-array (simple-array *) (simple-array * *) (simple-array t *) (simple-array character *)
    (simple-array t 6) (simple-array character 6)
    ;; vetor
    vector (vector *) (vector t) (vector character) (vector * *)
    (vector * 6) (vector t 6) (vector character 6)
    simple-vector (simple-vector *) (simple-vector 6)
    ;; string
    string (string *) (string 6)
    ;; number
    number real rational ratio fixnum bignum
    integer (integer *) (integer * *) (integer 3 *) (integer * 12) (integer 3 12)
    float (float *) (float * *) (float 3 *) (float * 12) (float 3 12)
    short-float (short-float *) (short-float * *) (short-float 3 *) (short-float * 12) (short-float 3 12)
    double-float (double-float *) (double-float * *) (double-float 3 *) (double-float * 12) (double-float 3 12)
    complex
    example
    (and) (and t) (and nil) (and atom integer) (and array string) (and function compiled-function)
    (or) (or t) (or nil) (or string symbol) (or vector list) (or character symbol) (or hash-table)
    ))

(defparameter *example-object-list*
  (list t nil "string" '(foo bar) 0 1 10 100 #(foo bar baz) (make-array '(2 3)) (make-hash-table)
        *package* :keyword (make-example :foo 1 :bar "hello")))


(defun parse-time-output (output)
  (when (ed:string-match "^Time: \\([0-9\\.]+\\) msec$" output)
    (ignore-errors (read-from-string (ed:match-string 1)))))

(defun get-time (form)
  (gc)
  (let ((*trace-output* (make-string-output-stream)))
    (funcall (compile nil `(lambda () (time ,form))))
    (parse-time-output (get-output-stream-string *trace-output*))))

(defun bench-1 (object typespec)
  (list `(typep ,object ',typespec)
        (get-time `(,+original-typep-function+ ',object ',typespec))
        (get-time `(typep ',object ',typespec))
        (get-time (optimize-typep `(typep ',object ',typespec)))))

(defun bench-all ()
  (let (results)
    (dolist (typespec *example-typespec-list*)
      (dolist (object *example-object-list*)
        (push (bench-1 object typespec) results)))
    (nreverse results)))

(defun benchmark ()
  (format t "~8A  ~8A ~7A  ~8A ~7A~%" :original :modified "" :optimized "")
  (format t "~100~~%")
  (let ((results (bench-all)))
    (format t "~:{~2,6F  ~2,6F (~2,3F)  ~2,6F (~2,3F)  ~S~%~}"
      (mapcar (lambda (result)
                (let ((form  (first result))
                      (original (second result))
                      (modified (third result))
                      (optimized (fourth result)))
                  (list original
                        modified (/ modified original)
                        optimized (/ optimized original)
                        form)))
        results))
    (format t "~100~~%")
    (labels ((average (all)
               (/ (apply #'+ all) (length all))))
      (let ((original (average (mapcar #'second results)))
            (modified (average (mapcar #'third results)))
            (optimized (average (mapcar #'fourth results))))
        (format t "~2,6F  ~2,6F (~2,3F)  ~2,6F (~2,3F)   ~S~%"
          original
          modified (/ modified original)
          optimized (/ optimized original)
          :average)))
    (values)))

;;; tests/benchmark.l ends here.
