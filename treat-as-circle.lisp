(in-package :cl-user)

(defpackage :treat-as-circle
  (:use :cl)
  (:export #:elt-as-circle
           #:nth-as-circle
           #:aref-as-circle
           #:char-as-circle
           #:schar-as-circle
           #:svref-as-circle
           #:bit-as-circle
           #:sbit-as-circle))

(in-package :treat-as-circle)

#| (mod 0 3) => 0 ; inside index.
 | (mod 3 3) => 0 ; when over, rewind to zero.
 | (mod -1 3) => 2 ; inverse.
 |#

(defvar *length*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defcircle (name lambda-list)
    (let* ((circle (intern (format nil "~A-AS-CIRCLE" name)))
           (internal (intern (format nil "%~A" circle))))
      `(progn
        ,(interface circle internal lambda-list)
        ,(internal internal lambda-list name)
        ,(setf-form circle internal lambda-list)
        ,(internal-setf internal lambda-list name))))
  (defun interface (name internal lambda-list)
    `(defun ,name ,(canonicalize lambda-list)
       (let ((*length* (length ,(car (find-if #'listp lambda-list)))))
         (,internal ,@(arguments lambda-list)))))
  (defun canonicalize (lambda-list)
    (loop :for elt :in lambda-list
          :collect (if (listp elt)
                       (car elt)
                       elt)))
  (defun arguments (lambda-list)
    (loop :for elt :in lambda-list
          :unless (find elt lambda-list-keywords)
            :collect (if (listp elt)
                         (car elt)
                         elt)))
  (defun internal (internal lambda-list name)
    `(defun ,internal ,(canonicalize lambda-list) ,(body name lambda-list)))
  (defun body (op lambda-list)
    `(,op
      ,@(mapcar
          (lambda (form)
            (if (listp form)
                (car form)
                `(mod ,form *length*)))
          lambda-list)))
  (defun setf-form (name internal lambda-list)
    `(defun (setf ,name) ,(cons 'new-value (canonicalize lambda-list))
       (let ((*length* (length ,(car (find-if #'listp lambda-list)))))
         (setf (,internal ,@(arguments lambda-list)) new-value))))
  (defun internal-setf (internal lambda-list name)
    `(defun (setf ,internal) ,(cons 'new-value (canonicalize lambda-list))
       (setf ,(body name lambda-list) new-value))))

(defcircle elt ((sequence sequence) index))

(defcircle nth (index (list list)))

(defcircle char ((string string) index))

(defcircle schar ((string string) index))

(defcircle svref ((vector vector) index))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defcircle* (name) `(progn ,(circle name) ,(circle name t)))
  (defun circle (name &optional setf)
    (let ((circle (intern (format nil "~A-AS-CIRCLE" name))))
      (flet ((ensure-name (flag)
               (if flag
                   `(setf ,circle)
                   circle))
             (ensure-lambda-list (flag)
               (let ((lambda-list '(array &rest indexes)))
                 (if flag
                     (cons 'new-value lambda-list)
                     lambda-list)))
             (body (flag)
               (let ((body `(apply #',name array result)))
                 (if flag
                     `(setf ,body new-value)
                     body))))
        `(defun ,(ensure-name setf) ,(ensure-lambda-list setf)
           (loop :for index :in (array-dimensions array)
                 :for arg-value :in indexes
                 :collect (mod arg-value index) :into result
                 :finally (return ,(body setf))))))))

(progn . #.(mapcar (lambda (op) `(defcircle* ,op)) `(aref bit sbit)))