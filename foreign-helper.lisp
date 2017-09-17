(in-package #:foreign-helper)
;;==============================================================================
;;
;; 
;; Automatic foreign array management.
;;
;; Allocated objects pre-pend a 'next' pointer; head is in *mallocs*.
;; The returned value is actually at +4; we hide next at +0
;;
;; malloc:vec


(eval-when (:execute :load-toplevel :compile-toplevel)
  (defparameter *nullptr* (null-pointer))
  (defparameter *mallocs* (null-pointer))
  (export '*mallocs*))
;;------------------------------------------------------------------------------
;; Free all the allocations in a quick loop.
;;
(defun mallocs-free ()
  (let ((pointer *mallocs*)
	(np *nullptr*))
    (loop for next = (cffi:mem-ref pointer :pointer)
       for dummy = (progn
		     ;;	   (format t "freeing ~A~&" p)
		     (cffi:foreign-free pointer))
       until (eql np next)
       do;; (print next)
	 (setf pointer next)))
  (setf *mallocs* *nullptr*))

(export 'mallocs-free)

;;------------------------------------------------------------------------------
;; Allocate some, then free
;; do we need a fancier one? with custom mm?
(defmacro with-mallocs (&body body)
  `(let ((*mallocs* *nullptr*))
     ,@body
     (mallocs-free)))

(export 'with-mallocs)

;;------------------------------------------------------------------------------
;; Finally, allocator for foreign arrays...
;;
;; low-level allocator.
(defun malloc (size)
  (let ((next *mallocs*)
	(ptr (setf *mallocs* (cffi::%foreign-alloc (+ 4 size)))))
    (cffi::%mem-set next ptr :pointer) ;; at bottom of new alloc, store next
    (cffi:inc-pointer ptr 4)))




(export 'malloc)


(defun mallocs-count ()
  (let ((ptr *mallocs*))
    (loop until (cffi:null-pointer-p ptr)
       for i from 0
       do
	 (setf ptr (mem-ref ptr :pointer))
       finally (return i))))

(export 'mallocs-count)


(defun foreign-element-coerce (foreign-type value)
  "attempt to convert a value to the canonicalized foreign-type"
  (format t "CONVERTING ~A to ~A~&" value foreign-type)
  (flet ((integer-helper (typespec)
	   (multiple-value-bind (v r)(truncate value)
	     (if (and (zerop r)
		      (typep v typespec))
		 v
		 (error "value ~A cannot be stored as foreign type ~A"
			value foreign-type )))))
    (if (numberp value);literal value?
	(case foreign-type
	  (:unsigned-char (integer-helper '(integer 0 #xFF)))
	  (:unsigned-short (integer-helper '(integer 0 #xFFFF)))
	  (:unsigned-int   (integer-helper '(integer 0 #xFFFFFFFF)))
	  (:unsigned-long-long   (integer-helper '(integer 0 #xFFFFFFFFFFFFFFFF)))
	  ;;
	  (:char  (integer-helper '(integer       -128   127)))
	  (:short (integer-helper '(integer     -32768 32767)))
	  (:int   (integer-helper '(integer -2147483648  2147483647)))
	  (:long-long (integer-helper '(integer  -9223372036854775808
					9223372036854775807)))
	  ;;
	  ((:float :double) (float value))
	  ;;
	  (:foreign-pointer
	   (typecase value
	     (foreign-pointer value)
	     (integer (case (foreign-type-size :pointer)
			(4 (cffi-sys:make-pointer 
			    (integer-helper '(integer 0 #xFFFFFFFF))))
			(8 (cffi-sys:make-pointer 
			    (integer-helper '(integer 0 #xFFFFFFFFFFFFFFFF)))))))))
	value)))
;; Process a list of values sent to initialize a foreign vector.
;; literal values are compiled; any functions must be executed at runtime;
;; so we shall build a let statement.

;;==============================================================================
;;
;; 
;; Infer vector type, try to normalize initializaiton list.
;; any non-literal initializers are passed on not checked.
;;
;; return size count type init-array
(defun %vec-type-infer (data)
  "given %vec initializer, return type and normalized list"
  (multiple-value-bind (foreign-type initlist)
      (let ((first (first data)))   
	(typecase first
	  (symbol (values (cffi::canonicalize-foreign-type first)
			  (cdr data)))
	  (integer (values :int data))
	  (double-float :double-float data)
	  (float (values :float data))
	  (t (error "Unsupported foreign vector element ~A type ~A"
		    first (type-of first)))))
    (values
     (* (length initlist) (cffi::foreign-type-size foreign-type))
     (length initlist)
     foreign-type
     (mapcar (lambda (item)
	       (foreign-element-coerce foreign-type item))
	     initlist))))

(defun %vproc (size count foreign-type initial-contents)
    ;; assuming the worst, create runtime-processing initializer
  (let ((ptr (malloc size)))
    (print initial-contents)
    (dotimes (i count)
      (setf (mem-aref ptr foreign-type i)
	    (elt initial-contents i)))
    ptr))

(defmacro %v (&rest data)
  (multiple-value-bind (size count foreign-type initial-contents)
      (%vec-type-infer data)
    `(%vproc ,size ,count ,foreign-type (list ,@initial-contents)))
)

;; for the case of all literals
#||(define-compiler-macro %v (&whole form &rest data)
  (if (every #'numberp data)
      (multiple-value-bind (foreign-type initial-contents)
	  (%vec-type-infer data)
	;; since we only have literals, just compile
	`(malloc ,foreign-type ',initial-contents))
     form))
||#
(export '%v)

;;==============================================================================
;; A reader macro to allow easy syntax of:
;;
;; #{:int 1 2 3 } or just #{ 1 2 3}
;;
;; The latter form decides the type based on the first element's type.
;;



;;==============================================================================
;;(eval-when (:execute :load-toplevel :compile-toplevel))
(defun foreign-vec-reader(stream sub-char )
  
  (let ((data (read-delimited-list #\} stream t)))
    `(%v ,@data)))
  
;;(set-dispatch-macro-character #\# #\{ #'foreign-vec-reader)
(set-macro-character #\{ #'foreign-vec-reader)
(set-macro-character #\} (get-macro-character #\) ))



(defun bench ()
  (loop for i from 1 to 100000 do
       (let ((q (fh:malloc 4)))
	 (setf (cffi:mem-ref q :uint ) 0)
	 (cffi:mem-ref q :uint )
	 )))
