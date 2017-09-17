(in-package :fh)
;;==============================================================================
;; RGBA values are very common forms of arrays. 
;; we allow the following text formats:
;;
;; - a single 32-bit value;
;; - a number of floats
;; - a number of ints
;; - any unspecified values are defaulted to 0.0 except A to 1.0.
;;
;; Literal values are range checked.
;;
;; Result compiles to a %v
(defun rgba-component (component)
  (typecase component
    (float
     (if (and (<= component 1.0)
	      (>= component 0.0))
	 component
	 (error "RGBA component ~A out of range" component)))
    (integer
     (if (and (<= component 255)
	      (>= component 0))
	 (/ component 255.0)
	 (error "RGBA component ~A out of range" component)))
    (t component)))

(defun rgba-unpack (val)
  (list  (ldb (byte 8 24) val) 
	 (ldb (byte 8 16) val) 
	 (ldb (byte 8 8) val) 
	 (ldb (byte 8 0) val)))

(defmacro rgba (&rest rest)
  (when (and (= 1  (length rest))
	     (integerp (car rest)))
    (setf rest (rgba-unpack (car rest))))
  (destructuring-bind (&optional (r 0.0) (g 0.0) (b 0.0) (a 1.0))
      (mapcar #'rgba-component rest)
    `(%v :float ,r ,g ,b ,a)))
(export 'rgba)
