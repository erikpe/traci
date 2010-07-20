;;; User functions: control functions
;;; ---------------------------------

(define (insert . args)
  (cond ((null? args)
	 (make-arg-list))
	((arg-list? (car args))
	 (arg-list-merge (car args)
			 (apply insert (cdr args))))
	(#t
	 (arg-list-merge (arg-list-add-arg (make-arg-list) (car args))
			 (apply insert (cdr args))))))

(define-syntax loop
  (syntax-rules ()
    ((_ var from to args ...)
     (do ((var from (+ 1 var))
	  (arg-list (make-arg-list)))
	 ((> var to) arg-list)
       (set! arg-list (arg-list-merge arg-list (insert args ...)))))))

;;; User functions: shapes
;;; ----------------------

(define (union . args)
  (generic-shape 'union '() args))

(define (difference . args)
  (generic-shape 'difference '() args))

(define (intersection . args)
  (generic-shape 'intersection '() args))

(define (sphere . args)
  (generic-shape 'sphere '() args))

(define (cylinder . args)
  (if (and (>= (length args) 3)
	   (number? (list-ref args 0))
	   (vec? (list-ref args 1))
	   (vec? (list-ref args 2)))
      (generic-shape 'cylinder (first-n args 3) (rest-n args 3))
      (generic-shape 'cylinder '() args)))

(define (box . args)
  (generic-shape 'box '() args))

;;; User functions: transformations
;;; -------------------------------

(define (translate . args)
  (apply generic-vector-transformation 'translate args))

(define (scale . args)
  (apply generic-vector-transformation 'scale args))

(define (rotx . args)
  (apply generic-scalar-transformation 'rotx args))

(define (roty . args)
  (apply generic-scalar-transformation 'roty args))

(define (rotz . args)
  (apply generic-scalar-transformation 'rotz args))
