;;; Generic helpers
;;; ---------------

(define (add-last l e)
  (if (null? l)
      (list e)
      (cons (car l) (add-last (cdr l) e))))

(define (replace-nth n l e)
  (if (= 0 n)
      (cons e (cdr l))
      (cons (car l)
            (replace-nth (- n 1) (cdr l) e))))

;;; Vector primitives
;;; -----------------

(define (vec . args)
  (cond ((not (= 3 (length args)))
	 (error 'vec "Expected 3 arguments, got ~a" (length args)))
	((not (number? (list-ref args 0)))
	 (error 'vec "Argument 1 not a number: `~a'" (list-ref args 0)))
	((not (number? (list-ref args 1)))
	 (error 'vec "Argument 2 not a number: `~a'" (list-ref args 1)))
	((not (number? (list-ref args 2)))
	 (error 'vec "Argument 3 not a number: `~a'" (list-ref args 2)))
	(#t
	 (cons 'vector args))))

(define (vec? obj)
  (and (list? obj)
       (= 4 (length obj))
       (eq? 'vector (list-ref obj 0))
       (number? (list-ref obj 1))
       (number? (list-ref obj 2))
       (number? (list-ref obj 3))))

(define (vector-x vec)
  (if (not (vec? vec))
      (error 'vector-x "Argument 1 not a vactor: `~a'" vec)
      (list-ref vec 1)))

(define (vector-y vec)
  (if (not (vec? vec))
      (error 'vector-y "Argument 1 not a vactor: `~a'" vec)
      (list-ref vec 2)))

(define (vector-z vec)
  (if (not (vec? vec))
      (error 'vector-z "Argument 1 not a vactor: `~a'" vec)
      (list-ref vec 3)))

;;; Shape primitives
;;; ----------------

(define (make-shape variant)
  (if (not (symbol? variant))
      (error 'make-shape "Argument 1 not a symbol: `~a'" variant))
      (list 'shape variant '()))

(define (shape? obj)
  (and (list? obj)
       (eq? 'shape (car obj))
       (= 3 (length obj))))

(define (shape-variant shape)
  (if (not (shape? shape))
      (error 'shape-variant "Argument 1 not a shape: `~a'" shape)
      (car (cdr shape))))

(define (add-shape-to-shape outer-shape shape)
  (cond ((not (shape? outer-shape))
	 (error 'add-shape-to-shape "Argument 1 not a shape: `~a'" shape))
	((not (shape? shape))
	 (error 'add-shape-to-shape "Argument 2 not a shape: `~a'" outer-shape))
	(#t
	 (let ((inner-args (list-ref outer-shape 2)))
	   (replace-nth 2 outer-shape (add-last inner-args shape))))))

(define (add-transform-to-shape outer-shape transform)
  (cond ((not (shape? outer-shape))
	 (error 'add-transform-to-shape "Argument 1 not a shape: `~a'" outer-shape))
	((not (transform? transform))
	 (error 'add-transform-to-shape "Argument 2 not a transform: `~a'" transform))
	(#t
	 (let ((inner-args (list-ref outer-shape 2)))
	   (replace-nth 2 outer-shape (add-last inner-args transform))))))

;;; Transform primitives
;;; --------------------

(define (make-generic-transform variant arg)
  (if (not (symbol? variant))
      (error 'make-generic-transform "Argument 1 not a symbol: `~a'" variant)
      (list 'transform variant arg)))

(define (transform? obj)
  (and (list? obj)
       (= 3 (length obj))
       (eq? 'transform (car obj))))

(define (transform-variant transform)
  (if (not (transform? transform))
      (error 'transform-variant "Argument 1 not a transform: `~a'" transform)
      (car (cdr transform))))

;;; Arg-list primitives
;;; -------------------

(define (make-arg-list)
  '(arg-list))

(define (arg-list? obj)
  (and (list? obj)
       (eq? 'arg-list (car obj))))

(define (arg-list-add-arg arg-list arg)
  (cond ((not (arg-list? arg-list))
	 (error 'arg-list-add-arg "Argument 1 not an arg-list: `~a'" arg-list))
	((arg-list? arg)
	 (error 'arg-list-add-arg "Attempting to insert arg-list into arg-list"))
	(#t
	 (add-last arg-list arg))))

(define (arg-list-empty? arg-list)
  (if (not (arg-list? arg-list))
      (error 'arg-list-empty? "Argument 1 not an arg-list: `~a'" arg-list)
      (null? (cdr arg-list))))

(define (arg-list-first arg-list)
  (cond ((not (arg-list? arg-list))
	 (error 'arg-list-first "Argument 1 not an arg-list: `~a'" arg-list))
	((arg-list-empty? arg-list)
	 (error 'arg-list-first "Arg-list empty"))
	(#t
	 (car (cdr arg-list)))))

(define (arg-list-rest arg-list)
  (cond ((not (arg-list? arg-list))
	 (error 'arg-list-rest "Argument 1 not an arg-list: `~a'" arg-list))
	((arg-list-empty? arg-list)
	 (error 'arg-list-rest "Arg-list empty"))
	(#t
	 (cons (car arg-list) (cdr (cdr arg-list))))))

;;; Other stuff
;;; -----------

(define (primitive-shape? shape)
  (if (not (shape? shape))
      (error 'primitive-shape? "Argument 1 not a shape: `~a'" shape)
      (let ((variant (shape-variant shape)))
	(or (eq? 'sphere variant)
	    (eq? 'cylinder variant)
	    (eq? 'box variant)))))

(define (csg-shape? shape)
  (if (not (shape? shape))
      (error 'csg-shape? "Argument 1 not a shape: `~a'" shape)
      (let ((variant (shape-variant shape)))
	(or (eq? 'union variant)
	    (eq? 'difference variant)
	    (eq? 'intersection variant)))))

(define (arg-list-merge arg-list1 arg-list2)
  (cond ((not (arg-list? arg-list1))
	 (error 'arg-list-merge "Argument 1 not an arg-list: `~a'" arg-list1))
	((not (arg-list? arg-list2))
	 (error 'arg-list-merge "Argument 2 not an arg-list: `~a'" arg-list2))
	((arg-list-empty? arg-list2)
	 arg-list1)
	(#t
	 (arg-list-merge
	  (arg-list-add-arg arg-list1 (arg-list-first arg-list2))
	  (arg-list-rest arg-list2)))))

(define (add-arg-to-shape outer-shape arg)
  (cond ((not (shape? outer-shape))
	 (error 'add-arg-to-shape "Argument 1 is not a shape: `~a'" outer-shape))
	((shape? arg)
	 (add-shape-to-shape outer-shape arg))
	((transform? arg)
	 (add-transform-to-shape outer-shape arg))
	((arg-list? arg)
	 (if (arg-list-empty? arg) outer-shape
	     (add-arg-to-shape (add-arg-to-shape outer-shape (arg-list-first arg))
			       (arg-list-rest arg))))
	(#t
	 (error 'add-arg-to-shape "Argument 2: Unknown argument type: `~a'" arg))))

(define (generic-shape variant . args)
  (if (not (symbol? variant))
      (error 'generic-shape "Argument 1 not a symbol: `~a'" variant)
      (let ((this-shape (make-shape variant)))
	(for-each
	 (lambda (arg) (set! this-shape (add-arg-to-shape this-shape arg)))
	 args)
	this-shape)))

(define (generic-vector-transformation variant . args)
  (cond ((not (symbol? variant))
	 (error 'generic-vector-transformation "Argument 1 not a symbol: `~a'" variant))
	((= 3 (length args))
	 (generic-vector-transformation variant (apply vec args)))
	((not (= 1 (length args)))
	 (error variant "Expected 1 or 3 arguments, got ~a" (length args)))
	((vec? (car args))
	 (make-generic-transform variant (car args)))
	(#t
	 (error variant "Argument 1 not a vector: `~a'" (car args)))))

(define (generic-scalar-transformation variant . args)
  (cond ((not (symbol? variant))
	 (error 'generic-vector-transformation "Argument 1 not a symbol: `~a'" variant))
	((not (= 1 (length args)))
	 (error variant "Expected 1 argument, got ~a" (length args)))
	((not (number? (car args)))
	 (error variant "Argument 1 not a number: `~a'" (car args)))
	(#t
	 (make-generic-transform variant (car args)))))

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
  (apply generic-shape 'union args))

(define (difference . args)
  (apply generic-shape 'difference args))

(define (intersection . args)
  (apply generic-shape 'intersection args))

(define (sphere . args)
  (apply generic-shape 'sphere args))

(define (cylinder . args)
  (apply generic-shape 'cylinder args))

(define (box . args)
  (apply generic-shape 'box args))

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




(define epsilon 0.0001)

(define (peg . args)
  (cylinder
   (scale .3 .1 .3)
   (translate .25 .6 .25)
   (apply insert args)))

(define (hole . args)
  (cylinder
   (scale .3 (+ (* 2 epsilon) .5) .3)
   (rotx 90)
   (translate .5 .3 (- epsilon))
   (apply insert args)))

(define (lego pegs . args)
  (union
   (difference
    (box (scale (/ pegs 2.0) .6 .5))
    (loop i 0 (- pegs 2)
          (hole (translate (* i .5) 0 0))))
   (loop i 0 (- pegs 1)
         (peg (translate (* i .5) 0 0)))
   (apply insert args)))

;(define (keso . args)
;  (union
;   (insert args)))

(display (lego 4))
(display "\n")
