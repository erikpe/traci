(define (union? shape)
  (and (shape? shape)
       (eq? 'union (shape-variant shape))))

(define (difference? shape)
  (and (shape? shape)
       (eq? 'difference (shape-variant shape))))

(define (intersection? shape)
  (and (shape? shape)
       (eq? 'intersection (shape-variant shape))))

(define (sphere? shape)
  (and (shape? shape)
       (eq? 'sphere (shape-variant shape))))

(define (cylinder? shape)
  (and (shape? shape)
       (eq? 'cylinder (shape-variant shape))))

(define (box? shape)
  (and (shape? shape)
       (eq? 'box (shape-variant shape))))

(define (primitive-shape? shape)
  (or (sphere? shape)
      (cylinder? shape)
      (box? shape)))

(define (csg-shape? shape)
  (or (union? shape)
      (difference? shape)
      (box? shape)))

(define (tranlate? obj)
  (and (transform? obj)
       (eq 'translate (transform-variant obj))))

(define (scale? obj)
  (and (transform? obj)
       (eq 'scale (transform-variant obj))))

(define (rotx? obj)
  (and (transform? obj)
       (eq 'rotx (transform-variant obj))))

(define (roty? obj)
  (and (transform? obj)
       (eq 'roty (transform-variant obj))))

(define (rotz? obj)
  (and (transform? obj)
       (eq 'rotz (transform-variant obj))))

(define (vector-transform? obj)
  (or (translate? obj)
      (scale? obj)))

(define (scalar-transform? obj)
  (or (rotx? obj)
      (roty? obj)
      (rotz? obj)))

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

(define (shape-add-arg outer-shape arg)
  (cond ((not (shape? outer-shape))
	 (error 'shape-add-arg "Argument 1 is not a shape: `~a'" outer-shape))
	((arg-list? arg)
	 (shape-set-arg-list outer-shape
			     (arg-list-merge (shape-get-arg-list outer-shape) arg)))
	((or (transform? arg) (shape? arg))
	 (shape-set-arg-list outer-shape
			     (arg-list-add-arg (shape-get-arg-list outer-shape) arg)))
	(#t
	 (error 'add-arg-to-shape "Argument 2 of unknown type: `~a'" arg))))

(define (generic-shape variant . args)
  (if (not (symbol? variant))
      (error 'generic-shape "Argument 1 not a symbol: `~a'" variant)
      (let ((this-shape (make-shape variant)))
	(for-each
	 (lambda (arg) (set! this-shape (shape-add-arg this-shape arg)))
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
