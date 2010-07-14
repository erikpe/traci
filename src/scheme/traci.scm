(define (replace-nth n l e)
  (if (= 0 n)
      (cons e (cdr l))
      (cons (car l)
	    (replace-nth (- n 1) (cdr l) e))))

(define (add-last e l)
  (if (null? l)
      (list e)
      (cons (car l) (add-last e (cdr l)))))

(define (make-shape variant)
  (list 'shape variant '()))

(define (make-transform variant)
  (list 'transform variant '()))

(define (add-shape outer-shape shape)
  (let ((shapes (list-ref outer-shape 2)))
    (replace-nth 2 outer-shape (add-last shape shapes))))

(define (add-transform outer-shape transform)
  (let ((transforms (list-ref outer-shape 2)))
    (replace-nth 2 outer-shape (add-last transform transforms))))

(define (shape variant args)
  (let ((this-shape (make-shape variant)))
    (for-each
     (lambda (inner-arg) (set! this-shape (inner-arg this-shape)))
     args)
    (lambda (outer-shape)
      (add-shape outer-shape this-shape))))

(define (union . args)
  (shape 'union args))

(define (difference . args)
  (shape 'difference args))

(define (intersection . args)
  (shape 'intersection args))

(define (sphere . args)
  (shape 'sphere args))

(define (cylinder . args)
  (shape 'cylinder args))

(define (box . args)
  (shape 'box args))

(define (transform variant args)
  (let ((this-transform (make-transform variant)))
    (set! this-transform (replace-nth 2 this-transform args))
    (lambda (outer-shape)
      (add-transform outer-shape this-transform))))

(define (translate . args)
  (transform 'translate args))

(define (scale . args)
  (transform 'scale args))

(define (rotx arg)
  (transform 'rotx arg))

(define (roty arg)
  (transform 'roty arg))

(define (rotz arg)
  (transform 'roty arg))

(define (insert . args)
  (define (insert-helper arg)
    (cond ((null? arg)
	   (lambda (outer-shape) outer-shape))
	  ((pair? arg)
	   (lambda (outer-shape)
	     (let ((outer-shape ((insert-helper (car arg)) outer-shape)))
	       ((insert-helper (cdr arg)) outer-shape))))
	  (#t
	   (lambda (outer-shape)
	     (arg outer-shape)))))
  (insert-helper args))

(define-syntax loop
  (syntax-rules ()
    ((_ var from to args ...)
     (lambda (outer-shape)
       (do ((outer-shape outer-shape ((insert args ...) outer-shape))
            (var from (+ 1 var)))
           ((>= var to) outer-shape))))))

(define epsilon .001)

(define (peg . args)
  (cylinder
   (scale .3 .1 .3)
   (translate .25 .6 .25)
   (insert args)))

(define (hole . args)
  (cylinder
   (scale .3 (+ (* 2 epsilon) .5) .3)
   (rotx 90)
   (translate .5 .3 (- epsilon))
   (insert args)))

(define (lego pegs . args)
  (union
   (difference
    (box (scale (/ pegs 2.0) .6 .5))
    (loop i 0 (- pegs 2)
	  (hole (translate (* i .5)))))
   (loop i 0 (- pegs 1)
	 (peg (translate (* i .5))))
   (insert args)))

(define (disp arg)
  (display (car (list-ref (arg (make-shape 'dummy-object)) 2)))
  (display "\n"))

;(disp (lego 100))
(disp (union (translate 1 2 3 (sphere))))
