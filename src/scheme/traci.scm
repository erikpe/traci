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
  (let ((this-transform (replace-nth 2 (make-transform variant) args)))
    (lambda (outer-shape)
      (add-transform outer-shape this-transform))))

(define (translate . args)
  (transform 'translate args))

(define (scale . args)
  (transform 'scale args))

(define (rotate . args)
  (transform 'rotate args))

(define (insert2 . args)
  (lambda (outer-shape)
    (let ((outer-shape outer-shape))
      (for-each
       (lambda (inner-arg) (set! outer-shape (inner-arg outer-shape)))
       args)
      outer-shape)))

(define-syntax insert3
  (syntax-rules ()
    ((_ (args ...) . rest)
     (insert args ... . rest))
    ((_ arg . rest)
     (lambda (outer-shape)
       (let ((outer-shape (arg outer-shape)))
	 ((insert . rest) outer-shape))))))

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

(define-syntax insert-into-last
  (syntax-rules ()
    ((_ (call-form ...) (last-def-form ...))
     (last-def-form ... call-form ...))
    ((_ (call-form ...) def-form . def-forms)
     (begin def-form (insert-into-last (call-form ...) . def-forms)))))

(define-syntax def
  (syntax-rules ()
    ((_ name (args ...) def-form ...)
     (define-syntax name
       (syntax-rules ()
	 ((_ (args ...))
	  (begin def-form ...))
	 ((_ (args ...) call-form . call-forms)
	  (insert-into-last (call-form . call-forms) def-form ...)))))))

(define (disp arg)
  (display (car (list-ref (arg (make-shape 'dummy-object)) 2))))

(define (peg . args)
  (cylinder
   (scale .3 .1 .3)
   (translate .25 .6 .25)
   (insert args)))

(disp (peg
       (translate 1 0 0)
       (translate 2 0 0)
       (loop i 0 3
	     (translate i i i)
	     (translate (* i 2) i i))
       (insert
	(scale 11 22 33)
	(scale 111 222 333))))

;(define (lego pegs)
;  (union
;   (difference
;    (box (scale (/ n 2.0) .6 .5)))
;   (peg

;(def peg2 (h hh)
;     (+ 1 2)
;     (cylinder (scale hh h h)))

;(disp

; (peg2 (17 23)
;      (translate 1 2 3)
;      (translate 11 22 33)))

;(disp

; (peg2 (18 19)
;      (scale 8 8 8)))

;(disp
 
; (peg2 (20 21)))

;(define (lego len)
;  (union
;   (peg)))
;  (union
;   (box (scale (/ len 2) .6 .5))
;   (loop i 1 len
;   (peg (translate (* i .5) 0 0))))

(display "\n")

