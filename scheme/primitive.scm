;;; Generic helpers
;;; ---------------

;;(define (add-last l e)
;;  (if (null? l)
;;      (list e)
;;      (cons (car l) (add-last (cdr l) e))))

(define (replace-nth n l e)
  (if (= 0 n)
      (cons e (cdr l))
      (cons (car l)
            (replace-nth (- n 1) (cdr l) e))))

(define (first-n l n)
  (if (= 0 n) '()
      (cons (car l) (first-n (cdr l) (- n 1)))))

(define (rest-n l n)
  (if (= 0 n) l
      (rest-n (cdr l) (- n 1))))

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
	 (cons 'vec args))))

(define (vec? obj)
  (and (list? obj)
       (= 4 (length obj))
       (eq? 'vec (list-ref obj 0))
       (number? (list-ref obj 1))
       (number? (list-ref obj 2))
       (number? (list-ref obj 3))))

(define (vec-x vec)
  (if (not (vec? vec))
      (error 'vec-x "Not a vactor: `~a'" vec)
      (list-ref vec 1)))

(define (vec-y vec)
  (if (not (vec? vec))
      (error 'vec-y "Not a vactor: `~a'" vec)
      (list-ref vec 2)))

(define (vec-z vec)
  (if (not (vec? vec))
      (error 'vec-z "Not a vactor: `~a'" vec)
      (list-ref vec 3)))

;;; Color primitives
;;; ----------------

(define (color . args)
  (cond ((not (= 3 (length args)))
         (error 'color "Expected 3 arguments, got ~a" (length args)))
        ((not (number? (list-ref args 0)))
         (error 'color "Argument 1 not a number: `~a'" (list-ref args 0)))
        ((not (number? (list-ref args 1)))
         (error 'color "Argument 2 not a number: `~a'" (list-ref args 1)))
        ((not (number? (list-ref args 2)))
         (error 'color "Argument 3 not a number: `~a'" (list-ref args 2)))
        (#t
         (cons 'color args))))

(define (color? obj)
  (and (list? obj)
       (= 4 (length obj))
       (eq? 'color (list-ref obj 0))
       (number? (list-ref obj 1))
       (number? (list-ref obj 2))
       (number? (list-ref obj 3))))

(define (color-r color)
  (if (not (color? color))
      (error 'color-r "Not a color: `~a'" color)
      (list-ref color 1)))
       
(define (color-g color)
  (if (not (color? color))
      (error 'color-g "Not a color: `~a'" color)
      (list-ref color 2)))
       
(define (color-b color)
  (if (not (color? color))
      (error 'color-b "Not a color: `~a'" color)
      (list-ref color 3)))
       
;;; Shape primitives
;;; ----------------

(define (make-shape variant)
  (if (not (symbol? variant))
      (error 'make-shape "Not a symbol: `~a'" variant))
      (list 'shape variant (make-arg-list) (make-arg-list)))

(define (shape? obj)
  (and (list? obj)
       (eq? 'shape (car obj))
       (= 4 (length obj))))

(define (shape-variant shape)
  (if (not (shape? shape))
      (error 'shape-variant "Not a shape: `~a'" shape)
      (list-ref shape 1)))

(define (shape-get-constructor-arg-list shape)
  (if (not (shape? shape))
      (error 'shape-get-constructor-arg-list "Not a shape: `~a'" shape)
      (list-ref shape 2)))

(define (shape-set-constructor-arg-list shape arg-list)
  (cond ((not (shape? shape))
	 (error 'shape-set-constructor-arg-list "Argument 1 not a shape: `~a'" shape))
	((not (arg-list? arg-list))
	 (error 'shape-set-constructor-arg-list "Argument 2 not an arg-list: `~a'" arg-list))
	(#t
	 (replace-nth 2 shape arg-list))))

(define (shape-get-arg-list shape)
  (if (not (shape? shape))
      (error 'shape-get-arg-list "Not a shape: `~a'" shape)
      (list-ref shape 3)))

(define (shape-set-arg-list shape arg-list)
  (cond ((not (shape? shape))
	 (error 'shape-set-arg-list "Argument 1 not a shape: `~a'" shape))
	((not (arg-list? arg-list))
	 (error 'shape-set-arg-list "Argument 2 not an arg-list: `~a'" arg-list))
	(#t
	 (replace-nth 3 shape arg-list))))

;;; Transform primitives
;;; --------------------

(define (make-generic-transform variant arg)
  (if (not (symbol? variant))
      (error 'make-generic-transform "Not a symbol: `~a'" variant)
      (list 'transform variant arg)))

(define (transform? obj)
  (and (list? obj)
       (= 3 (length obj))
       (eq? 'transform (car obj))))

(define (transform-variant transform)
  (if (not (transform? transform))
      (error 'transform-variant "Not a transform: `~a'" transform)
      (list-ref transform 1)))

(define (transform-get-argument transform)
  (if (not (transform? transform))
      (error 'transform-get-argument "Not a transform: `~a'" transform)
      (list-ref transform 2)))

;;; Arg-list primitives
;;; -------------------

(define (make-arg-list)
  '(arg-list))

(define (arg-list? obj)
  (and (not (null? obj))
       (list? obj)
       (eq? 'arg-list (car obj))))

(define (arg-list-add-arg arg-list arg)
  (cond ((not (arg-list? arg-list))
         (error 'arg-list-add-arg "Not an arg-list: `~a'" arg-list))
        ;((arg-list? arg)
        ; (error 'arg-list-add-arg "Attempting to insert arg-list into arg-list"))
        (#t
         (cons (car arg-list) (cons arg (cdr arg-list))))))

(define (arg-list-empty? arg-list)
  (if (not (arg-list? arg-list))
      (error 'arg-list-empty? "Not an arg-list: `~a'" arg-list)
      (null? (cdr arg-list))))

(define (arg-list-merge arg-list1 arg-list2)
  (cond ((not (arg-list? arg-list1))
         (error 'arg-list-merge "Argument 1 not an arg-list: `~a'" arg-list1))
        ((not (arg-list? arg-list2))
         (error 'arg-list-merge "Argument 2 not an arg-list: `~a'" arg-list2))
        ((arg-list-empty? arg-list2)
         arg-list1)
        (#t
         (arg-list-add-arg arg-list1 arg-list2))))

;;(define (arg-list-merge arg-list1 arg-list2)
;;  (cond ((not (arg-list? arg-list1))
;;         (error 'arg-list-merge "Argument 1 not an arg-list: `~a'" arg-list1))
;;        ((not (arg-list? arg-list2))
;;         (error 'arg-list-merge "Argument 2 not an arg-list: `~a'" arg-list2))
;;        ((arg-list-empty? arg-list2)
;;         arg-list1)
;;        (#t
;;         (cons (car arg-list1)
;;               (append (cdr arg-list2) (cdr arg-list1))))))

(define (list-of-args-helper args res)
  (cond ((null? args)
         res)
        ((arg-list? (car args))
         (list-of-args-helper (cdr args) (list-of-args-helper (car args) res)))
        ((eq? 'arg-list (car args))
         (list-of-args-helper (cdr args) res))
        (#t
         (list-of-args-helper (cdr args) (cons (car args) res)))))

(define (list-of-args arg-list)
  (if (not (arg-list? arg-list))
      (error 'list-of-args "Not an arg-list: `~a'" arg-list)
      (list-of-args-helper arg-list '())))

;;(define (list-of-args arg-list)
;;  (if (not (arg-list? arg-list))
;;      (error 'list-of-args "Not an arg-list: `~a'" arg-list)
;;      (reverse (cdr arg-list))))
