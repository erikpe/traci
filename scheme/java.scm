;;; Imports
;;; -------

(import s2j)

;;; Java converter
;;; --------------

(define-java-class <traci.math.Vector>)
(define-java-class <traci.math.Transformations>)

(define-java-class <traci.model.shape.csg.Union>)
(define-java-class <traci.model.shape.csg.Difference>)
(define-java-class <traci.model.shape.csg.Intersection>)
(define-java-class <traci.model.shape.primitive.Sphere>)
(define-java-class <traci.model.shape.primitive.Cylinder>)
(define-java-class <traci.model.shape.primitive.Box>)
(define-java-class <traci.model.shape.Bounding-Box>)

(define (->jvec vec)
  (if (not (vec? vec))
      (error '->jvec "Not a vector: `~a'" vec)
      ((generic-java-method '|make|)
       (java-null <traci.math.Vector>)
       (->jdouble (vec-x vec))
       (->jdouble (vec-y vec))
       (->jdouble (vec-z vec)))))

(define (transform-get-java-method transform)
  (cond ((not (transform? transform))
	 (error 'get-java-transform-method "Not a transform: `~a'" transform))
	((translate? transform)
	 (generic-java-method '|translate|))
	((scale? transform)
	 (generic-java-method '|scale|))
	((rotx? transform)
	 (generic-java-method '|rotx|))
	((roty? transform)
	 (generic-java-method '|roty|))
	((rotz? transform)
	 (generic-java-method '|rotz|))
	(#t
	 (error 'get-java-transform-method "Unknown transform type: `~a'" transform))))

(define (->jtransform transform)
  (if (not (transform? transform))
      (error '->jtransform "Not a transform: `~a'" transform)
      ((transform-get-java-method transform)
       (java-null <traci.math.Transformations>)
       (->java (transform-get-argument transform)))))

(define (jshape-add-arg jshape arg)
  (cond ((bbox? arg)
	 ((generic-java-method '|setBoundingBox|) jshape (->jshape arg)))
	((shape? arg)
	 ((generic-java-method '|add|) jshape (->jshape arg)))
	((transform? arg)
	 ((generic-java-method '|transform|) jshape (->jtransform arg)))
	(#t
	 (error 'jshape-add-arg "Unable to add arg to jshape: `~a'" arg))))

(define (->jshape-class shape)
  (cond ((not (shape? shape))
	 (error 'shape-get-constructor "Not a shape: `~a'" shape))
	((union? shape)
	 <traci.model.shape.csg.Union>)
	((difference? shape)
	 <traci.model.shape.csg.Difference>)
	((intersection? shape)
	 <traci.model.shape.csg.Intersection>)
	((sphere? shape)
	 <traci.model.shape.primitive.Sphere>)
	((cylinder? shape)
	 <traci.model.shape.primitive.Cylinder>)
	((box? shape)
	 <traci.model.shape.primitive.Box>)
	((bbox? shape)
	 <traci.model.shape.Bounding-Box>)
	(#t
	 (error '->jclass "Unknown shape type: `~a'" shape))))

(define (->jshape shape)
  (if (not (shape? shape))
      (error '->jshape2 "Not a shape: `~a'" shape)
      (let* ((cargs (list-of-args (shape-get-constructor-arg-list shape)))
	     (jshape (apply java-new (->jshape-class shape) (map ->java cargs))))
	(for-each
	 (lambda (arg) (jshape-add-arg jshape arg))
	 (list-of-args (shape-get-arg-list shape)))
	jshape)))

(define (->java obj)
  (cond ((transform? obj)
	 (->jtransform obj))
	((shape? obj)
	 (->jshape obj))
	((vec? obj)
	 (->jvec obj))
	((number? obj)
	 (->jdouble obj))
	(#t
	 (error '->java "Unable to convert to java object: `~a'" obj))))
