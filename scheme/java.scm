;;; Imports
;;; -------

(import s2j)

;;; Java converter
;;; --------------

(define-java-class <traci.math.Vector>)
(define-java-class <traci.math.Transformation>)

(define-java-class <traci.model.shape.csg.Union>)
(define-java-class <traci.model.shape.csg.Difference>)
(define-java-class <traci.model.shape.csg.Intersection>)
(define-java-class <traci.model.shape.primitive.Sphere>)
(define-java-class <traci.model.shape.primitive.Cylinder>)
(define-java-class <traci.model.shape.primitive.Box>)

(define (->jvec vec)
  (if (not (vec? vec))
      (error '->jvec "Not a vector: `~a'" vec)
      ((generic-java-method '|make|)
       (java-null <traci.math.Vector>)
       (->jdouble (vec-x vec))
       (->jdouble (vec-y vec))
       (->jdouble (vec-z vec)))))

(define (get-java-transform-method transform)
  (cond ((not (transform? transform))
	 (error 'get-java-transform-method "Not a transform: `~a'" transform))
	((translate? transform)
	 (generic-java-method '|translate|))
	((scale? transform)
	 (generic-java-method '|scale|))
	((rotx? transform)
	 (generic-java-method '|rotateX|))
	((roty? transform)
	 (generic-java-method '|rotateY|))
	((rotz? transform)
	 (generic-java-method '|rotateZ|))
	(#t
	 (error 'get-java-transform-method "Unknown transform type: `~a'" transform))))

(define (->jtransform transform)
  (if (not (transform? transform))
      (error '->jtransform "Not a transform: `~a'" transform)
      ((get-java-transform-method transform)
       (java-null <traci.math.Transformation>)
       (->java (transform-get-argument transform)))))

(define (->jshape/empty shape)
  (let ((variant (shape-variant shape)))
    (cond ((union? shape)
	   (java-new <traci.model.shape.csg.Union>))
	  ((difference? shape)
	   (java-new <traci.model.shape.csg.Difference>))
	  ((intersection? shape)
	   (java-new <traci.model.shape.csg.Intersection>))
	  ((sphere? shape)
	   (java-new <traci.model.shape.primitive.Sphere>))
	  ((cylinder? shape)
	   (java-new <traci.model.shape.primitive.Cylinder>))
	  ((box? shape)
	   (java-new <traci.model.shape.primitive.Box>))
	  (#t
	   (error 'shape->java/empty "Unknown shape type: `~a'" shape)))))

(define (jshape-add-shape jshape shape)
  (if (not (shape? shape))
      (error 'jshape-add-shape "Argument 2 not a shape: `~a'" shape)
      ((generic-java-method '|add|)
       jshape
       (->jshape shape))))

(define (jshape-add-transform jshape transform)
  (if (not (transform? transform))
      (error 'jshape-add-transform "Argument 2 not a transform: `~a'" transform)
      ((generic-java-method '|transform|)
       jshape
       (->jtransform transform))))

(define (jshape-add-arg jshape arg)
  (cond ((transform? arg)
	 (jshape-add-transform jshape arg))
	((shape? arg)
	 (jshape-add-shape jshape arg))
	(#t
	 (error 'jshape-add-arg "Unable to add arg to jshape: `~a'" arg))))

(define (jshape-add-arg-list jshape arg-list)
  (cond ((not (arg-list? arg-list))
	 (error 'jshape-add-arg-list "Argument 2 not an arg-list: `~a'" arg-list))
	((not (arg-list-empty? arg-list))
	 (jshape-add-arg jshape (arg-list-first arg-list))
	 (jshape-add-arg-list jshape (arg-list-rest arg-list)))))

(define (->jshape shape)
  (if (not (shape? shape))
      (error '->jshape "Not a shape: `~a'" shape)
      (let ((jshape (->jshape/empty shape)))
	(jshape-add-arg-list jshape (shape-get-arg-list shape))
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