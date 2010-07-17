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
      (error 'vec->java "Not a vector: `~a'" vec)
      ((generic-java-method '|make|)
       (java-null <traci.math.Vector>)
       (->jdouble (vec-x vec))
       (->jdouble (vec-y vec))
       (->jdouble (vec-z vec)))))

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

;(define (->jshape shape)
;  (if (not (shape? shape))
;      (error 'shape->java "Not a shape: `~a'" shape)
;      (let ((java-shape (shape->java/empty) shape))
;	(for-each
;	 java-shape))))

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
  (cond ((not (transform? transform))
	 (error 'transform->java "Not a transform: `~a'" transform))
	((vector-transform? transform)
	 ((get-java-transform-method (transform-variant transform))
	  (vec->java (transform-argument transform))))
	((scalar-transform? transform)
	 ((get-java-transform-method (transform-variant transform))
	  (->jdouble (transform-argument transform))))))
	(#t
	 (error 'transform->java "Unknown transform type: `~a'" transform))))
