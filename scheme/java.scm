;;; Imports
;;; -------

(import s2j)

;;; Java converter
;;; --------------

(define-java-class <traci.math.Vector>)
(define-java-class <traci.math.Transformations>)
(define-java-class <traci.model.material.Color>)
(define-java-class <traci.model.shape.csg.Union>)
(define-java-class <traci.model.shape.csg.Difference>)
(define-java-class <traci.model.shape.csg.Intersection>)
(define-java-class <traci.model.shape.primitive.Sphere>)
(define-java-class <traci.model.shape.primitive.Cylinder>)
(define-java-class <traci.model.shape.primitive.Box>)
(define-java-class <traci.model.shape.primitive.Torus>)
(define-java-class <traci.model.shape.Bounding-Box>)

(define-generic-java-method <make> |make|)
(define-generic-java-method <translate> |translate|)
(define-generic-java-method <scale> |scale|)
(define-generic-java-method <rotx> |rotx|)
(define-generic-java-method <roty> |roty|)
(define-generic-java-method <rotz> |rotz|)
(define-generic-java-method <setBoundingBox> |setBoundingBox|)
(define-generic-java-method <add> |add|)
(define-generic-java-method <transform> |transform|)
(define-generic-java-method <setColor> |setColor|)

(define <Vector-null> (java-null <traci.math.Vector>))
(define <Color-null> (java-null <traci.model.material.Color>))
(define <Transformations-null> (java-null <traci.math.Transformations>))

(define (->jvec vec)
  (if (not (vec? vec))
      (error '->jvec "Not a vector: `~a'" vec)
      (<make> <Vector-null>
       (->jdouble (vec-x vec))
       (->jdouble (vec-y vec))
       (->jdouble (vec-z vec)))))

(define (->jcolor color)
  (if (not (color? color))
      (error '->jcolor "Not a color: `~a'" color)
      (<make> <Color-null>
       (->jdouble (color-r color))
       (->jdouble (color-g color))
       (->jdouble (color-b color)))))

(define (transform-get-java-method transform)
  (cond ((not (transform? transform))
         (error 'get-java-transform-method "Not a transform: `~a'" transform))
        ((translate? transform) <translate>)
        ((scale? transform) <scale>)
        ((rotx? transform) <rotx>)
        ((roty? transform) <roty>)
        ((rotz? transform) <rotz>)
        (#t
         (error 'get-java-transform-method "Unknown transform type: `~a'" transform))))

(define (->jtransform transform)
  (if (not (transform? transform))
      (error '->jtransform "Not a transform: `~a'" transform)
      ((transform-get-java-method transform) <Transformations-null>
       (->java (transform-get-argument transform)))))

(define (jshape-add-arg jshape arg)
  (cond ((bbox? arg)
         (<setBoundingBox> jshape (->jshape arg)))
        ((shape? arg)
         (<add> jshape (->jshape arg)))
        ((transform? arg)
         (<transform> jshape (->jtransform arg)))
        ((color? arg)
         (<setColor> jshape (->jcolor arg)))
        (#t
         (error 'jshape-add-arg "Unable to add arg to jshape: `~a'" arg))))

(define (->jshape-class shape)
  (cond ((not (shape? shape))
         (error 'shape-get-constructor "Not a shape: `~a'" shape))
        ((union? shape) <traci.model.shape.csg.Union>)
        ((difference? shape) <traci.model.shape.csg.Difference>)
        ((intersection? shape) <traci.model.shape.csg.Intersection>)
        ((sphere? shape) <traci.model.shape.primitive.Sphere>)
        ((cylinder? shape) <traci.model.shape.primitive.Cylinder>)
        ((box? shape) <traci.model.shape.primitive.Box>)
        ((torus? shape) <traci.model.shape.primitive.Torus>)
        ((bbox? shape) <traci.model.shape.Bounding-Box>)
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
        ((color? obj)
         (->jcolor obj))
        ((number? obj)
         (->jdouble obj))
        (#t
         (error '->java "Unable to convert to java object: `~a'" obj))))
