(define (legopart-peg . args)
  (let ((height (* .2 lu))
        (radius (* .3 lu))
        (lower-rounding lego-rounding)
        (upper-rounding (* 2 lego-rounding)))
    (rounded-cylinder-2 radius height lower-rounding upper-rounding
                        (translate lu/2 0 lu/2)
                        (apply insert args))))

(define (legopart-peg-hole . args)
  (let ((height (* .2 lu))
        (radius (* .3 lu))
        (inner-radius (* .2 lu))
        (lower-rounding lego-rounding)
        (upper-rounding (* 2 lego-rounding)))
    (difference
     (rounded-cylinder-2 radius height lower-rounding upper-rounding)
     (rounded-cylinder-2 inner-radius (- height epsilon)
                         upper-rounding lower-rounding
                         (rotx pi) (translate 0 height 0))
     (translate lu/2 0 lu/2)
     (apply insert args))))

(define (legopart-technic-brick-hole . args)
  (let ((radius (* .3 lu))
        (length lu)
        (rounding (* .5 lego-rounding))
        (cw (* .08 lu))
        (cd (* .08 lu))
        (height (* .7 lu)))
    (union
     (rounded-cylinder-3 radius (- length cd cd) rounding (translate 0 cd 0))
     (rounded-cylinder-2 (+ radius cw) cd rounding rounding)
     (rounded-cylinder-2 (+ radius cw) cd rounding rounding
                         (rotx pi) (translate 0 length 0))
     (bbox
      (scale 2 1 2)
      (translate -1 0 -1)
      (scale (+ radius cw rounding)
             (+ length epsilon epsilon)
             (+ radius cw rounding))
      (translate 0 (- epsilon) 0))
     (rotx pi/2)
     (translate lu height 0)
     (apply insert args))))

(define (legopart-technic-plate-hole . args)
  (let ((radius (* .3 lu))
        (height (* .4 lu)))
    (rounded-cylinder-3 radius height lego-rounding
                        (translate lu 0 lu)
                        (apply insert args))))

(define (legopiece-technic-brick-with-holes length . args)
  (let ((box-length (* length lu))
        (box-height (* 1.2 lu))
        (box-width lu)
        (thickness (* .15 lu))
        (peg-height (* .2 lu)))
    (union
     (difference
      (rounded-box box-length box-height box-width lego-rounding)
      (difference
       (rounded-box2 (- box-length (* 2 thickness))
                     (- box-height thickness)
                     (- box-width (* 2 thickness))
                     lego-rounding
                     (translate thickness 0 thickness))
       (loop i 0 (- length 2)
             (cylinder (+ (* .3 lu) (* .5 thickness))
                       (vec lu (* .7 lu) (- thickness epsilon))
                       (vec lu (* .7 lu) (+ (- box-width thickness) epsilon))
                       (translate (* i lu) 0 0))))
      (loop i 0 (- length 2)
            (legopart-technic-brick-hole (translate (* i lu) 0 0))))
     (loop i 0 (- length 2)
           (rounded-cylinder-0 (* .15 lu) (* .35 lu) lego-rounding
                               (scale 1 -1 1)
                               (translate lu (* .35 lu) (* .5 lu))
                               (translate (* i lu) 0 0)))
     (loop i 0 (- length 1)
           (legopart-peg-hole (translate (* i lu) box-height 0)))
     (bbox (scale box-length (+ box-height peg-height) box-width))
     (apply insert args))))

(define (legopiece-plate width length . args)
  (let ((box-length (* length lu))
        (box-height (* .4 lu))
        (box-width (* width lu))
        (peg-height (* .2 lu)))
    (union
     (rounded-box box-length box-height box-width lego-rounding)
     (loop x 0 (- length 1)
           (loop z 0 (- width 1)
                 (legopart-peg (translate (* x lu) box-height (* z lu)))))
     (bbox (scale box-length (+ box-height peg-height) box-width))
     (apply insert args))))

(define (legopiece-technic-plate-with-holes length . args)
  (let ((box-length (* length lu))
        (box-height (* .4 lu))
        (box-width (* 2 lu))
        (peg-height (* .2 lu)))
    (union
     (difference
      (rounded-box box-length box-height box-width lego-rounding)
      (loop x 0 (- length 2)
            (legopart-technic-plate-hole (translate (* x lu) 0 0))))
     (loop x 0 (- length 1)
           (legopart-peg (translate (* x lu) box-height 0))
           (legopart-peg (translate (* x lu) box-height lu)))
     (bbox (scale box-length (+ box-height peg-height) box-width))
     (apply insert args))))