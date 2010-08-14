(define (legopiece-technic-pin-0 length . args)
  (let ((radius (* .3 lu))
        (outer-stopper-radius (* .04 lu))
        (opening-width (* .1 lu))
        (opening-depth (* .4 lu)))
    (difference
     (union
      (rounded-cylinder-4 radius length lego-rounding (rotx pi/2))
      (torus outer-stopper-radius radius
             (translate 0 0 (- length outer-stopper-radius))))
     (cylinder (* .5 opening-width)
               (vec (- 0.0 radius epsilon) 0 (- length opening-depth))
               (vec (+ radius epsilon) 0 (- length opening-depth)))
     (box
      (translate -.5 -.5 0)
      (scale (* 2 (+ radius outer-stopper-radius epsilon))
             opening-width
             (+ opening-depth epsilon))
      (translate 0 0 (- length opening-depth)))
     (apply insert args))))

(define (legopiece-technic-pin . args)
  (let ((middle-stopper-radius (* .38 lu))
        (middle-stopper-length (* .15 lu))
        (inner-radius (* .22 lu)))
    (difference
     (union
      (legopiece-technic-pin-0 (- lu (* .5 middle-stopper-length))
                               (translate 0 0 (* .5 middle-stopper-length)))
      (legopiece-technic-pin-0 (- lu (* .5 middle-stopper-length))
                               (rotx pi)
                               (translate 0 0 (* -.5 middle-stopper-length)))
      (rounded-cylinder-1 middle-stopper-radius middle-stopper-length lego-rounding
                          (rotx pi/2)
                          (translate 0 0 (* -.5 middle-stopper-length))))
     (rounded-cylinder-3 inner-radius (* 2 lu) lego-rounding
                         (translate 0 (- lu) 0)
                         (rotx pi/2))
     (bbox
      (scale 2 2 2)
      (translate -1 -1 -1)
      (scale middle-stopper-radius middle-stopper-radius lu))
     (translate lu (* .7 lu) 0)
     (apply insert args))))
