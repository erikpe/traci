(define (legopart-peg . args)
  (let ((height .1)
        (radius .15)
        (rounding lego-rounding)
        (upper-rounding .015))
  (rounded-cylinder-2 radius height rounding upper-rounding
                      (translate .25 0 .25)
                      (apply insert args))))

(define (legopart-peg-hole . args)
  (let ((height .1)
        (radius .15)
        (inner-radius .1)
        (rounding lego-rounding)
        (upper-rounding .015))
  (difference
   (rounded-cylinder-2 radius height rounding upper-rounding)
   (rounded-cylinder-2 inner-radius (- height epsilon) upper-rounding rounding
                       (rotx pi) (translate 0 height 0))
   (translate .25 0 .25)
   (apply insert args))))

(define (legopart-technic-brick-hole-new . args)
  (let ((radius .15)
        (length .5)
        (rounding (/ lego-rounding 2))
        (cw .04)
        (cd .04))
    (union
     (rounded-cylinder-3 radius (- length cd cd) rounding
                         (translate 0 cd 0))
     (rounded-cylinder-2 (+ radius cw) cd rounding rounding)
     (rounded-cylinder-2 (+ radius cw) cd rounding rounding
                         (rotx pi) (translate 0 length 0))
     (bbox (scale 2 1 2)
           (translate -1 0 -1)
           (scale (+ radius cw rounding)
                  (+ length epsilon epsilon)
                  (+ radius cw rounding))
           (translate 0 (- epsilon) 0))
     (rotx pi/2)
     (translate .5 .35 0)
     (apply insert args))))

;;(define (legopart-technic-brick-hole-0 radius rounding cw cd . args)
;;  (union
;;    (cylinder (+ radius cw)
;;      (vec 0 0 (- epsilon))
;;      (vec 0 0 (- cd rounding)))
;;    (cylinder (- (+ radius cw) rounding)
;;      (vec 0 0 (- cd rounding epsilon))
;;      (vec 0 0 cd))
;;    (torus rounding (- (+ radius cw) rounding)
;;      (translate 0 0 (- cd rounding)))
;;    (difference
;;      (cylinder (+ radius rounding)
;;        (vec 0 0 (- cd epsilon))
;;        (vec 0 0 (+ cd rounding)))
;;      (torus rounding (+ radius rounding)
;;        (translate 0 0 (+ cd rounding))))
;;    (difference
;;      (cylinder (+ radius cw rounding)
;;        (vec 0 0 (- epsilon))
;;        (vec 0 0 rounding))
;;      (torus rounding (+ radius cw rounding)
;;        (translate 0 0 rounding)))
;;    (apply insert args)))

;;(define (legopart-technic-brick-hole . args)
;;  (let ((length .5)
;;        (radius .15)
;;        (hole-rounding (/ lego-rounding 2))
;;        (cw .04)
;;        (cd .04))
;;    (union
;;      (legopart-technic-brick-hole-0 radius hole-rounding cw cd)
;;      (legopart-technic-brick-hole-0 radius hole-rounding cw cd
;;        (roty pi)
;;        (translate 0 0 length))
;;      (cylinder radius
;;        (vec 0 0 (+ length epsilon))
;;        (vec 0 0 (- epsilon)))
;;      (bbox
;;        (translate -.5 -.5 0)
;;        (scale (* 2 (+ radius cw hole-rounding))
;;               (* 2 (+ radius cw hole-rounding))
;;               length))
;;      (translate .5 .35 0)
;;      (apply insert args))))

(define (legopart-technic-plate-hole . args)
  (let ((radius .15)
        (height .2)
        (rounding lego-rounding))
    (union
      (cylinder radius
        (vec 0 (- epsilon) 0)
        (vec 0 (+ height epsilon) 0))
      (difference
        (cylinder (+ radius rounding)
          (vec 0 (- height rounding) 0)
          (vec 0 (+ height epsilon) 0))
        (torus rounding (+ radius rounding)
          (rotx (/ pi 2))
          (translate 0 (- height rounding) 0)))
      (translate .5 0 .5)
      (apply insert args))))

;;(define (legopiece-technic-brick-with-holes length . args)
;;  (union
;;    (difference
;;      (rounded-box (/ length 2.0) .6 .5 lego-rounding)
;;      (loop i 0 (- length 2)
;;        (legopart-technic-brick-hole (translate (* i .5) 0 0))))
;;    (loop i 0 (- length 1)
;;      (legopart-peg-hole (translate (* i .5) .6 0)))
;;    (bbox (scale (/ length 2.0) .7 .5))
;;    (apply insert args)))

(define (legopiece-technic-brick-with-holes length . args)
  (union
   (difference
    (rounded-box (/ length 2.0) .6 .5 lego-rounding)
    (loop i 0 (- length 2)
          (legopart-technic-brick-hole-new (translate (* i .5) 0 0))))
   (loop i 0 (- length 1)
         (legopart-peg-hole (translate (* i .5) .6 0)))
   (bbox (scale (/ length 2.0) .7 .5))
   (apply insert args)))

(define (legopiece-plate width length . args)
  (union
    (rounded-box (/ length 2.0) .2 (/ width 2.0) lego-rounding)
    (loop x 0 (- length 1)
      (loop y 0 (- width 1)
        (legopart-peg (translate (* x .5) .2 (* y .5)))))
    (bbox (scale (/ length 2.0) .3 (/ width 2.0)))
    (apply insert args)))

(define (legopice-technic-plate-with-holes width length . args)
  (difference
    (legopiece-plate width length)
    (loop x 0 (- length 2)
      (legopart-technic-plate-hole (translate (* x .5) 0 0)))
    (apply insert args)))

(define (legopiece-technic-pin . args)
  (let ((radius .15)
        (middle-radius .19)
        (middle-length .04)
        (inner-radius .11)
        (stopping-radius .02)
        (opening-width .05)
        (opening-depth .2))
    (difference
      (union
        (cylinder radius
          (vec 0 0 (- .5))
          (vec 0 0 .5))
        (cylinder middle-radius
          (vec 0 0 (/ middle-length 2))
          (vec 0 0 (- (/ middle-length 2))))
        (torus stopping-radius radius
          (translate 0 0 (- .5 stopping-radius)))
        (torus stopping-radius radius
          (translate 0 0 (- stopping-radius .5))))
      (cylinder inner-radius
        (vec 0 0 (- 0.0 .5 epsilon))
        (vec 0 0 (+ .5 epsilon)))
      (cylinder (/ opening-width 2.0)
        (vec (- 0.0 radius epsilon) 0 0)
        (vec (+ radius epsilon) 0 0)
        (translate 0 0 (- .5 opening-depth)))
      (box
        (translate -.5 -.5 0)
        (scale 2 1 1)
        (scale
          (+ radius stopping-radius epsilon)
          opening-width
          (+ opening-depth epsilon))
        (translate 0 0 (- .5 opening-depth)))
      (cylinder (/ opening-width 2.0)
        (vec (- 0.0 radius epsilon) 0 0)
        (vec (+ radius epsilon) 0 0)
        (translate 0 0 (- opening-depth .5)))
      (box
        (translate -.5 -.5 -1)
        (scale 2 1 1)
        (scale
          (+ radius stopping-radius epsilon)
          opening-width
          (+ opening-depth epsilon))
        (translate 0 0 (- opening-depth .5)))
      (bbox
        (translate -.5 -.5 -.5)
        (scale 2 2 2)
        (scale middle-radius middle-radius .5))
      (translate .5 .35 0)
      (apply insert args))))
