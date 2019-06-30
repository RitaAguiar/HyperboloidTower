#lang racket

(require rosetta/autocad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINITIONS

(define p (u0)) ;tower base point
(define r 0.1) ;structural cylindrical columns radius
(define rb 5.3) ;tower base radius
(define rt 3) ;tower top radius
(define dfi (/ pi 2)) ;structural cylindrical columns angle with top/base of the tower
(define n-cyl 12) ;number of facade cylinders
(define h-slab 0.3) ;height of the slabs
(define h 20) ;total height of the tower 
(define n-step 19) ;number of steps per floor
(define t-step 0.2) ;steps thickness
(define h-floor (* t-step n-step)) ;floors height
(define n-floors (exact-floor (/ (- h h-floor) h-floor))) ;number of stair floors/platforms
(define h-rail 1) ;handrail height
(define r-rail 0.01) ;handrail radius
(define l 3) ;steps length
(define w 0.6) ;steps width
(define alfa 0) ;stairs start angle
(define dalfa (/ pi 18)) ;angle between steps

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;LAYERS

(define slab-layer (create-layer "Slab"))
(define stair-layer (create-layer "Stair"))
(define column-layer (create-layer "Column"))
(define structure-layer (create-layer "Structure"))
(define platform-layer (create-layer "Platform"))
(define posts-layer (create-layer "Posts"))
(define person-layer (create-layer "Person"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FUNCTIONS

;structural slab

(define (slab p r h-slab)
  (with-current-layer slab-layer
    (cylinder p r (+z p h-slab))))

;cylindrical structural column

(define (column p w h h-slab)
  (with-current-layer column-layer
    (cylinder p (* 2 (/ w 3)) (+z p (- h h-slab)))))

;facade cylindrical columns

(define (hyperboloid-tower r rb rt h dfi n-cyl)
  (define (cylinders-pair fi)
    (with-current-layer structure-layer
      (cylinder (+pol (u0) rb fi) r (+cyl (u0) rt (+ fi dfi) h))
      (cylinder (+pol (u0) rb fi) r (+cyl (u0) rt (- fi dfi) h))))
  (map cylinders-pair
       (division 0 2pi n-cyl #f)))

;spiral steps of the stairs

(define (spiral-stair p l w t-step alfa dalfa n-step n-floors n-floor)
  (define (stair-rec z-d alfa-d c-d)
    (define pt-i (+z p z-d))
    (with-current-layer stair-layer
      (right-cuboid pt-i w t-step (+pol pt-i c-d alfa-d))))
  (map stair-rec
       (division (/ t-step 2) (* t-step (- n-step .5)) (- n-step 1))
       (division alfa (+ alfa (* dalfa (- n-step 1))) (- n-step 1))
       (division (/ (* l (+ (- n-floors n-floor) 2)) n-floors)
                 (/ (* l (+ (- n-floors n-floor) 1)) n-floors)
                 (- n-step 1))))

;platforms of the stairs

(define (platform p l t-step h-slab n-step n-floor)
  (define h-floor (* t-step n-step))
  (define l-plat (/ (* l (+ (- n-floors n-floor) 1)) n-floors))
  (with-current-layer platform-layer
    (subtraction
     (cylinder (+z p h-floor) l-plat (+z p (+ h-floor h-slab)))
     (box (+xz p (- l-plat) h-floor) (+xyz p l-plat l-plat (+ h-floor h-slab))))))

;handrails of the stairs

(define (handrail p r z h-rail n-floors n-floor)
  (define (pt ang z r-cir)
    #;(sphere (+cyl p r ang z) 0.05)
    #;(+cyl p r ang z)
    (with-current-layer posts-layer
      (cylinder (+cyl p r-cir ang (+ z (/ t-step 2)))
                r-rail
                (+cyl p r-cir ang (+ z h-rail)))
      (cylinder (+cyl p r-cir ang (+ z h-rail))
                r-rail
                (+cyl p r-cir (+ ang dalfa) (+ z h-rail))))
    #;(printf "ang = ~a\n" ang)
    #;(printf "z = ~a\n" z))
  (map pt
       (division alfa (- (* n-step dalfa) dalfa) (- n-step 1))
       (division h-slab (- (+ z h-slab) t-step) (- n-step 1))
       (division (/ (* l (+ (- n-floors n-floor) 2)) n-floors)
                 (/ (* l (+ (- n-floors n-floor) 1)) n-floors)
                 (- n-step 1)))
  (map pt
       (division (- (* n-step dalfa) dalfa)
                 (* 2 (- (* n-step dalfa) dalfa))
                 (- n-step 1) #f)
       (division (- (+ h-floor h-slab) t-step)
                 (- (+ h-floor h-slab) t-step)
                 (- n-step 1) #f)
       (division (/ (* l (+ (- n-floors n-floor) 1)) n-floors)
                 (/ (* l (+ (- n-floors n-floor) 1)) n-floors)
                 (- n-step 1) #f)))

;top handrail of the stairs

(define (top-handrail p ang l-floor h-rail n-floors n-floor)
  (define dist (* 2 (sin (/ dalfa 2))))
  (define l-rail (+ l-floor h-slab (- (/ t-step 2))))
  (define l-t-rail (+ l-floor h-rail (/ t-step 2)))
  (define (pt r)
    (with-current-layer posts-layer
      (cylinder (+cyl p r ang l-rail)
                r-rail
                (+cyl p r ang l-t-rail))
      (cylinder (+cyl p r ang l-t-rail)
                r-rail
                (+cyl p (- r (* (/ (* l (+ (- n-floors n-floor) 2)) n-floors) dist)) ang l-t-rail))))
  (map pt
       (division 0 (/ (* l (+ (- n-floors n-floor) 2)) n-floors)
                 (exact-round (/ 1 dist)))))

;recursion of the stairs, platflorms, and handrails

(define (stair p l w t-step h-slab alfa dalfa n-step n-floors)
  (define h-floor (* t-step n-step))
  (define (stair-rec n-floor)
    (spiral-stair (+z p (+ h-slab (* h-floor n-floor)))
                    l w t-step alfa dalfa n-step n-floors n-floor))
  (map stair-rec
       (division 0 n-floors n-floors #f))
  (define (platform-rec n-floor)
    (platform (+z p (* h-floor n-floor))
             l t-step h-slab n-step n-floor))
  (map platform-rec
       (division 0 n-floors n-floors #f))
  (define (handrail-rec n-floor)
    (handrail (+z p (* h-floor n-floor))
            l h-floor h-rail n-floors n-floor))
  (map handrail-rec
       (division 0 n-floors n-floors #f))
  (top-handrail p alfa (* n-floors h-floor) h-rail n-floors n-floors))

;tower

(define (tower)
  (define h-floor (* t-step n-step))
  (slab p rb h-slab)
  (column p w h h-slab)
  (stair p l w t-step h-slab alfa dalfa n-step n-floors)
  (slab (+z p (- h h-slab)) rt h-slab)
  (hyperboloid-tower r rb rt h dfi n-cyl))

;people

(define (person p)
  (with-current-layer person-layer
    (cone-frustum p 0.15 (+z p 0.2) 0.1)
    (cone-frustum (+z p 0.2) 0.1 (+z p 1.45) 0.2)
    (cylinder (+cyl p 0.225 0 0.8) 0.04 (+cyl p 0.225 0 1.4))
    (cylinder (+cyl p 0.225 pi 0.8) 0.04 (+cyl p 0.225 pi 1.4))
    (sphere (+z p 1.6) 0.125)))

(define (people)
  (person (xyz (/ l 2) (/ l 2) (+ (* 6 t-step) (+ h-slab h-floor))))
  (person (xyz (/ l -3) (/ l -3) (+ h-slab (* (/ n-floors 2) h-floor))))
  (person (xyz (/ l 4) (/ l -4) (+ h-slab (* n-floors h-floor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;EXECUTIONS

(delete-all-shapes)
(tower)
(people)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RENDER

(render-dir "D:\\Rita\\ADA\\Torre")
(render-size 3840 2160)

(view
  (xyz -5.2432 -86.6265 9.9545)
  (xyz -5.2432 37.7816 9.9545)
  65.0)

(render-view "HyperboloidTower")
