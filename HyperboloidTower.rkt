#lang racket

(require rosetta/autocad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINIÇÕES

(define p (u0)) ;ponto base da torre
(define r 0.1) ;raio dos cilindros estruturais
(define rb 5.3) ;raio da base da torre
(define rt 3) ;raio do topo da torre
(define dfi (/ pi 2)) ;ângulo de diferença entre o topo e a base de cada cilindro estrutural
(define n-cil 12) ;número de cilindros
(define h-laje 0.3) ;altura da laje
(define h 20) ; altura da torre
(define n-deg 19) ;número de degraus por piso
(define e-deg 0.2) ;espessura do degrau
(define h-piso (* e-deg n-deg)) ;altura de cada piso
(define n-pisos (exact-floor (/ (- h h-piso) h-piso))) ;número de pisos/plataformas da escada
(define h-gua 1) ;altura da guarda
(define r-gua 0.01) ;raio da guarda
(define c 3) ;comprimento do degrau
(define l 0.6) ;largura do degrau
(define alfa 0) ;ângulo onde começa a escada
(define dalfa (/ pi 18)) ;ângulo entre os degraus

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

;FUNÇÕES

;faz a laje estrutural

(define (laje p r h-laje)
  (with-current-layer slab-layer
    (cylinder p r (+z p h-laje))))

;faz a coluna cilíndrica estrutural

(define (coluna p l h h-laje)
  (with-current-layer column-layer
    (cylinder p (* 2 (/ l 3)) (+z p (- h h-laje)))))

;faz os cilindros estruturais da torre

(define (torre-hiperboloide r rb rt h dfi n-cil)
  (define (par-cilindros fi)
    (with-current-layer structure-layer
      (cylinder (+pol (u0) rb fi) r (+cyl (u0) rt (+ fi dfi) h))
      (cylinder (+pol (u0) rb fi) r (+cyl (u0) rt (- fi dfi) h))))
  (map par-cilindros
       (division 0 2pi n-cil #f)))

;faz os degraus/cuboids das escadas com diferentes dimensões

(define (escada-caracol p c l e-deg alfa dalfa n-deg n-pisos n-piso)
  (define (escada-rec z-d alfa-d c-d)
    (define pt-i (+z p z-d))
    (with-current-layer stair-layer
      (right-cuboid pt-i l e-deg (+pol pt-i c-d alfa-d))))
  (map escada-rec
       (division (/ e-deg 2) (* e-deg (- n-deg .5)) (- n-deg 1))
       (division alfa (+ alfa (* dalfa (- n-deg 1))) (- n-deg 1))
       (division (/ (* c (+ (- n-pisos n-piso) 2)) n-pisos)
                 (/ (* c (+ (- n-pisos n-piso) 1)) n-pisos)
                 (- n-deg 1))))

;faz os patamares das escadas com diferentes dimensões

(define (patamar p c e-deg h-laje n-deg n-piso)
  (define h-piso (* e-deg n-deg))
  (define c-pat (/ (* c (+ (- n-pisos n-piso) 1)) n-pisos))
  (with-current-layer platform-layer
    (subtraction
     (cylinder (+z p h-piso) c-pat (+z p (+ h-piso h-laje)))
     (box (+xz p (- c-pat) h-piso) (+xyz p c-pat c-pat (+ h-piso h-laje))))))

;faz a guarda das escadas

(define (guarda p r z h-gua n-pisos n-piso)
  (define (pt ang z r-cir)
    #;(sphere (+cyl p r ang z) 0.05)
    #;(+cyl p r ang z)
    (with-current-layer posts-layer
      (cylinder (+cyl p r-cir ang (+ z (/ e-deg 2)))
                r-gua
                (+cyl p r-cir ang (+ z h-gua)))
      (cylinder (+cyl p r-cir ang (+ z h-gua))
                r-gua
                (+cyl p r-cir (+ ang dalfa) (+ z h-gua))))
    #;(printf "ang = ~a\n" ang)
    #;(printf "z = ~a\n" z))
  (map pt
       (division alfa (- (* n-deg dalfa) dalfa) (- n-deg 1))
       (division h-laje (- (+ z h-laje) e-deg) (- n-deg 1))
       (division (/ (* c (+ (- n-pisos n-piso) 2)) n-pisos)
                 (/ (* c (+ (- n-pisos n-piso) 1)) n-pisos)
                 (- n-deg 1)))
  (map pt
       (division (- (* n-deg dalfa) dalfa)
                 (* 2 (- (* n-deg dalfa) dalfa))
                 (- n-deg 1) #f)
       (division (- (+ h-piso h-laje) e-deg)
                 (- (+ h-piso h-laje) e-deg)
                 (- n-deg 1) #f)
       (division (/ (* c (+ (- n-pisos n-piso) 1)) n-pisos)
                 (/ (* c (+ (- n-pisos n-piso) 1)) n-pisos)
                 (- n-deg 1) #f)))

;faz a guarda final das escadas

(define (guarda-topo p ang c-piso h-gua n-pisos n-piso)
  (define dist (* 2 (sin (/ dalfa 2))))
  (define c-gua (+ c-piso h-laje (- (/ e-deg 2))))
  (define c-t-gua (+ c-piso h-gua (/ e-deg 2)))
  (define (pt r)
    (with-current-layer posts-layer
      (cylinder (+cyl p r ang c-gua)
                r-gua
                (+cyl p r ang c-t-gua))
      (cylinder (+cyl p r ang c-t-gua)
                r-gua
                (+cyl p (- r (* (/ (* c (+ (- n-pisos n-piso) 2)) n-pisos) dist)) ang c-t-gua))))
  (map pt
       (division 0 (/ (* c (+ (- n-pisos n-piso) 2)) n-pisos)
                 (exact-round (/ 1 dist)))))

;faz as escadas e os patamares com diferentes dimensões segundo o número de pisos

(define (escada p c l e-deg h-laje alfa dalfa n-deg n-pisos)
  (define h-piso (* e-deg n-deg))
  (define (escada-rec n-piso)
    (escada-caracol (+z p (+ h-laje (* h-piso n-piso)))
                    c l e-deg alfa dalfa n-deg n-pisos n-piso))
  (map escada-rec
       (division 0 n-pisos n-pisos #f))
  (define (patamar-rec n-piso)
    (patamar (+z p (* h-piso n-piso))
             c e-deg h-laje n-deg n-piso))
  (map patamar-rec
       (division 0 n-pisos n-pisos #f))
  (define (guarda-rec n-piso)
    (guarda (+z p (* h-piso n-piso))
            c h-piso h-gua n-pisos n-piso))
  (map guarda-rec
       (division 0 n-pisos n-pisos #f))
  (guarda-topo p alfa (* n-pisos h-piso) h-gua n-pisos n-pisos))

;define a torre com todos os seus elementos

(define (torre)
  (define h-piso (* e-deg n-deg))
  (laje p rb h-laje)
  (coluna p l h h-laje)
  (escada p c l e-deg h-laje alfa dalfa n-deg n-pisos)
  (laje (+z p (- h h-laje)) rt h-laje)
  (torre-hiperboloide r rb rt h dfi n-cil))

;pessoas

(define (pessoa p)
  (with-current-layer person-layer
    (cone-frustum p 0.15 (+z p 0.2) 0.1)
    (cone-frustum (+z p 0.2) 0.1 (+z p 1.45) 0.2)
    (cylinder (+cyl p 0.225 0 0.8) 0.04 (+cyl p 0.225 0 1.4))
    (cylinder (+cyl p 0.225 pi 0.8) 0.04 (+cyl p 0.225 pi 1.4))
    (sphere (+z p 1.6) 0.125)))

(define (pessoas)
  (pessoa (xyz (/ c 2) (/ c 2) (+ (* 6 e-deg) (+ h-laje h-piso))))
  (pessoa (xyz (/ c -3) (/ c -3) (+ h-laje (* (/ n-pisos 2) h-piso))))
  (pessoa (xyz (/ c 4) (/ c -4) (+ h-laje (* n-pisos h-piso)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;EXECUÇÕES

; (delete-all-shapes)
; (torre)
; (pessoas)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RENDER

(render-dir "D:\\Rita\\ADA\\Torre")
;(render-size 1920 1080)
(render-size 3840 2160)

; ;frente
; (view
;   (xyz 0 -55.4129 14.3765)
;   (xyz 0 -3.99189 9.57781)
;   65.0)

; ;trás
; (view
;  (xyz 0.0 55.8641 10.0)
;  (xyz 0.0 0.0 10.0)
;  65.0)

; ;Bosque-2
; (view
;   (xyz -44.7577 -90.8815 76.6063)
;   (xyz -7.77706 2.53597 8.84701)
;   65.0)

; ;Bosque-3
; (view
;   (xyz 0.0 -84.9649 4.30138)
;   (xyz 0.0 37.7408 11.0359)
;   65.0)

;Bosque-5
(view
  (xyz -5.2432 -86.6265 9.9545)
  (xyz -5.2432 37.7816 9.9545)
  65.0)
(render-view "Torre-12")


