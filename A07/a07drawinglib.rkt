#lang racket

(require 2htdp/image)
(require lang/posn)

;; Uncomment if you want to play with check-expect in this
;; file (but you probably don't)
;(require test-engine/racket-tests)


;; Describe what we allow students to use.
(provide
 save-image
 save-svg-image
 (struct-out prim-triangle)
 (struct-out prim-circle)

 point-x
 point-y
 shape-type
 shape-id
 component-picture
 circle-radius
 circle-color
 triangle-p1
 triangle-p2
 triangle-p3
 triangle-color
 rect-width
 rect-height
 rect-color
 
 render-image)


;; ############ Convenience functions for accessing fields in lists

;; --------------
;; (point-x point) produces the x-coordinate of a Point
;; point-x: Point -> Int
;; Example:
;(check-expect (point-x '(13 74)) 13)

(define (point-x point) (first point))

;; -------------
;; (point-y point) produces the y-coordinate of a Point
;; point-y: Point -> Int
;; Example:
;(check-expect (point-y '(13 74)) 74)

(define (point-y point) (second point))




;; -----------------
;; (shape-type x) produces the type of shape
;; shape-type: Shape -> Sym
;; Example
;(check-expect (shape-type (list 'component 'cloud empty)) 'component)
;(check-expect (shape-type '(circle useless 89 "Pink")) 'circle)

(define (shape-type x) (first x))


;; -----------------
;; (shape-id x) produces the ID of shape
;; shape-id: Shape -> ShapeID
;; Example
;(check-expect (shape-id '(circle useless 50 "Pink")) 'useless)

(define (shape-id x) (second x))


;; -----------------
;; (component-picture c) produces the picture out of c
;; shape-picture: Shape -> Picture
;; requires: Shape is a component (symbol=? (first c) 'component)
;; Examples: 
;(check-expect (component-picture (list 'component 'dumb empty)) empty)

(define (component-picture c) (third c))



;; --------------------------------------
;; (circle-radius x) produces the radius of the given circle
;; circle-radius: Shape -> Nat
;; requires: x is a circle
;; Example:
;(check-expect (circle-radius '(circle useless 20 "Pink")) 20)

(define (circle-radius x) (third x))

;;-----------------
;; (circle-color x) produces the colour of x
;; circle-color: Shape -> ImageColor
;; requires: x is a circle
;; Example:
;(check-expect (circle-color '(circle useless 30 "Pink")) "Pink")

(define (circle-color x) (fourth x))


;; ------------------------
;; (triangle-p1 x) produces the first point of x
;; triangle-p1: Shape -> Offset
;; requires: x is a triangle
;; Example:
;(check-expect (triangle-p1 '(triangle t (4 2) (-4 7) (40 -1) "Gray")) '(4 2))

(define (triangle-p1 x) (third x))


;; ------------------------
;; (triangle-p2 x) produces the second point of x
;; triangle-p2: Shape -> Offset
;; requires: x is a triangle
;; Example:
;(check-expect (triangle-p2 '(triangle t (4 2) (-4 7) (40 -1) "Gray")) '(-4 7))

(define (triangle-p2 x) (fourth x))


;; ------------------------
;; (triangle-p3 x) produces the first point of x
;; triangle-p3: Shape -> Offset
;; requires: x is a triangle
;; Example:
;(check-expect (triangle-p3 '(triangle t (4 2) (-4 7) (40 -1) "Gray")) '(40 -1))

(define (triangle-p3 x) (fifth x))

;; ------------------------
;; (triangle-color x) produces the color of x
;; triangle-color: Shape -> ImageColor
;; requires: x is a triangle
;; Example:
;(check-expect (triangle-color '(triangle t (4 2) (-4 7) (40 -1) "Gray")) "Gray")

(define (triangle-color x) (sixth x))


;; ------------------------
;; (rect-width x) produces the width of x
;; rect-width: Shape -> Nat
;; requires: x is a rectangle
;; Example:
;(check-expect (rect-width '(rectangle r 89 43 "Gray")) 89)

(define (rect-width x) (third x))


;; ------------------------
;; (rect-height x) produces the width of x
;; rect-height: Shape -> Nat
;; requires: x is a rectangle
;; Example:
;(check-expect (rect-height '(rectangle r 89 43 "Gray")) 43)

(define (rect-height x) (fourth x))


;; ------------------------
;; (rect-color x) produces the color of x
;; rect-color: Shape -> ImageColor
;; requires: x is a rectangle
;; Example:
;(check-expect (rect-color '(rectangle r 89 43 "Gray")) "Gray")

(define (rect-color x) (fifth x))



;; ##################### DATA DEFINITIONS 

;; A Point is a (list Int Int)

;; An Offset is a Point

;; A ShapeID is a Sym
;; requires: ShapeID is not 'circle, 'triangle, 'rectangle, 'component

;; An ImageColor is a Str
;; requires: the Str is from the racket/draw colour database:
;; https://docs.racket-lang.org/draw/color-database___.html

;; A Coordinate is a (make-posn Int Int)

;; A Shape is one of:
;; - (list 'circle ShapeID radius ImageColor)
;; - (list 'triangle ShapeID Point Point Point ImageColor)
;; - (list 'rectangle ShapeID width height ImageColor)
;; - (list 'component ShapeID Picture)
;; requires: radius,width,height are Nat

;; A Picture is a (listof (list Offset ShapeID))

;; A ShapeList is a (listof Shape)
;; requires: every ID in the ShapeList is unique

;; A BundledDrawing is a (list width height Picture ShapeList)
;; requires: width, height are Nat
;;    every ShapeID in the picture (recursively) is in ShapeList



;; ################ PRIMITIVES 

;; (Don't worry about the #:transparent)
(define-struct prim-circle (center radius color) #:transparent)
;; A PrimCircle is a (make-prim-circle Coordinate Nat ImageColor)

(define-struct prim-triangle (p1 p2 p3 color) #:transparent)
;; A PrimTriangle is a (make-prim-triangle
;;    Coordinate Coordinate Coordinate Coordinate ImageColor)

;; A PrimElement is (anyof PrimTriangle PrimCircle)

;; --------------------
;; (process-primitives p-shapes scene-sofar) draws the elements of p-shapes
;;   onto scene-sofar.
;; process-primitives: (listof PrimitiveShapes) Image -> Image
;; Produces an error if bad things happen.
(define (process-primitives p-shapes scene-sofar)
  (cond
    [(empty? p-shapes) scene-sofar]
    [(prim-circle? (first p-shapes))
     (place-image/align
      (circle (prim-circle-radius (first p-shapes))
              "solid"
              (prim-circle-color (first p-shapes)))
      (posn-x (prim-circle-center (first p-shapes)))
      (posn-y (prim-circle-center (first p-shapes)))
      'center
      'center
      (process-primitives (rest p-shapes) scene-sofar))]
    [(prim-triangle? (first p-shapes))
     (scene+polygon
      (process-primitives (rest p-shapes) scene-sofar)
      (list (prim-triangle-p1 (first p-shapes))
                     (prim-triangle-p2 (first p-shapes))
                     (prim-triangle-p3 (first p-shapes)))
      "solid"
      (prim-triangle-color (first p-shapes)))]
    [else 
     (error "Unrecognized shape: " (first p-shapes))]))      


;; ---------------------------
;; (render-image canvas-size p-shapes) produces the result
;;    of drawing the elements of p-shapes onto a canvas of canvas-size.
;; render-image: Posn ImageColor (listof PrimitiveShapes) -> Image

(define (render-image canvas-size p-shapes)
  (cond
    [(not (posn? canvas-size))
     (error "Expect a Posn for canvas-size, received:" canvas-size)]
    [(not (and (integer? (posn-x canvas-size)) (<= 0 (posn-x canvas-size))))
     (error "Expect canvas width to be a Nat, received:" (posn-x canvas-size))]
    [(not (and (integer? (posn-y canvas-size)) (<= 0 (posn-y canvas-size))))
     (error "Expect canvas height to be a Nat, received:" (posn-y canvas-size))]    
    [(not (list? p-shapes))
     ;; Should be more specific.
     (error "Expect a list for p-shapes, received:" p-shapes)]
    [else
     (process-primitives p-shapes
                         (empty-scene (posn-x canvas-size)
                                      (posn-y canvas-size)))]))

