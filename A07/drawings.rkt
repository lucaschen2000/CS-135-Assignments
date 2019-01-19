;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname drawings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 07, Question 2
;; *****************************************
;;

(require "a07drawinglib.rkt")

;; ==== Question 2a ========================

(define prim-picture
  (list
   (make-prim-circle (make-posn 64 227) 8.82 "Black")
   (make-prim-circle (make-posn 150 150) 70.46 "MistyRose")
   (make-prim-circle (make-posn 88 212) 39 "MistyRose")
   (make-prim-circle (make-posn 110 80) 28 "MistyRose")
   (make-prim-circle (make-posn 189 79) 28 "MistyRose")
   (make-prim-triangle
    (make-posn 210 150) (make-posn 230 170) (make-posn 229 129) "MistyRose")
   (make-prim-triangle
    (make-posn 80 248) (make-posn 100 248) (make-posn 89 277) "MistyRose")
   (make-prim-triangle
    (make-posn 53 216) (make-posn 39 208) (make-posn 53 200) "Black")))


;; ==== Question 2b ========================

;; (rect offset rectangle-dimensions color) outputs two triangles,
;;   which construct a rectangle when placed on top of one another
;; rect: Coordinate Coordinate ImageColor -> (list PrimTriangle PrimTriangle)
;; requires: the parameters of rectangle-dimensions must both be > 0
;; Examples:
(check-expect (rect (make-posn 0 0) (make-posn 50 100) "Blue")
              (list (make-prim-triangle
                     (make-posn 0 0) (make-posn 50 0)
                     (make-posn 50 100) "Blue")
                    (make-prim-triangle
                     (make-posn 0 0) (make-posn 50 100)
                     (make-posn 0 100) "Blue")))

(define (rect offset rectangle-dimensions color)
  (list (make-prim-triangle (make-posn (posn-x offset) (posn-y offset))
                            (make-posn (+ (posn-x rectangle-dimensions)
                                          (posn-x offset))
                                       (posn-y offset))
                            (make-posn (+ (posn-x rectangle-dimensions)
                                          (posn-x offset))
                                       (+ (posn-y rectangle-dimensions)
                                          (posn-y offset)))
                            color)
        (make-prim-triangle (make-posn (posn-x offset) (posn-y offset))
                            (make-posn (+ (posn-x rectangle-dimensions)
                                          (posn-x offset))
                                       (+ (posn-y rectangle-dimensions)
                                          (posn-y offset)))
                            (make-posn (posn-x offset)
                                       (+ (posn-y rectangle-dimensions)
                                          (posn-y offset)))
                            color)))

;; Tests:
(check-expect (rect (make-posn 0 0) (make-posn 1 1) "Black")
              (list (make-prim-triangle
                     (make-posn 0 0) (make-posn 1 0)
                     (make-posn 1 1) "Black")
                    (make-prim-triangle
                     (make-posn 0 0) (make-posn 1 1)
                     (make-posn 0 1) "Black")))

(check-expect (rect (make-posn -1 1) (make-posn 2 2) "Brown")
              (list (make-prim-triangle
                     (make-posn -1 1) (make-posn 1 1)
                     (make-posn 1 3) "Brown")
                    (make-prim-triangle
                     (make-posn -1 1) (make-posn 1 3)
                     (make-posn -1 3) "Brown")))

(check-expect (rect (make-posn 20 30) (make-posn 23 54) "Red")
              (list (make-prim-triangle
                     (make-posn 20 30) (make-posn 43 30)
                     (make-posn 43 84) "Red")
                    (make-prim-triangle
                     (make-posn 20 30) (make-posn 43 84)
                     (make-posn 20 84) "Red")))

(check-expect (rect (make-posn -20 -30) (make-posn 23 54) "White")
              (list (make-prim-triangle
                     (make-posn -20 -30) (make-posn 3 -30)
                     (make-posn 3 24) "White")
                    (make-prim-triangle
                     (make-posn -20 -30) (make-posn 3 24)
                     (make-posn -20 24) "White")))


;; ==== Question 2c ========================

;; Assignment examples
(define summer-shapes '((circle top-scoop 10 "Pink")
                        (circle bottom-scoop 10 "LightBlue")
                        (component ice-cream
                                   (((0 40) cone)
                                    ((10 35) bottom-scoop)
                                    ((10 25) top-scoop)))
                        (triangle cone (0 0) (20 0) (10 50) "Burlywood")
                        (circle moon 35 "Light Gray")
                        (circle sun 40 "Yellow")))

(define ice-cream-pic '(((10 50) ice-cream)
                        ((70 20) sun)
                        ((130 30) ice-cream)))

(define ice-cream-drawing (list 200 150 ice-cream-pic summer-shapes))

;; Lucas defined
(define fun-shapes '((circle wheel1 10 "Black")
                     (circle wheel2 15 "Black")
                     (component car
                                (((20 10) wheel1)
                                 ((70 10) wheel2)
                                 ((20 40) car-body)))
                     (circle cloud 50 "White")
                     (component car-body
                                (((20 20) door1)
                                 ((70 20) door2)
                                 ((30 35) window)))
                     (rectangle door1 20 30 "Black")
                     (rectangle door2 20 30 "Black")
                     (rectangle window 20 10 "Blue")))

(define fun-pic '(((10 30) car)
                  ((80 30) cloud)
                  ((70 30) car)))

(define fun-drawing (list 200 600 fun-pic fun-shapes))


;; ==== Question 2d ========================

;; (get-component-ids picture) outputs a list of every shape-id that occurs
;;   in picture
;; get-component-ids: Picture -> (listof ShapeID)
;; Examples:
(check-expect (get-component-ids ice-cream-pic)
              '(ice-cream sun ice-cream))

(define (get-component-ids picture)
  (cond [(empty? picture) empty]
        [else (cons (shape-id (first picture))
                    (get-component-ids (rest picture)))]))

;; (shapeid-in-shapelist shapeid shapelist parent-shapelist) returns empty
;;   if shapeid isn't in shapelist or creates list containing shapeid and
;;   the shape ids of its components, parent-shapelist retains full shapelist
;; shapeid-in-shapelist: ShapeID ShapeList ShapeList -> (listof ShapeID)
;; Examples:
(check-expect (shapeid-in-shapelist 'top-scoop summer-shapes summer-shapes)
              (list 'top-scoop))

(define (shapeid-in-shapelist shapeid shapelist parent-shapelist)
  (cond [(empty? shapelist) empty]
        [(not (symbol=? shapeid (shape-id (first shapelist))))
         (shapeid-in-shapelist shapeid (rest shapelist) parent-shapelist)]
        [(symbol=? (shape-type (first shapelist)) 'component)
         (append (cons (shape-id (first shapelist))
                       (get-component-ids
                        (component-picture (first shapelist))))
                 (get-picture-ids/duplicates
                  (component-picture (first shapelist)) parent-shapelist))]
        [else (list (shape-id (first shapelist)))]))

;; (get-picture-ids/duplicates picture shapelist) produces a list of ShapeIDs
;;   from shapelist that occur in picture along with their components,
;;   with duplicates
;; get-picture-ids/duplicates: Picture ShapeList -> (listof ShapeID)
;; Examples:
(check-expect (get-picture-ids/duplicates ice-cream-pic summer-shapes)
              '(ice-cream cone bottom-scoop top-scoop
                          cone bottom-scoop top-scoop sun ice-cream
                          cone bottom-scoop top-scoop
                          cone bottom-scoop top-scoop))

(define (get-picture-ids/duplicates picture shapelist)
  (cond [(empty? picture) empty]
        [else (append (shapeid-in-shapelist (second (first picture))
                                            shapelist shapelist)
                      (get-picture-ids/duplicates (rest picture) shapelist))]))

;; (unduplicate-list list) returns all items in list, without duplicates
;; unduplicate: (listof Any) -> (listof Any)
;; Examples:
(check-expect (unduplicate-list '(0 0 1 2 3 4 4 5 5 6 7 8 8 9 10 10))
              '(0 1 2 3 4 5 6 7 8 9 10))

(define (unduplicate-list list)
  (cond [(empty? list) empty]
        [(member? (first list) (rest list)) 
         (unduplicate-list (rest list))]
        [else (cons (first list) (unduplicate-list (rest list)))]))

;; (get-picture-ids picture shapelist) produces a list of ShapeIDs
;;   from shapelist that occur in picture along with their components,
;;   without duplicates
;; get-picture-ids: Picture ShapeList -> (listof ShapeID)
;; Examples:
(check-expect (get-picture-ids ice-cream-pic summer-shapes)
              '(sun ice-cream cone bottom-scoop top-scoop))

(define (get-picture-ids picture shapelist)
  (unduplicate-list (get-picture-ids/duplicates picture shapelist)))

;; Tests:
(check-expect (get-picture-ids empty empty) empty)
(check-expect (get-picture-ids empty fun-shapes) empty)
(check-expect (get-picture-ids fun-pic empty) empty)
(check-expect (get-picture-ids fun-pic fun-shapes)
              '(cloud car wheel1 wheel2 car-body door1 door2 window))
(check-expect (get-picture-ids fun-pic summer-shapes) empty)
(check-expect (get-picture-ids ice-cream-pic fun-shapes) empty)
(check-expect (get-picture-ids '(((70 20) sun)) summer-shapes)
              '(sun))
(check-expect (get-picture-ids '(((70 20) sun)
                                 ((130 30) moon)((50 50) ball)
                                 ((130 30) top-scoop)
                                 ((130 30) bottom-scoop)
                                 ((130 30) cone)
                                 ((130 30) sun)) summer-shapes)
              '(moon top-scoop bottom-scoop cone sun))
(check-expect (get-picture-ids '(((10 30) car-body)
                                 ((80 30) cloud))
                               fun-shapes)
              '(car-body door1 door2 window cloud))
(check-expect (get-picture-ids '(((10 10) wheel2)
                                 ((10 20) wheel1))
                               fun-shapes)
              '(wheel2 wheel1))
(check-expect (get-picture-ids '(((50 50) ball)) '((circle ball 10 "Black")))
              '(ball))
(check-expect (get-picture-ids '(((50 50) ball)
                                 ((50 50) ball)) '((circle ball 10 "Black")))
              '(ball))
(check-expect (get-picture-ids '(((10 30) car)
                                 ((80 30) cloud)
                                 ((70 30) car))
                               '((circle wheel1 10 "Black")
                                 (circle wheel2 15 "Black")
                                 (circle cloud 50 "White")
                                 (component car-body
                                            (((20 20) door1)
                                             ((70 20) door2)
                                             ((30 35) window)))
                                 (component car
                                            (((20 10) wheel1)
                                             ((70 10) wheel2)
                                             ((20 40) car-body)))))
              '(cloud car wheel1 wheel2 car-body door1 door2 window))


;; ==== Question 2e ========================

;; (add-offset-to-picture offset picture) outputs a list of picture with
;;   x and y values of offset added to each offset in picture
;; get-component-ids: Offset Picture -> Picture
;; Examples:
(check-expect (add-offset-to-picture '(100 100) ice-cream-pic)
              '(((110 150) ice-cream)
                ((170 120) sun)
                ((230 130) ice-cream)))

(define (add-offset-to-picture offset picture)
  (cond [(empty? picture) empty]
        [else
         (cons (list
                (list (+ (point-x offset) (point-x (first (first picture))))
                      (+ (point-y offset) (point-y (first (first picture)))))
                (shape-id (first picture)))
               (add-offset-to-picture offset (rest picture)))]))

;; (produce-primitives offset/shapeid shapelist parent-shapelist) returns empty
;;   if offset/shapeid shapeid isn't in shapelist or creates list of PrimEleme-
;;   nt (of offset/shapeid and components), parent-shapelist retains shapelist
;; produce-primitives: (list Offset ShapeID) ShapeList Shapelist ->
;;                       (listof PrimElement)
;; Examples:
(check-expect
 (produce-primitives '((20 30) ice-cream) summer-shapes summer-shapes)
 (list
  (make-prim-triangle (make-posn 20 70) (make-posn 40 70) (make-posn 30 120)
                      "Burlywood")
  (make-prim-circle (make-posn 30 65) 10 "LightBlue")
  (make-prim-circle (make-posn 30 55) 10 "Pink")))

(define (produce-primitives offset/shapeid shapelist parent-shapelist)
  (cond [(empty? shapelist) empty]
        [(not (symbol=? (second offset/shapeid) (shape-id (first shapelist))))
         (produce-primitives offset/shapeid (rest shapelist) parent-shapelist)]
        [(symbol=? (shape-type (first shapelist)) 'component)
         (picture->primitives
          (add-offset-to-picture (first offset/shapeid)
                                 (component-picture (first shapelist)))
          parent-shapelist)]
        [(symbol=? (shape-type (first shapelist)) 'circle)
         (list (make-prim-circle (make-posn (point-x (first offset/shapeid))
                                            (point-y (first offset/shapeid)))
                                 (circle-radius (first shapelist))
                                 (circle-color (first shapelist))))]
        [(symbol=? (shape-type (first shapelist)) 'triangle)
         (list (make-prim-triangle
                (make-posn (+ (point-x (triangle-p1 (first shapelist)))
                              (point-x (first offset/shapeid)))
                           (+ (point-y (triangle-p1 (first shapelist)))
                              (point-y (first offset/shapeid))))
                (make-posn (+ (point-x (triangle-p2 (first shapelist)))
                              (point-x (first offset/shapeid)))
                           (+ (point-y (triangle-p2 (first shapelist)))
                              (point-y (first offset/shapeid))))
                (make-posn (+ (point-x (triangle-p3 (first shapelist)))
                              (point-x (first offset/shapeid)))
                           (+ (point-y (triangle-p3 (first shapelist)))
                              (point-y (first offset/shapeid))))
                (triangle-color (first shapelist))))]
        [else (rect (make-posn (point-x (first offset/shapeid))
                               (point-y (first offset/shapeid)))
                    (make-posn (rect-width (first shapelist))
                               (rect-height (first shapelist)))
                    (rect-color (first shapelist)))]))

;; (picture->primitives picture shapelist) produces a list of PrimElement(s)
;;   from shapelist of elements that occur in picture along with each
;;   Shape's components
;; picture->primitives: Picture ShapeList -> (listof PrimElement)
;; Examples:
(check-expect (picture->primitives '(((20 30) ice-cream)) summer-shapes)
              (list
               (make-prim-triangle
                (make-posn 20 70)
                (make-posn 40 70)
                (make-posn 30 120)
                "Burlywood")
               (make-prim-circle (make-posn 30 65) 10 "LightBlue")
               (make-prim-circle (make-posn 30 55) 10 "Pink")))

(define (picture->primitives picture shapelist)
  (cond [(empty? picture) empty]
        [else (append (produce-primitives (first picture) shapelist shapelist)
                      (picture->primitives (rest picture) shapelist))]))

;; Tests:
(check-expect (picture->primitives empty empty) empty)
(check-expect (picture->primitives empty fun-shapes) empty)
(check-expect (picture->primitives fun-pic empty) empty)
(check-expect (picture->primitives fun-pic fun-shapes)
              (list
               (make-prim-circle (make-posn 30 40) 10 "Black")
               (make-prim-circle (make-posn 80 40) 15 "Black")
               (make-prim-triangle (make-posn 50 90) (make-posn 70 90)
                                   (make-posn 70 120) "Black")
               (make-prim-triangle (make-posn 50 90) (make-posn 70 120)
                                   (make-posn 50 120) "Black")
               (make-prim-triangle (make-posn 100 90) (make-posn 120 90)
                                   (make-posn 120 120) "Black")
               (make-prim-triangle (make-posn 100 90) (make-posn 120 120)
                                   (make-posn 100 120) "Black")
               (make-prim-triangle (make-posn 60 105) (make-posn 80 105)
                                   (make-posn 80 115) "Blue")
               (make-prim-triangle (make-posn 60 105) (make-posn 80 115)
                                   (make-posn 60 115) "Blue")
               (make-prim-circle (make-posn 80 30) 50 "White")
               (make-prim-circle (make-posn 90 40) 10 "Black")
               (make-prim-circle (make-posn 140 40) 15 "Black")
               (make-prim-triangle (make-posn 110 90) (make-posn 130 90)
                                   (make-posn 130 120) "Black")
               (make-prim-triangle (make-posn 110 90) (make-posn 130 120)
                                   (make-posn 110 120) "Black")
               (make-prim-triangle (make-posn 160 90) (make-posn 180 90)
                                   (make-posn 180 120) "Black")
               (make-prim-triangle (make-posn 160 90) (make-posn 180 120)
                                   (make-posn 160 120) "Black")
               (make-prim-triangle (make-posn 120 105) (make-posn 140 105)
                                   (make-posn 140 115) "Blue")
               (make-prim-triangle (make-posn 120 105) (make-posn 140 115)
                                   (make-posn 120 115) "Blue")))
(check-expect (picture->primitives fun-pic summer-shapes) empty)
(check-expect (picture->primitives ice-cream-pic fun-shapes) empty)
(check-expect (picture->primitives '(((70 20) sun)) summer-shapes)
              (list (make-prim-circle (make-posn 70 20) 40 "Yellow")))
(check-expect (picture->primitives '(((70 20) sun)
                                     ((130 30) moon)
                                     ((50 50) ball)
                                     ((130 30) top-scoop)
                                     ((130 30) bottom-scoop)
                                     ((130 30) cone)
                                     ((130 30) sun)) summer-shapes)
              (list
               (make-prim-circle (make-posn 70 20) 40 "Yellow")
               (make-prim-circle (make-posn 130 30) 35 "Light Gray")
               (make-prim-circle (make-posn 130 30) 10 "Pink")
               (make-prim-circle (make-posn 130 30) 10 "LightBlue")
               (make-prim-triangle (make-posn 130 30) (make-posn 150 30)
                                   (make-posn 140 80) "Burlywood")
               (make-prim-circle (make-posn 130 30) 40 "Yellow")))
(check-expect (picture->primitives '(((10 30) car-body)
                                     ((80 30) cloud))
                                   fun-shapes)
              (list
               (make-prim-triangle (make-posn 30 50) (make-posn 50 50)
                                   (make-posn 50 80) "Black")
               (make-prim-triangle (make-posn 30 50) (make-posn 50 80)
                                   (make-posn 30 80) "Black")
               (make-prim-triangle (make-posn 80 50) (make-posn 100 50)
                                   (make-posn 100 80) "Black")
               (make-prim-triangle (make-posn 80 50) (make-posn 100 80)
                                   (make-posn 80 80) "Black")
               (make-prim-triangle (make-posn 40 65) (make-posn 60 65)
                                   (make-posn 60 75) "Blue")
               (make-prim-triangle (make-posn 40 65) (make-posn 60 75)
                                   (make-posn 40 75) "Blue")
               (make-prim-circle (make-posn 80 30) 50 "White")))
(check-expect (picture->primitives '(((10 10) wheel2)
                                     ((10 20) wheel1))
                                   fun-shapes)
              (list (make-prim-circle (make-posn 10 10) 15 "Black")
                    (make-prim-circle (make-posn 10 20) 10 "Black")))
(check-expect (picture->primitives '(((50 50) ball))
                                   '((circle ball 10 "Black")))
              (list (make-prim-circle (make-posn 50 50) 10 "Black")))
(check-expect (picture->primitives '(((50 50) ball)
                                     ((50 50) ball))
                                   '((circle ball 10 "Black")))
              (list (make-prim-circle (make-posn 50 50) 10 "Black")
                    (make-prim-circle (make-posn 50 50) 10 "Black")))
(check-expect (picture->primitives '(((10 30) car)
                                     ((80 30) cloud)
                                     ((70 30) car))
                                   '((circle wheel1 10 "Black")
                                     (circle wheel2 15 "Black")
                                     (circle cloud 50 "White")
                                     (component car-body
                                                (((20 20) door1)
                                                 ((70 20) door2)
                                                 ((30 35) window)))
                                     (component car
                                                (((20 10) wheel1)
                                                 ((70 10) wheel2)
                                                 ((20 40) car-body)))))
              (list
               (make-prim-circle (make-posn 30 40) 10 "Black")
               (make-prim-circle (make-posn 80 40) 15 "Black")
               (make-prim-circle (make-posn 80 30) 50 "White")
               (make-prim-circle (make-posn 90 40) 10 "Black")
               (make-prim-circle (make-posn 140 40) 15 "Black")))

(check-expect (picture->primitives '(((10 30) car)
                                     ((80 30) cloud)
                                     ((70 30) car)
                                     ((50 50) random-box))
                                   '((circle wheel1 10 "Black")
                                     (circle wheel2 15 "Black")
                                     (circle cloud 50 "White")
                                     (component car-body
                                                (((20 20) door1)
                                                 ((70 20) door2)
                                                 ((30 35) window)))
                                     (component car
                                                (((20 10) wheel1)
                                                 ((70 10) wheel2)
                                                 ((20 40) car-body)))
                                     (rectangle random-box 50 50 "Black")))
              (list
               (make-prim-circle (make-posn 30 40) 10 "Black")
               (make-prim-circle (make-posn 80 40) 15 "Black")
               (make-prim-circle (make-posn 80 30) 50 "White")
               (make-prim-circle (make-posn 90 40) 10 "Black")
               (make-prim-circle (make-posn 140 40) 15 "Black")
               (make-prim-triangle (make-posn 50 50) (make-posn 100 50)
                                   (make-posn 100 100) "Black")
               (make-prim-triangle (make-posn 50 50) (make-posn 100 100)
                                   (make-posn 50 100) "Black")))

(check-expect (picture->primitives '(((10 30) car))
                                   '((rectangle door1 10 20 "Blue")
                                     (rectangle door2 10 20 "Blue")
                                     (rectangle window 20 10 "Yellow")
                                     (circle wheel1 10 "Black")
                                     (circle wheel2 15 "Black")
                                     (circle cloud 50 "White")
                                     (component car-body
                                                (((20 20) door1)
                                                 ((70 20) door2)
                                                 ((30 35) window)))
                                     (component car
                                                (((20 10) wheel1)
                                                 ((70 10) wheel2)
                                                 ((20 40) car-body)))
                                     (rectangle random-box 50 50 "Black")))
              (list
               (make-prim-circle (make-posn 30 40) 10 "Black")
               (make-prim-circle (make-posn 80 40) 15 "Black")
               (make-prim-triangle (make-posn 50 90) (make-posn 60 90)
                                   (make-posn 60 110) "Blue")
               (make-prim-triangle (make-posn 50 90) (make-posn 60 110)
                                   (make-posn 50 110) "Blue")
               (make-prim-triangle (make-posn 100 90) (make-posn 110 90)
                                   (make-posn 110 110) "Blue")
               (make-prim-triangle (make-posn 100 90) (make-posn 110 110)
                                   (make-posn 100 110) "Blue")
               (make-prim-triangle (make-posn 60 105) (make-posn 80 105)
                                   (make-posn 80 115) "Yellow")
               (make-prim-triangle (make-posn 60 105) (make-posn 80 115)
                                   (make-posn 60 115) "Yellow")))


;; ==== Question 2f ========================

;; (drawing->image bundled-drawing) produces an Image of bundled-drawing
;;   according to the canvas-size (width and height) and primitive-elements
;;   generated from the picture and shapelist in bundled-drawing
;; drawing->image: BundledDrawing -> Image

(define (drawing->image bundled-drawing)
  (render-image
   (make-posn (first bundled-drawing) (second bundled-drawing))
   (picture->primitives (third bundled-drawing) (fourth bundled-drawing))))