;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rainbow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 04, Question 4
;; *****************************************
;;

;; ==== Question 4a ========================

;; (colour->number colour) converts colour from the rainbow 
;;   (roygbiv) to a corresponding position with red starting 
;;   at 1 and outputs 0 if colour isn't roygbiv
;; colour->number: Any -> Nat

(define red 1)
(define orange 2)
(define yellow 3)
(define green 4)
(define blue 5)
(define indigo 6)
(define violet 7)
(define invalid 0)

(define (colour->number colour)
  (cond [(equal? colour 'red) red]
        [(equal? colour 'orange) orange]
        [(equal? colour 'yellow) yellow]
        [(equal? colour 'green) green]
        [(equal? colour 'blue) blue]
        [(equal? colour 'indigo) indigo]
        [(equal? colour 'violet) violet]
        [else invalid]))

;; (insert n slon) inserts n
;;   into slon according to numerical value
;; insert: Num (listof Num) -> (list of Num)
;; Examples: see wrapper function rsort

(define (insert n slon)
  (cond [(empty? slon) (cons n empty)]
        [(<= n (first slon)) (cons n slon)]
        [else (cons (first slon) (insert n (rest slon)))]))

;; (rsort lst) converts a list of rainbow colours or other
;;   elements to numbers and sorts their position from
;;   lowest to highest value
;; rsort: (listof Any) -> (listof Nat)
;; Examples:
(check-expect (rsort (cons 'black (cons 'green (cons' blue empty))))
                    (cons 0 (cons 4 (cons 5 empty))))
(check-expect (rsort (cons 'yellow (cons 'yellow (cons' red empty))))
                    (cons 1 (cons 3 (cons 3 empty))))

(define (rsort lst)
  (cond [(empty? lst) empty]
        [else (insert (colour->number (first lst)) (rsort (rest lst)))]))

;; (rainbow->lon lst) converts elements in lst to
;;   numbers corresponding to a position of a colour
;;   in the rainbow or an invalid number otherwise
;; rainbow->lon: (listof Any) -> (listof Nat)
;; Examples:
(check-expect (rainbow->lon (cons 'indigo (cons 'red empty)))
              (cons 6 (cons 1 empty)))

(define (rainbow->lon lst)
  (cond
    [(empty? lst) empty]
    [else (cons (colour->number (first lst)) (rainbow->lon (rest lst)))]))

;; (repeating? lst) checks if lst has
;;   repeating numbers
;; repeating?: (listof Num) -> Bool
;; requires: lst must be sorted from lowest to highest value
;; Examples:
(check-expect (repeating? (cons 1 (cons 1 empty))) true)
(check-expect (repeating? (cons 1 (cons 2 empty))) false)

(define (repeating? lst)
  (cond [(empty? lst) false]
        [(empty? (rest lst)) false]
        [(= (first lst) (first (rest lst))) true]
        [else (repeating? (rest lst))]))

;; (invalid-colour? lst) checks if lst
;;   contains invalid colours represented through
;;   numbers
;; invalid-colour?: (listof Nat) -> Bool
(check-expect (repeating? (cons 1 (cons 1 empty))) true)
(check-expect (repeating? (cons 1 (cons 0 empty))) false)

(define (invalid-colour? lst)
  (cond [(empty? lst) false]
        [(= invalid (first lst)) true]
        [else (invalid-colour? (rest lst))]))

;; (rainbow? list) determines if list is a valid rainbow
;; rainbow?: (listof Any) -> Bool
;; Examples:
(check-expect
 (rainbow? (cons 'red (cons 'green (cons 'blue (cons' indigo empty))))) true)
(check-expect
 (rainbow? (cons 'green (cons 'green (cons' indigo empty)))) false)

(define (rainbow? list)
  (cond [(not (equal? (rainbow->lon list) (rsort list))) false]
        [(repeating? (rsort list)) false]
        [(invalid-colour? (rsort list)) false]
        [else true]))

;; Tests:
(check-expect (rainbow? empty) true)
(check-expect (rainbow? (cons 'orange (cons 'orange empty))) false)
(check-expect (rainbow? (cons 'indigo (cons 'red empty))) false)
(check-expect (rainbow? (cons 'orange (cons 'violet empty))) true)
(check-expect (rainbow? (cons 'yellow (cons 'fries empty))) false)
(check-expect (rainbow? (cons 'black empty)) false)
(check-expect (rainbow? (cons 'blue empty)) true)
(check-expect (rainbow? (cons 'brown (cons 'green (cons 'blue empty)))) false)


;; ==== Question 4b ========================

;; (unicorn colour rainbow) produces a new rainbow
;;   with all instances of colour missing from rainbow
;; unicorn: Sym (listof Sym) -> (listof Sym)
;; requires: (rainbow? rainbow) to produce the boolean true
;;           colour: (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet) 
;; Examples:
(check-expect
 (unicorn 'red (cons 'red (cons 'yellow (cons 'blue (cons 'violet empty)))))
 (cons 'yellow (cons 'blue (cons 'violet empty))))
(check-expect
 (unicorn 'green (cons 'red (cons 'yellow (cons 'blue empty))))
 (cons 'red (cons 'yellow (cons 'blue empty))))

(define (unicorn colour rainbow)
  (cond [(empty? rainbow) empty]
        [(symbol=? colour (first rainbow)) (unicorn colour (rest rainbow))]
        [else (cons (first rainbow) (unicorn colour (rest rainbow)))]))

(check-expect (unicorn 'green empty) empty)
(check-expect (unicorn 'blue (cons 'blue empty)) empty)
(check-expect (unicorn 'indigo (cons 'red empty)) (cons 'red empty))
(check-expect
 (unicorn 'blue (cons 'red (cons 'orange (cons 'blue empty))))
 (cons 'red (cons 'orange empty)))
(check-expect
 (unicorn 'yellow (cons 'orange (cons 'yellow (cons 'blue empty))))
 (cons 'orange (cons 'blue empty)))


;; ==== Question 4c ========================

;; (number->colour number) converts a number
;;   to its corresponding colour in the rainbow (roygbiv)
;;   with 1 starting at red
;; number->colour: Nat -> Sym
;; requires: 1 <= number <= 7
;; Examples:
(check-expect (number->colour 7) 'violet)

(define (number->colour number)
  (cond [(= number red) 'red]
        [(= number orange) 'orange]
        [(= number yellow) 'yellow]
        [(= number green) 'green]
        [(= number blue) 'blue]
        [(= number indigo) 'indigo]
        [else 'violet]))

;; (lon->rainbow lon) converts numbers in lon to
;;   their corresponding colour in the rainbow
;; lon->rainbow: (listof Nat) -> (listof Sym)
;; Examples:
(check-expect (lon->rainbow (cons 1 (cons 2 (cons 5 empty))))
              (cons 'red (cons 'orange (cons 'blue empty))))

(define (lon->rainbow lon)
  (cond [(empty? lon) empty]
        [else (cons (number->colour (first lon)) (lon->rainbow (rest lon)))]))

;; (leprechaun colour rainbow) produces a new rainbow
;;   with colour added to it in its proper position, if
;;   colour is already in rainbow, rainbow is unchanged
;; leprechaun: Sym (listof Sym) -> (listof Sym)
;; requires: (rainbow? rainbow) to produce the boolean true
;;           colour: (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet)
;; Examples:
(check-expect
 (leprechaun 'violet (cons 'red (cons 'yellow (cons 'blue empty))))
 (cons 'red (cons 'yellow (cons 'blue (cons 'violet empty)))))

(define (leprechaun colour rainbow)
  (cond
    [(repeating? (insert (colour->number colour) (rsort rainbow)))
     (lon->rainbow (rsort rainbow))]
    [else (lon->rainbow (insert (colour->number colour) (rsort rainbow)))]))

;; Tests:
(check-expect (leprechaun 'red empty) (cons 'red empty))
(check-expect (leprechaun 'orange (cons 'orange empty)) (cons 'orange empty))
(check-expect (leprechaun 'yellow (cons 'violet empty))
              (cons 'yellow (cons 'violet empty)))
(check-expect (leprechaun 'green (cons 'orange empty))
              (cons 'orange (cons 'green empty)))
(check-expect
 (leprechaun 'indigo (cons 'red (cons 'orange (cons 'blue empty))))
 (cons 'red (cons 'orange (cons 'blue (cons 'indigo empty)))))
(check-expect
 (leprechaun 'red (cons 'red (cons 'orange (cons 'blue empty))))
 (cons 'red (cons 'orange (cons 'blue empty))))



