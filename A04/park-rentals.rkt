;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname park-rentals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 04, Question 2
;; *****************************************
;;

(define-struct bicycle (make model serial-number))
;; A Bicycle is a (make-bicycle Str Str Nat)

(define-struct boat (type serial-number))
;; A Boat is a (make-boat Sym Nat)
;; requires: type is 'paddle-boat or 'canoe

(define-struct horse (name serial-number capacity stamina))
;; A Horse is a (make-horse Str Nat Nat Num)
;; requires: stamina > 0, capacity > 0

;; A Rental is (anyof Bicycle Boat Horse)

;; ==== Question 2a ========================

;; rental-template: Rental -> Any
;; (define (rental-template rental)
;;   (cond
;;     [(bicycle? rental)
;;      (... (bicycle-make rental) ...
;;      ... (bicycle-model rental) ...
;;      ... (bicycle-serial-number rental) ...)]
;;     [(boat? rental)
;;      (... (boat-type rental) ...
;;      ... (boat-serial-number rental) ...)]
;;     [(horse? rental)
;;      (... (horse-name rental) ...
;;      ... (horse-serial-number rental) ...
;;      ... (horse-capacity rental) ...
;;      ... (horse-stamina rental) ...)]))


;; ==== Question 2b ========================

;; (rental-id rental) outputs the serial
;;   number associated with rental
;; rental-id: Rental -> Nat
;; Examples:
(check-expect (rental-id (make-bicycle "Chromag" "Stylus" 174)) 174)

(define (rental-id rental)
  (cond [(bicycle? rental) (bicycle-serial-number rental)]
        [(boat? rental) (boat-serial-number rental)]
        [(horse? rental) (horse-serial-number rental)]))

;; Tests:
(check-expect (rental-id (make-boat 'paddle-boat 365)) 365)
(check-expect (rental-id (make-horse "Jack" 8277 6 1.5)) 8277)


;; ==== Question 2c ========================

;; (rental-ok? rental renters rental-duration)
;;   outputs true if the specified rental is
;;   legal and false otherwise
;; rental-ok: Rental Nat Num -> Bool
;; requires: renters > 0
;;           rental-duration > 0
;; Examples:
(check-expect (rental-ok? (make-bicycle "Chromag" "Stylus" 174) 1 2.3) true)
(check-expect (rental-ok? (make-boat 'canoe 899) 2 2.2) true)

(define bicycle-capacity 1)
(define canoe-capacity 2)
(define paddle-boat-capacity 3)

(define (rental-ok? rental renters rental-duration)
  (cond
    [(bicycle? rental) (= renters bicycle-capacity)]
    [(boat? rental) (cond [(symbol=? (boat-type rental) 'canoe)
                           (<= renters canoe-capacity)]
                          [else (<= renters paddle-boat-capacity)])]
    [(horse? rental) (and (<= renters (horse-capacity rental))
                          (<= rental-duration (horse-stamina rental)))]))

;; Tests:
(check-expect (rental-ok? (make-bicycle "Chromag" "Stylus" 174) 4 2.3) false)
(check-expect (rental-ok? (make-boat 'canoe 899) 1 2.2) true)
(check-expect (rental-ok? (make-boat 'canoe 899) 5 2.2) false)
(check-expect (rental-ok? (make-boat 'paddle-boat 899) 3 2.2) true)
(check-expect (rental-ok? (make-boat 'paddle-boat 899) 1 2.2) true)
(check-expect (rental-ok? (make-boat 'paddle-boat 899) 5 2.2) false)
(check-expect (rental-ok? (make-horse "Jack" 8277 6 1.5) 6 1) true)
(check-expect (rental-ok? (make-horse "Jack" 8277 6 1.5) 4 1) true)
(check-expect (rental-ok? (make-horse "Jack" 8277 6 1.5) 8 1) false)
(check-expect (rental-ok? (make-horse "Jack" 8277 6 1.5) 3 1.5) true)
(check-expect (rental-ok? (make-horse "Jack" 8277 6 1.5) 3 1.3) true)
(check-expect (rental-ok? (make-horse "Jack" 8277 6 1.5) 3 1.7) false)


;; ==== Question 2d ========================

;; (round-up-to-nearest-half number) takes
;;   number and rounds it up to the closest 0.5
;;   interval
;; round-up-to-nearest-half: Num -> Num
;; Examples:
(check-expect (round-up-to-nearest-half 1.25) 1.5)
(check-expect (round-up-to-nearest-half -1.25) -1)

(define (round-up-to-nearest-half number)
  (/ (ceiling (* 2 number)) 2))

;; (horse-price renters rental-duration) outputs the cost
;;   of a horse rental given renters and rental-duration
;; horse-price: Nat Num -> Nat
;; requires: renters > 0
;;           rental-duration > 0
;; Examples:
(check-expect (horse-price 1 1.25) 90)
(check-expect (horse-price 5 1.75) 200)

(define horse-hour-rate 60)
(define additional-hour-rate 20)
(define base-price-max-people 3)

(define (horse-price renters rental-duration)
  (cond
    [(> renters base-price-max-people)
     (* (+ horse-hour-rate
           (* additional-hour-rate
              (- renters base-price-max-people)))
        (round-up-to-nearest-half rental-duration))]
    [else (* horse-hour-rate (round-up-to-nearest-half rental-duration))]))

;; (rental-price rental renters rental-duration) outputs
;;   the cost of a rental given renters and rental-duration
;; rental-price: Rental Nat Num -> Nat
;; requires: (rental-ok? rental renters rental-duration) to output boolean true
;;           renters > 0
;;           rental-duration > 0
;; Examples:
(check-expect (rental-price (make-bicycle "Chromag" "Stylus" 174) 1 2.3) 60)
(check-expect (rental-price (make-boat 'canoe 899) 2 2.2) 90)

(define bicycle-hour-rate 20)
(define boat-hour-rate 30)

(define (rental-price rental renters rental-duration)
  (cond
    [(bicycle? rental) (* bicycle-hour-rate (ceiling rental-duration))]
    [(boat? rental) (* boat-hour-rate (ceiling rental-duration))]
    [(horse? rental) (horse-price renters rental-duration)]))

;; Tests:
(check-expect (rental-price (make-bicycle "Chromag" "Stylus" 174) 1 2.6) 60)
(check-expect (rental-price (make-boat 'canoe 899) 2 0.1) 30)
(check-expect (rental-price (make-boat 'paddle-boat 899) 1 3.1) 120)
(check-expect (rental-price (make-horse "Jack" 8277 6 1.75) 6 1.25) 180)
(check-expect (rental-price (make-horse "Jack" 8277 6 1.75) 3 1.25) 90)
(check-expect (rental-price (make-horse "Jack" 8277 6 1.75) 2 1.5) 90)
(check-expect (rental-price (make-horse "Jack" 8277 6 1.75) 2 1.75) 120)
(check-expect (rental-price (make-horse "Jack" 8277 6 1.75) 4 1.75) 160)