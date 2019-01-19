;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 04, Question 3
;; *****************************************
;;

;; ==== Question 3a ========================

;; (absolutely-odd lst) outputs the sum of
;;   the absolute values of odd integers in lst
;; absolutely-odd: (listof Int) -> Nat
;; Examples:
(check-expect (absolutely-odd (cons -1 (cons 2 (cons 3 empty)))) 4)
(check-expect (absolutely-odd (cons 2 (cons 4 (cons 6 empty)))) 0)
(check-expect (absolutely-odd (cons 10 (cons 9 (cons 8 empty)))) 9)

(define base-case 0)

(define (absolutely-odd lst)
  (cond
    [(empty? lst) base-case]
    [(even? (first lst)) (absolutely-odd (rest lst))]
    [else (+ (abs (first lst)) (absolutely-odd (rest lst)))]))

(check-expect (absolutely-odd (cons -1 (cons -1 (cons -1 empty)))) 3)
(check-expect (absolutely-odd (cons -2 (cons 2 (cons 2 empty)))) 0)
(check-expect (absolutely-odd empty) 0)
(check-expect (absolutely-odd (cons -1 empty)) 1)


;; ==== Question 3b ========================

;; (spiraling? lst) outputs true if lst
;;   is a spiraling list and false otherwise
;; spiraling?: (listof Int) -> Bool
;; Examples:
(check-expect (spiraling? (cons 1 (cons -10 (cons 100 empty)))) true)
(check-expect (spiraling? (cons 0 (cons -10 (cons 100 empty)))) false)

(define pos-neg-threshold 0)

(define (spiraling? lst)
  (cond
    [(empty? lst) true]
    [(empty? (rest lst)) true]
    [(and (< (abs (first lst)) (abs (first (rest lst))))
          (or (and (< (first lst) pos-neg-threshold)
                   (> (first (rest lst)) pos-neg-threshold))
              (and (> (first lst) pos-neg-threshold)
                   (< (first (rest lst)) pos-neg-threshold))))
     (spiraling? (rest lst))]
    [else false]))

;; Tests:
(check-expect (spiraling? empty) true)
(check-expect (spiraling? (cons 1 empty)) true)
(check-expect (spiraling? (cons 0 empty)) true)
(check-expect (spiraling? (cons -1 (cons 2 (cons -3 (cons 4 empty))))) true)
(check-expect (spiraling? (cons 99 (cons -100 (cons 100 empty)))) false)
(check-expect (spiraling? (cons -9 (cons 10 (cons -11 (cons 12 empty))))) true)
(check-expect (spiraling? (cons 8 (cons -9 (cons 10 (cons -10 empty))))) false)


;; ==== Question 3c ========================

;; (count lst) outputs the number
;;   of elements in lst
;; count: (listof Any) -> Nat
;; requires: lst to be non-empty
;; Examples:
(check-expect (count (cons 9 (cons 0.5 (cons 6 empty)))) 3)

(define counter 1)

(define (count lst)
  (cond
    [(empty? (rest lst)) counter]
    [else (+ counter (count (rest lst)))]))

;; (product lst) outputs the product
;;   of numbers within lst
;; product: (listof Num) -> Num
;; requires: lst to be non-empty
;;           elements of lst must be greater than 0
;; Examples
(check-expect (product (cons 9 (cons 0.5 (cons 6 empty)))) 27)

(define (product lst)
  (cond
    [(empty? (rest lst)) (first lst)]
    [else (* (first lst) (product (rest lst)))]))

;; (geometric-mean lst) outputs the
;;   geometric mean of lst
;; geometric-mean: (listof Num) -> Num
;; requires: lst to be non-empty
;;           elements of lst must be greater than 0
;; Examples:
(check-within (geometric-mean (cons 9 (cons 0.5 (cons 6 empty)))) 3 0.0001)
(check-within (geometric-mean (cons 1 empty)) 1 0.0001)

(define (geometric-mean lst)
  (expt (product lst) (/ 1 (count lst))))

;; Tests:
(check-within (geometric-mean (cons 3 (cons 6 (cons 9 empty)))) 5.4514 0.0001)
(check-within (geometric-mean (cons 10 (cons 10 empty))) 10 0.0001)
(check-within
 (geometric-mean (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))
 2.6052 0.0001)
