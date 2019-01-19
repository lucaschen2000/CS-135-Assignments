;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f () #f)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 09, Question 4
;; *****************************************
;;

;; ==== Question 3a ========================

;; (solution? f lon) checks that f generates the values in
;;   the list of integers lon, where f takes a position in
;;   the list as an argument, starting at 0.
;; solution?: (Nat -> Int) (listof Int) -> Bool
;; Examples:
(check-expect (solution? identity (build-list 10 identity)) true)

(define (solution? f lon)
  (equal? (build-list (length lon) f) lon))

;; Tests:
(check-expect (solution? (lambda (x) (* 3 x)) '(0 3 6 9 12)) true)
(check-expect (solution? (lambda (i) (* 2 (+ i 1))) (list 2 4 6 8 10 12)) true)
(check-expect (solution? (lambda (x) (sub1 (* 3 (add1 x)))) '(2 5 8 11)) true)
(check-expect (solution? (lambda (x) (sub1 (* 3 (add1 x)))) '(2 1 8 11)) false)
(check-expect (solution? add1 (list 1 2 3 4 5 6)) true)
(check-expect (solution? add1 (list 2 3 4 5 6 7)) false)
(check-expect (solution? identity empty) true)


;; ==== Question 3b ========================

;; (guess-quadratic lon) guesses that the number sequence lon has a
;; quadratic solution and produces the guess.
;; guess-quadratic: (listof Int) -> (Nat -> Int)
;; Examples:
(check-expect (solution? (guess-quadratic '(8 11 16)) '(8 11 16 23 32)) true)

(define (guess-quadratic lon)
  (cond [(empty? lon) (lambda (x) 0)]
        [(= (length lon) 1)
         (lambda (x) (first lon))]
        [(= (length lon) 2)
         (lambda (x) (+ (* (- (second lon) (first lon)) x) (first lon)))]
        [else (local
                [(define c (first lon))
                 (define a (/ (- (+ c (third lon)) (* 2 (second lon))) 2))
                 (define b (- (second lon) a c))]
                (lambda (i) (+ (* (sqr i) a) (* i b) c)))]))

;; Tests:
(define test-a (list 2 4 6 8 10 12))
(define test-b (list 1 11 33 67 113))
(define test-c (list 2 4 8 16 32 64))
(check-expect (solution? (guess-quadratic test-a) test-a) true)
(check-expect (solution? (guess-quadratic test-b) test-b) true)
(check-expect (solution? (guess-quadratic test-c) test-c) false)

(check-expect (solution? (guess-quadratic '()) '()) true)
(check-expect (solution? (guess-quadratic '()) '(0)) true)
(check-expect (solution? (guess-quadratic '()) '(1)) false)
(check-expect (solution? (guess-quadratic '()) '(0 0 0 0)) true)
(check-expect (solution? (guess-quadratic '()) '(1 0 0 0)) false)
(check-expect (solution? (guess-quadratic '(1)) '(1 1 1 1)) true)
(check-expect (solution? (guess-quadratic '(1)) '(1 1 1 0)) false)
(check-expect (solution? (guess-quadratic '(1 3)) '(1 3 5 7)) true)
(check-expect (solution? (guess-quadratic '(1 3)) '(3 5)) false)
(check-expect (solution? (guess-quadratic '(1 3 5 7)) '(1 3 5 7 9)) true)
(check-expect (solution? (guess-quadratic '(8 11 16)) '(8 11 16 23 2 2)) false)
(check-expect (solution? (guess-quadratic '(11 -39 22 63 111))
                         '(11 -39 22 63 111)) false)
(check-expect (solution? (guess-quadratic '(10 13 18))
                         '(10 13 18 25 34 45)) true)


;; ==== Question 3c ========================

;; (try-quadratic lon) tries solving the number sequence lon with
;;   a quadratic. It produces the solution if it works and empty
;;   if it doesn't.
;; try-quadratic: (listof Int) -> (anyof (Nat -> Int) empty)
;; Examples:
(check-expect ((try-quadratic '(10 13 18)) 5) 45)

(define (try-quadratic lon)
  (cond [(solution? (guess-quadratic lon) lon)
         (guess-quadratic lon)]
        [else empty]))

;; Tests:
(check-expect ((try-quadratic test-a) 6) 14)
(check-expect ((try-quadratic test-a) 1000000) 2000002)
(check-expect ((try-quadratic test-b) 5) 171)
(check-expect (try-quadratic test-c) empty)

(check-expect ((try-quadratic '(1 3)) 5) 11)
(check-expect ((try-quadratic '(8 11 16)) 4) 32)
(check-expect (try-quadratic '(11 -39 22 63 111)) empty)
(check-expect ((try-quadratic '()) 5) 0)


;; ==== Question 3d ========================

;; (guess-recursive lon) guesses that the number sequence lon has a
;;   recursive solution of the form (ni = a(n(i-1)) + b(n(i-2)) and produces
;;   the guess.
;; guess-recursive: (listof Int) -> (Nat -> Int)
;; Examples:
(check-expect (solution? (guess-recursive '(1 2 3 4 5)) '(1 2 3 4 5)) true)

(define (guess-recursive lon)
  (cond [(< (length lon) 4) (guess-quadratic lon)]
        [else
         (local
           [(define n0 (first lon))
            (define n1 (second lon))
            (define n2 (third lon))
            (define n3 (fourth lon))]
           (cond
             [(and (zero? n0) (zero? n1))
              (lambda (x) 0)]
             [(= (- (sqr n1) (* n0 n2)) 0)
              (local [(define a (/ n1 n0))
                      (define b 0)
                      (define (nth-term-acc n-1 n-2 acc)
                        (cond [(= acc 2) (+ (* a n-1) (* b n-2))]
                              [else
                               (nth-term-acc
                                (+ (* a n-1) (* b n-2)) n-1 (sub1 acc))]))
                      (define (nth-term n)
                        (cond [(= n 0) n0]
                              [(= n 1) n1]
                              [else (nth-term-acc n1 n0 n)]))]
                (lambda (x) (nth-term x)))]
             [(= n1 0)
              (local
                [(define b (/ (- (sqr n2) (* n1 n3)) (- (* n0 n2) (sqr n1))))
                 (define a (/ (- n3 (* n1 b)) n2))
                 (define (nth-term-acc n-1 n-2 acc)
                   (cond [(= acc 2) (+ (* a n-1) (* b n-2))]
                         [else (nth-term-acc
                                (+ (* a n-1) (* b n-2)) n-1 (sub1 acc))]))
                 (define (nth-term n)
                   (cond [(= n 0) n0]
                         [(= n 1) n1]
                         [else (nth-term-acc n1 n0 n)]))]
                (lambda (x) (nth-term x)))]
             [else
              (local [(define b
                        (/ (- (sqr n2) (* n1 n3)) (- (* n0 n2) (sqr n1))))
                      (define a (/ (- n2 (* n0 b)) n1))
                      (define (nth-term-acc n-1 n-2 acc)
                        (cond [(= acc 2) (+ (* a n-1) (* b n-2))]
                              [else (nth-term-acc
                                     (+ (* a n-1) (* b n-2)) n-1 (sub1 acc))]))
                      (define (nth-term n)
                        (cond [(= n 0) n0]
                              [(= n 1) n1]
                              [else (nth-term-acc n1 n0 n)]))]
                (lambda (x) (nth-term x)))]))]))

;; Tests:
(check-expect (solution? (guess-recursive '(1 2)) '(1 2 3 4 5)) true)
(check-expect (solution? (guess-recursive '(1 2 7)) '(1 2 7)) true)
(check-expect (solution? (guess-recursive '(11 -39 22)) '(11 -39 22)) true)
(check-expect (solution? (guess-recursive '(11 -39 22)) '(11 1 22)) false)
(check-expect (solution? (guess-recursive '(0 0 0 0)) '(0 0 0 0 0 0)) true)
(check-expect (solution? (guess-recursive '(0 0 0 0)) '(0 0 0 0 0 1)) false)
(check-expect (solution? (guess-recursive '(0 0 1 17)) '(0 0 0 0 0 0)) true)
(check-expect (solution? (guess-recursive '(0 0 1 17)) '(0 0 2 3 0 0)) false)
(check-expect (solution? (guess-recursive '(3 9 27 81)) '(3 9 27 81 243)) true)
(check-expect (solution? (guess-recursive '(3 9 27 81)) '(3 9 27 81 27)) false)
(check-expect (solution? (guess-recursive '(1 0 1 0)) '(1 0 1 0 1 0)) true)
(check-expect (solution? (guess-recursive '(0 1 0 1)) '(0 1 0 1 0 1 0)) true)
(check-expect (solution? (guess-recursive '(1 0 0 8)) '(1 0 0 0 0)) true)
(check-expect (solution? (guess-recursive '(1 1 2 3)) '(1 1 2 3 5 8 13)) true)
(check-expect (solution? (guess-recursive '(11 0 2 3)) '(11 0 2 3)) true)
(check-expect
 (solution? (guess-recursive '(1 2 6 16)) '(1 2 6 16 44 120)) true)
(check-expect (solution? (guess-recursive '(1 2 6 16)) '(0)) false)

;; (try-recursive lon) tries solving the number sequence lon with
;;   a recursive formula. It produces the solution if it works and empty
;;   if it doesn't.
;; try-recursive: (listof Int) -> (anyof (Nat -> Int) empty)
;; Examples:
(check-expect ((try-recursive '(1 2 3 4 5)) 8) 9)

(define (try-recursive lon)
  (cond [(solution? (guess-recursive lon) lon) (guess-recursive lon)]
        [else empty]))

;; Tests:
(define test-d (list 1 1 2 3 5 8 13))
(check-expect ((try-recursive test-d) 7) 21)
(check-expect ((try-recursive test-d) 100) 573147844013817084101)
(define test-e (list 0 1 1 2 3 5 8))
(check-expect ((try-recursive test-e) 7) 13)
(define test-f (list 1 1 3 5 11 21 43))
(check-expect ((try-recursive test-f) 7) 85)
(define test-g (list 2 0 0 0 0 0))
(check-expect ((try-recursive test-g) 0) 2)
(check-expect ((try-recursive test-g) 100) 0)
(check-expect (try-recursive test-b) empty)

(check-expect ((try-recursive '(1 1 2 3)) 6) 13)
(check-expect ((try-recursive '(11 0 2 3)) 3) 3)
(check-expect (try-recursive '(11 0 2 3 1 89 1)) empty)
(check-expect ((try-recursive '(3 9 27 81)) 4) 243)
(check-expect ((try-recursive '(0 0 0 0)) 6) 0)
(check-expect ((try-recursive '(11 -39 22)) 1) -39)
(check-expect ((try-recursive '(1 0 1 0 1 0)) 12) 1)
(check-expect ((try-recursive '(0 1 0 1)) 12) 0)
(check-expect ((try-recursive '(0 1 -2 3 -4)) 5) 5)


;; ==== Question 3e ========================

;; (supersolve lon) tries to create a solution to the quadratic solution
;;   and then a recursive solution to lon in that order, and returns a
;;   valid solution if it exists, otherwise returns empty
;; supersolve: (listof Int) -> (anyof (Nat -> Int) empty)
;; Examples:
(check-expect ((supersolve '(10 -8 6 -4 2 0)) 6) -2)

(define (supersolve lon)
  (cond [(not (empty? (try-quadratic lon))) (try-quadratic lon)]
        [else (try-recursive lon)]))

;; Tests:
(check-expect ((supersolve test-a) 6) 14)
(check-expect ((supersolve test-a) 1000000) 2000002)
(check-expect ((supersolve test-b) 5) 171)
(check-expect ((supersolve test-c) 8) 512)
(check-expect ((supersolve test-d) 7) 21)
(check-expect ((supersolve test-d) 100) 573147844013817084101)
(check-expect ((supersolve test-e) 7) 13)
(check-expect ((supersolve test-f) 7) 85)
(check-expect ((supersolve test-g) 0) 2)
(check-expect ((supersolve test-g) 100) 0)
(check-expect (supersolve '(1 1 2 1 2 3 1 2 3 4)) empty)


(check-expect ((supersolve '(1 3)) 5) 11)
(check-expect ((supersolve '(8 11 16)) 4) 32)
(check-expect (supersolve '(11 -39 22 63 111)) empty)
(check-expect ((supersolve '()) 5) 0)
(check-expect ((supersolve '(1 1 2 3)) 6) 13)
(check-expect ((supersolve '(11 0 2 3)) 3) 3)
(check-expect (supersolve '(11 0 2 3 1 89 1)) empty)
(check-expect ((supersolve '(3 9 27 81)) 4) 243)
(check-expect ((supersolve '(0 0 0 0)) 6) 0)
(check-expect ((supersolve '(11 -39 22)) 1) -39)
(check-expect ((supersolve '(1 0 1 0 1 0)) 12) 1)
(check-expect ((supersolve '(0 1 0 1)) 12) 0)
(check-expect ((supersolve '(0 1 -2 3 -4)) 5) 5)