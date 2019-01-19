;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fib) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f () #f)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 09, Question 2
;; *****************************************
;;

;; (fast-fib n) returns the nth term of the fibonacci sequence
;; fast-fib: Nat -> Nat
;; Examples:
(check-expect (fast-fib 3) 2)

(define (fast-fib n)
  (local [(define (fibonacci a b acc)
            (cond [(= 0 acc) (+ a b)]
                  [else (fibonacci b (+ a b) (sub1 acc))]))]
          (fibonacci -1 1 n)))

;; Tests:
(check-expect (fast-fib 0) 0)
(check-expect (fast-fib 1) 1)
(check-expect (fast-fib 4) 3)
(check-expect (fast-fib 5) 5)
(check-expect (fast-fib 41) 165580141)
(check-expect (fast-fib 100) 354224848179261915075)
(check-expect (fast-fib 199) 173402521172797813159685037284371942044301)
(check-expect (fast-fib 200) 280571172992510140037611932413038677189525)

