;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 08, Question 7
;; *****************************************
;;

;; ==== Question 7a ========================

;; my-compose: (A -> B) (X -> Y) -> ((X -> Y) -> B)
(define (my-compose func-f func-g)
  (lambda (x) (func-f (func-g x))))


;; ==== Question 7b ========================

;; curry: (A B -> C) -> (A -> (B -> C))
(define (curry func-f)
  (lambda (x) (lambda (y) (func-f x y))))


;; ==== Question 7c ========================

;; uncurry: (A -> (B -> C)) -> (A B -> C)
(define (uncurry func-f)
  (lambda (x y) ((func-f x) y)))


;; ==== Question 7d ========================

;; eat-apples: (listof Sym) -> (listof Sym)
(define (eat-apples lst)
  (filter (my-compose not ((curry symbol=?) 'apple)) lst))


;; ==== Question 7e ========================

;; my-map: (X -> Y) (listof X) -> (listof Y)
(define (my-map func lst)
  (foldr (uncurry (my-compose (curry cons) func)) empty lst))


;; ==== Examples: ==========================
(check-expect ((my-compose add1 (lambda (y) (* y 2))) 5) 11)
(check-expect ((((curry my-compose) add1) (lambda (y) (* y 2))) 5) 11)
(check-expect (((uncurry (curry my-compose)) add1 (lambda (y) (* y 2))) 5) 11)
(check-expect (eat-apples '(apple orange apple pear)) '(orange pear))
(check-expect (my-map - '(1 2 3 4 5)) '(-1 -2 -3 -4 -5))
