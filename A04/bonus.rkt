;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 04, Question 5 (Bonus)
;; *****************************************
;;

(define (eval-poly lon x)
  (cond [(empty? lon) 0]
        [else (+ (first lon) (* x (eval-poly (rest lon) x)))]))
  