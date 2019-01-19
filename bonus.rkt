;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 01, Problem 5
;; ***************************************************
;;

(define best-questions 0.75)

(define correct-points 2)

(define (cs135-participation questions correct incorrect)
  (* 100 (/ (+ (* (min correct (* best-questions questions)) correct-points)
         (min (max 0 (- (* best-questions questions) correct))
              incorrect)) (* best-questions questions correct-points))))