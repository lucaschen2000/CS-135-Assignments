;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 01, Problem 4
;; ***************************************************
;;

(define class-participation 5)

(define first-weight 0.1)

(define second-weight 0.2)

(define final-weight 0.45)

(define assignments-weight 0.2)


;; a)

(define (final-cs135-grade first second final assignments)
  (+ class-participation (* first first-weight) (* second second-weight)
     (* final final-weight) (* assignments assignments-weight)))


;; b)

(define passing-grade 60)

(define (cs135-final-exam-grade-needed first second assignments)
  (/ (- passing-grade (+ class-participation (* first first-weight)
              (* second second-weight)
              (* assignments assignments-weight))) final-weight))

