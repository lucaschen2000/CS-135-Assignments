;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 01, Problem 3
;; ***************************************************
;;

;; a)

(define litres/gallon 3.785411784)

(define kilometres/mile 1.609344)

(define (mpg->lp100km mpg)
  (/ (* mpg litres/gallon 100) kilometres/mile))


;; b)

(define yards/mile 1760)

(define mL/thimble 2.1)

(define mL/gallon 3785.41)

(define yards/rod 5.5)

(define rods/chain 4)

(define (mpg->cpt mpg)
  (/ (* mpg yards/mile mL/thimble) (* mL/gallon yards/rod rods/chain)))