;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 03, Question 2
;; *****************************************
;;

;; ==== Question 2a ========================

;; (posn-mult posn1 posn2) outputs the product 
;;   between posn1 and posn2 using the formula:
;;   (x1, y1)*(x2, y2) = (x1*x2 - y1*y2 , x1*y2 + x2*y1)
;; posn-mult: Posn Posn -> Posn
;; Examples:
(check-expect (posn-mult (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (posn-mult (make-posn 3 4) (make-posn 6 5)) (make-posn -2 39))

(define (posn-mult posn1 posn2)
  (make-posn (- (* (posn-x posn1) (posn-x posn2))
                (* (posn-y posn1) (posn-y posn2)))
             (+ (* (posn-x posn1) (posn-y posn2))
                (* (posn-x posn2) (posn-y posn1)))))

;; Tests:
(check-expect (posn-mult (make-posn 5 5) (make-posn 10 10))
              (make-posn 0 100))
(check-expect (posn-mult (make-posn -2 -9) (make-posn 4 -6))
              (make-posn -62 -24))
(check-expect (posn-mult (make-posn 5 -9) (make-posn -5 6))
              (make-posn 29 75))


;; ==== Question 2b ========================

;; (posn-div posn1 posn2) outputs the division 
;;   between posn1 and posn2 using the formula:
;;   (x1, y1)/(x2, y2) = ((x1*x2 + y1*y2) / ((x_2)^2 + (y_2)^2) ,
;;                        (y1*x2 - x1*y2) / ((x_2)^2 + (y_2)^2))
;; posn-div: Posn Posn -> Posn
;; requires: (posn-x posn2) or (posn-y posn2) must be a non-zero real number
;; Examples:
(check-expect (posn-div (make-posn 3 5) (make-posn 1 3)) (make-posn 1.8 -0.4))

(define (posn-div posn1 posn2)
  (make-posn (/ (+ (* (posn-x posn1) (posn-x posn2))
                   (* (posn-y posn1) (posn-y posn2)))
                (+ (sqr (posn-x posn2)) (sqr (posn-y posn2))))
             (/ (- (* (posn-y posn1) (posn-x posn2))
                   (* (posn-x posn1) (posn-y posn2)))
                (+ (sqr (posn-x posn2)) (sqr (posn-y posn2))))))

;; Tests:
(check-expect (posn-div (make-posn 0 0) (make-posn 1 0))
              (make-posn 0 0))
(check-expect (posn-div (make-posn 1 1) (make-posn 1 1))
              (make-posn 1 0))
(check-expect (posn-div (make-posn -3 -5) (make-posn -5 -7))
              (make-posn 25/37 2/37))
(check-expect (posn-div (make-posn 3 5) (make-posn -5 -7))
              (make-posn -25/37 -2/37))


;; ==== Question 2c ========================

;; (posn-reciprocal posn) outputs the reciprocal 
;;   of posn using the formula:
;;   (1/(x,y)) = (x/(x^2 + y^2) , -y/(x^2 + y^2))
;; posn-reciprocal: Posn -> Posn
;; requires: (posn-x posn) or (posn-y posn) must be a non-zero real number
;; Examples:
(check-expect (posn-reciprocal (make-posn -3 5)) (make-posn -3/34 -5/34))
(check-expect (posn-reciprocal (make-posn 10 20)) (make-posn 0.02 -0.04))

(define (posn-reciprocal posn)
  (make-posn (/ (posn-x posn)
                (+ (sqr (posn-x posn)) (sqr (posn-y posn))))
             (/ (-(posn-y posn))
                (+ (sqr (posn-x posn)) (sqr (posn-y posn))))))

;; Tests:
(check-expect (posn-reciprocal (make-posn 0 1)) (make-posn 0 -1))
(check-expect (posn-reciprocal (make-posn 1 0)) (make-posn 1 0))
(check-expect (posn-reciprocal (make-posn -4 -1)) (make-posn -4/17 1/17))
(check-expect (posn-reciprocal (make-posn -5 7)) (make-posn -5/74 -7/74))







