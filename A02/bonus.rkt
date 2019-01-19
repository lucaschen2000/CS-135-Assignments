;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 02, Bonus question
;; *****************************************
;;

;; ==== Bonus question ========================

(define (year date)
  (quotient date 10000))

(define (day date)
  (remainder date 100))

(define (month date)
  (/ (- (remainder date 10000) (day date)) 100))

(define (adjusted-year date)
  (cond [(<= (month date) 2) (- (year date) 1)]
        [else (year date)]))

(define (adjusted-month date)
  (cond [(<= (month date) 2) (+ (month date) 12)]
        [else (month date)]))

(adjusted-month 20000328)

(define (year-of-century date)
  (remainder (adjusted-year date) 100))

(define (zero-based-century date)
  (floor (/ (adjusted-year date) 100)))

(define (day-of-week date)
  (remainder (- (+ (day date) (floor (* 13/5 (+ (adjusted-month date) 1)))
     (year-of-century date)
     (floor (/ (year-of-century date) 4))
     (floor (/ (zero-based-century date) 4)))
     (* 2 (zero-based-century date))) 7))

;; (day->day-of-week date) outputs the day of
;;   the week given a date in the form yyyymmdd
(define (date->day-of-week date)
  (cond [(= (day-of-week date) 0) 'Saturday]
        [(= (day-of-week date) 1) 'Sunday]
        [(= (day-of-week date) 2) 'Monday]
        [(= (day-of-week date) 3) 'Tuesday]
        [(= (day-of-week date) 4) 'Wednesday]
        [(= (day-of-week date) 5) 'Thursday]
        [(= (day-of-week date) 6) 'Friday]))

;; This function uses Zeller's congruence formula

;; Citation:
;; Wikipedia contributors. "Zeller's congruence."
;;   Wikipedia, The Free Encyclopedia. Wikipedia, The Free Encyclopedia,
;;   24 Nov. 2017. Web. 25 Sep. 2018.