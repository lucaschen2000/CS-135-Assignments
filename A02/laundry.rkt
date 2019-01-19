;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname laundry) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 02, Question 3
;; *****************************************
;;

;; ==== Question 3a ========================

;; (acceptable-to-wear/cond? smell clothing-type days)
;;   indicates if a piece of clothing is wearable
;;   based on the smell, clothing-type, and number of
;;   days that a clothing item has been in the hamper
;; acceptable-to-wear/cond?: Bool Sym Nat -> Bool
;; requires: clothing-type is any of 'shirt or 'socks
;; Examples:
(check-expect (acceptable-to-wear/cond? false 'socks 2) true)
(check-expect (acceptable-to-wear/cond? true 'shirt 0) false)

(define (acceptable-to-wear/cond? smell clothing-type days)
  (cond [smell false]
        [(symbol=? clothing-type 'socks) (cond [(>= days 4) false]
                                               [else true])]
        [(symbol=? clothing-type 'shirt) (cond [(>= days 10) false]
                                               [(<= days 2) false]
                                               [else true])]))

;; Tests:
(check-expect (acceptable-to-wear/cond? false 'socks 4) false)
(check-expect (acceptable-to-wear/cond? false 'socks 5) false)
(check-expect (acceptable-to-wear/cond? false 'shirt 0) false)
(check-expect (acceptable-to-wear/cond? false 'shirt 2) false)
(check-expect (acceptable-to-wear/cond? false 'shirt 5) true)
(check-expect (acceptable-to-wear/cond? false 'shirt 10) false)
(check-expect (acceptable-to-wear/cond? false 'shirt 12) false)


;; ==== Question 3b ========================

;; (acceptable-to-wear/bool? smell clothing-type days)
;;   indicates if a piece of clothing is wearable
;;   based on the smell, clothing-type, and number of
;;   days that a clothing item has been in the hamper
;; acceptable-to-wear/cond?: Bool Sym Nat -> Bool
;; requires: clothing-type is any of 'shirt or 'socks
;; Examples:
(check-expect (acceptable-to-wear/bool? false 'socks 2) true)
(check-expect (acceptable-to-wear/bool? true 'shirt 0) false)
               
(define (acceptable-to-wear/bool? smell clothing-type days)
  (or (and (not smell)(symbol=? clothing-type 'shirt)(< 2 days 10))
      (and (not smell)(symbol=? clothing-type 'socks)(< days 4))))

;; Tests:
(check-expect (acceptable-to-wear/bool? false 'socks 4) false)
(check-expect (acceptable-to-wear/bool? false 'socks 5) false)
(check-expect (acceptable-to-wear/bool? false 'shirt 0) false)
(check-expect (acceptable-to-wear/bool? false 'shirt 2) false)
(check-expect (acceptable-to-wear/bool? false 'shirt 5) true)
(check-expect (acceptable-to-wear/bool? false 'shirt 10) false)
(check-expect (acceptable-to-wear/bool? false 'shirt 12) false)