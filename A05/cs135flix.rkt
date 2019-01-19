;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135flix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 05, Question 3
;; *****************************************
;;

;; A Movie-fv is a (list Nat Nat Nat Nat Nat Nat Nat Nat)

;; ==== Question 3a ========================

;; if list 2 is empty, just return what we had before

;; (add-list list1 list2) adds the integers in
;;   list1 with the numbers in list two and
;;   returns a resulting summation list
;; add-list: (listof Int) (listof Int) -> (listof Int)
;; requires: list1 and list2 to be equal length
;; Examples:
(check-expect (add-list (list 1 1 0 0 0 0 0 0) (list 1 1 1 0 0 0 0 0))
              (list 2 2 1 0 0 0 0 0))

(define (add-list list1 list2)
  (cond [(empty? list2) list1]
        [else (cons (+ (first list1) (first list2))
                    (add-list (rest list1) (rest list2)))]))

;; (negate-list list) takes each integer
;;   in list and multiples it by -1
;;   to make a resulting negated list
;; negate-list: (listof Int) -> (listof Int)
;; Examples:
(check-expect (negate-list (list 1 1 0 0 0 0 0 0))
              (list -1 -1 0 0 0 0 0 0))

(define (negate-list list)
  (cond [(empty? list) empty]
        [else (cons (-(first list)) (negate-list (rest list)))]))

;; A Rating is an Int
;; requires: Int is either -1 or 1

;; A Pref-v is a (list Int Int Int Int Int Int Int Int)

;; (find-preference ratings movie-vectors) outputs a preference
;;   vector with a score corresponding to how much an individual
;;   likes or dislikes each movie genre
;; find-preference: (listof Rating) (listof Movie-fv) -> Pref-v
;; requires: both lists are the same size
;;           both lists are non-empty
;; Examples:
(check-expect (find-preference (list 1)
                               (list (list 1 1 1 1 1 1 1 1)))
              (list 1 1 1 1 1 1 1 1))
(check-expect (find-preference (list 1 1 -1)
                               (list (list 1 1 0 0 0 0 0 0)
                                     (list 1 1 1 0 0 0 0 0)
                                     (list 0 1 1 1 0 0 0 0)))
              (list 2 1 0 -1 0 0 0 0))

(define liked-movie 1)

(define (find-preference ratings movie-vectors)
  (cond [(empty? ratings) empty]
        [(= (first ratings) liked-movie)
         (add-list (first movie-vectors)
                   (find-preference (rest ratings) (rest movie-vectors)))]
        [else
         (add-list (negate-list (first movie-vectors))
                   (find-preference (rest ratings) (rest movie-vectors)))]))

;; Tests:
(check-expect (find-preference (list 1)
                               (list (list 0 0 0 0 0 0 0 0)))
              (list 0 0 0 0 0 0 0 0))
(check-expect (find-preference (list -1)
                               (list (list 1 1 1 1 1 1 1 1)))
              (list -1 -1 -1 -1 -1 -1 -1 -1))
(check-expect (find-preference (list 1 1)
                               (list (list 1 1 0 0 0 1 1 0)
                                     (list 0 0 0 0 0 1 1 1)))
              (list 1 1 0 0 0 2 2 1))
(check-expect (find-preference (list -1 -1)
                               (list (list 1 1 0 0 0 0 0 1)
                                     (list 0 0 0 1 1 0 1 1)))
              (list -1 -1 0 -1 -1 0 -1 -2))
(check-expect (find-preference (list -1 1 -1 -1)
                               (list (list 1 1 0 0 0 0 0 0)
                                     (list 0 0 0 0 0 0 0 0)
                                     (list 0 1 1 1 0 0 0 0)
                                     (list 0 0 0 0 1 1 1 1)))
              (list -1 -2 -1 -1 -1 -1 -1 -1))


;; ==== Question 3b ========================

(define-struct movie (title genres))
;; A Movie is a (make-movie Str Movie-fv)

;; (dot-product lon1 lon2) takes the dot product
;;   of lon1 and lon2
;; dot-product: (listof Num) (listof Num) -> Num
;; requires: both lists to be equal length
;; Examples:
(check-expect (dot-product (list 1 -2) (list 3 -4)) 11)
(check-expect (dot-product (list 3 -2) (list 0 10)) -20)

(define end-of-dot-product 0)

(define (dot-product lon1 lon2)
  (cond [(empty? lon1) end-of-dot-product]
        [else (+ (* (first lon1) (first lon2))
                 (dot-product (rest lon1) (rest lon2)))]))

;; (list-of-scores preference-vector movies) outputs a list
;;   of the dot products between preference-vector and each
;;   movie in movies
;; list-of-scores: Pref-v (listof Movie) -> (listof Int)
;; Examples:
(check-expect
 (list-of-scores (list 2 1 0 -1 0 0 0 0)
                 (list (make-movie "The Meg"        (list 1 0 0 0 1 0 1 0))
                       (make-movie "Smallfoot"      (list 0 1 1 0 0 0 0 0))
                       (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
 (list 2 1 -1))

(define (list-of-scores preference-vector movies)
  (cond [(empty? movies) empty]
        [else (cons (dot-product preference-vector
                                 (movie-genres (first movies)))
                    (list-of-scores preference-vector (rest movies)))]))

;; (highest-score list) outputs the largest
;;   value in list
;; highest-score: (listof Int) -> Int
;; requires: list to be non-empty
(check-expect (highest-score (list 4 1 3 19)) 19)

(define (highest-score list)
  (cond [(empty? (rest list)) (first list)]
        [(>= (first list) (second list))
         (highest-score (cons (first list) (rest (rest list))))]
        [else (highest-score (rest list))]))

;; (suggestions preference-vector movies) outputs the title of
;;   the movie which best matches preference-vector (determined
;;   by largest dot product between preference-vector and movies)
;; suggestions: Pref-v (listof Movie) -> Str
;; requires: (listof Movie) is non-empty
;; Examples:

(define (suggestions preference-vector movies)
  (cond [(= (dot-product preference-vector (movie-genres (first movies)))
            (highest-score (list-of-scores preference-vector movies)))
         (movie-title (first movies))]
        [else (suggestions preference-vector (rest movies))]))

;; Tests:
(check-expect (suggestions (list 0 0 0 0 0 0 0 0)
 (list (make-movie "Zero" (list 0 0 0 0 0 0 0 0))))
              "Zero")
(check-expect (suggestions (list 100 100 100 100 100 100 100 100)
 (list (make-movie "The One" (list -1 -1 -1 -1 -1 -1 -1 -1))))
              "The One")
(check-expect (suggestions (list 2 1 0 -1 0 0 0 0)
 (list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
       (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
       (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              "The Meg")
(check-expect (suggestions (list -2 6 2 0 0 0 0 0)
 (list (make-movie "Hamster" (list 1 0 0 0 1 0 1 0))
       (make-movie "Duck" (list 0 1 1 0 0 0 0 0))
       (make-movie "Goose" (list 0 0 0 1 0 1 0 0))))
              "Duck")
(check-expect (suggestions (list 1 2 3 4 5 6 7 8)
 (list (make-movie "A" (list 1 0 0 0 0 0 0 0))
       (make-movie "B" (list 0 1 0 0 0 0 0 0))
       (make-movie "C" (list 0 0 1 0 0 0 0 0))
       (make-movie "D" (list 0 0 0 1 0 0 0 0))
       (make-movie "E" (list 0 0 0 0 1 0 0 0))
       (make-movie "F" (list 0 0 0 0 0 1 0 0))
       (make-movie "G" (list 0 0 0 0 0 0 1 0))
       (make-movie "H" (list 0 0 0 0 0 0 0 1)))) "H")