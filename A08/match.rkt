;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname match) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 08, Question 5
;; *****************************************
;;

(require "a08helpers.rkt")
(require "ranking.rkt")

;; (switch flatranking-list) switches the first and second elements of
;;   each FlatRanking in flatranking-list (i.e. switches ranking-id with
;;   ranked-id)
;; switch: (listof FlatRanking) -> (listof FlatRanking)
;; Examples:
(check-expect (switch test-flat-employers)
              '(("Alice" "General" 1)
                ("Chance" "General" 2)
                ("Frank" "General" 2)
                ("Bob" "Just" 2)
                ("Emily" "Just" 1)
                ("Frank" "Harmony" 1)
                ("Chance" "Harmony" 3)
                ("Dennis" "Harmony" 2)
                ("Bob" "Information" 1)
                ("Alice" "Information" 2)))

(define (switch flatranking-list)
  (map (lambda (flat-ranking)
         (list (second flat-ranking) (first flat-ranking) (third flat-ranking)))
       flatranking-list))

;; (pref-sort flatrankings) sorts each flatranking in flatrankings
;;   according to the third element (a number) of each flatranking
;;   in increasing order
;; pref-sort: (listof FlatRanking) -> (listof FlatRanking)
;; requires: each number (3rd element of flatranking)
;;           in flatrankings must be unique
;; Examples:
(check-expect (pref-sort '(("A" "a" 1)
                           ("B" "b" 3)
                           ("C" "c" 2)))
              '(("A" "a" 1)
                ("C" "c" 2)
                ("B" "b" 3)))

(define (pref-sort flatrankings)
  (map (lambda (preference)
         (first
          (filter (lambda (flatranking) (= preference (third flatranking)))
                  flatrankings)))
       (quicksort (map third flatrankings) <)))

;; (combine-sort employers students) outputs a list of pairs of an employer
;;   and a student, in increasing order of the sum of eachother's preferences
;;   in employers and students, with a random generator to break ties
;; combine-sort: (listof EmpRanking) (listof StdRanking) ->
;;                 (listof (list EmpId StdId))
;; requires: every student ranked in employers is present in students
;;           and ranks that employer
;; Examples:
(check-expect (combine-sort test-employers test-students)
              '(("Just" "Emily")
                ("General" "Alice")
                ("Harmony" "Frank")
                ("Information" "Bob")
                ("Just" "Bob")
                ("General" "Frank")
                ("Harmony" "Dennis")
                ("General" "Chance")
                ("Harmony" "Chance")
                ("Information" "Alice")))

(define (combine-sort employers students)
  (map (lambda (sorted-flatranking)
         (list (first sorted-flatranking) (second sorted-flatranking)))
       (pref-sort
        (map (lambda (sorted-employers sorted-students)
               (list (first sorted-employers) (second sorted-employers)
                     (+ (third sorted-employers) (third sorted-students)
                        (random-epsilon (first sorted-employers)
                                        (second sorted-employers)))))
             (ranking-sort (unfold employers))
             (ranking-sort (switch (unfold students)))))))

;; (match employers students) lowest Pref sums matched first, produces list
;;   of pairs of employer from employers and student from students, after match,
;;   both employer and student are removed from the pool of potential matches
;; match: (listof EmpRanking) (listof StdRanking) -> (listof (list EmpId StdId))
;; requires: every student ranked in employers is present in students
;;           and ranks that employer
;; Examples:
(check-expect (match test-employers test-students)
              '(("Information" "Bob")
                ("Harmony" "Frank")
                ("General" "Alice")
                ("Just" "Emily")))

(define (match employers students)
  (foldl
   (lambda (x matched-ids)
     (cond [(and (empty? (filter (lambda (element) (equal? element (first x)))
                                 (map first matched-ids)))
                 (empty? (filter (lambda (element) (equal? element (second x)))
                                 (map second matched-ids))))
            (cons x matched-ids)]
           [else matched-ids]))
   empty (combine-sort employers students)))

;; Tests:
(check-expect (match empty empty) empty)
(check-expect (match '(("Com A" (("Std A" 1) ("Std B" 2))))
                '(("Std A" (("Com A" 1)))
                  ("Std B" (("Com A" 1)))))
              '(("Com A" "Std A")))
(check-expect (match '(("Com A" (("Std A" 1)))
                       ("Com B" (("Std A" 1))))
                '(("Std A" (("Com A" 1) ("Com B" 2)))))
              '(("Com A" "Std A")))
(check-expect (match '(("Com A" (("Std A" 1) ("Std B" 2))))
                '(("Std A" (("Com A" 3)))
                  ("Std B" (("Com A" 1)))))
              '(("Com A" "Std B")))
(check-expect (match '(("Com A" (("Std A" 1) ("Std B" 2))))
                '(("Std A" (("Com A" 2)))
                  ("Std B" (("Com A" 1)))))
              '(("Com A" "Std A")))
(check-expect (match '(("Com A" (("Std A" 4)
                                 ("Std B" 1)
                                 ("Std C" 3)))
                       ("Com B" (("Std A" 1)
                                 ("Std C" 2)
                                 ("Std D" 2)))
                       ("Com C" (("Std D" 2)
                                 ("Std E" 1))))
                '(("Std A" (("Com A" 1)
                            ("Com B" 2)))
                  ("Std B" (("Com A" 3)))
                  ("Std C" (("Com A" 4)
                            ("Com B" 5)))
                  ("Std D" (("Com B" 2)
                            ("Com C" 3)))
                  ("Std E" (("Com C" 5)))))
              '(("Com C" "Std D")
                ("Com A" "Std B")
                ("Com B" "Std A")))
(check-expect (match '(("Com A" (("Std A" 4)
                                 ("Std B" 1)
                                 ("Std C" 3)))
                       ("Com B" (("Std A" 1)
                                 ("Std C" 2)
                                 ("Std D" 2)))
                       ("Com C" (("Std D" 2)
                                 ("Std E" 1))))
                '(("Std A" (("Com A" 1)
                            ("Com B" 2)))
                  ("Std B" (("Com A" 3)))
                  ("Std C" (("Com A" 4)
                            ("Com B" 5)))
                  ("Std D" (("Com B" 2)
                            ("Com C" 3)))
                  ("Std E" (("Com C" 5)))))
              '(("Com C" "Std D")
                ("Com A" "Std B")
                ("Com B" "Std A")))




