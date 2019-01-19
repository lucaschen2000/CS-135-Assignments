;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ranking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f () #f)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 08, Question 4
;; *****************************************
;;

(require "provide.rkt")
(provide expunge)
(provide ranking-sort)
(provide unfold)
(provide test-employers)
(provide test-flat-employers)
(provide test-students)

;; An employer ID, EmpId, is a Str (e.g. "Google")
;; A student ID, StdId, is a Str (e.g. "Anna")
;; An Id is (anyof EmpId StdId)
;; A preference, Pref, is a Nat (e.g. 2)
;; requires: a preference is >= 1

;; An EmpRanking is (list EmpId (listof (list StdId Pref)))
;; requires: The list of preferences is in non-decreasing order
;;           and non-empty.

;; A StdRanking is (list StdId (listof (list EmpId Pref)))
;; requires: The list of preferences is in non-decreasing order
;;           and non-empty.

;; A Ranking is (anyof EmpRanking StdRanking)

;; A FlatRanking is a (list Str Str Num).

(define test-employers
  '(("General" (("Alice" 1) ("Chance" 2) ("Frank" 2)))
    ("Just" (("Bob" 2) ("Emily" 1)))
    ("Harmony" (("Frank" 1) ("Chance" 3) ("Dennis" 2)))
    ("Information" (("Bob" 1) ("Alice" 2)))
    ))

(define test-students
  '(("Alice" (("General" 1) ("Information" 2)))
    ("Bob" (("Just" 1) ("Information" 1)))
    ("Chance" (("Harmony" 1) ("General" 2)))
    ("Dennis" (("Harmony" 1)))
    ("Emily" (("Just" 1)))
    ("Frank" (("General" 1) ("Harmony" 1)))
    ))

(define test-flat-employers
  '(("General" "Alice" 1)
    ("General" "Chance" 2)
    ("General" "Frank" 2)
    ("Just" "Bob" 2)
    ("Just" "Emily" 1)
    ("Harmony" "Frank" 1)
    ("Harmony" "Chance" 3)
    ("Harmony" "Dennis" 2)
    ("Information" "Bob" 1)
    ("Information" "Alice" 2)))


;; ==== Question 4a ========================

;; (expunge flatrankings ranking-id ranked-id) produces the same data but
;;   without elements containing ranking-id or ranked-id
;; expunge: (listof FlatRanking) Id Id -> (listof FlatRanking)
;; Examples:
(check-expect (expunge test-flat-employers "General" "Alice")
              '(("Just" "Bob" 2)
                ("Just" "Emily" 1)
                ("Harmony" "Frank" 1)
                ("Harmony" "Chance" 3)
                ("Harmony" "Dennis" 2)
                ("Information" "Bob" 1)))

(define (expunge flatrankings ranking-id ranked-id)
  (foldr (lambda (x rest) (cond [(or (string=? ranking-id (first x))
                                     (string=? ranked-id (second x))) rest]
                                [else (cons x rest)])) empty flatrankings))

;; Tests:
(check-expect (expunge empty "null" "null")
              empty)
(check-expect (expunge '(("General" "Alice" 1)) "null" "null")
              '(("General" "Alice" 1)))
(check-expect (expunge '(("General" "Alice" 1)) "null" "Alice")
              empty)
(check-expect (expunge '(("General" "Alice" 1)) "General" "Alice")
              empty)
(check-expect (expunge test-flat-employers "Not in list" "Not in list")
              test-flat-employers)
(check-expect (expunge test-flat-employers "Not in list" "Alice")
              '(("General" "Chance" 2)
                ("General" "Frank" 2)
                ("Just" "Bob" 2)
                ("Just" "Emily" 1)
                ("Harmony" "Frank" 1)
                ("Harmony" "Chance" 3)
                ("Harmony" "Dennis" 2)
                ("Information" "Bob" 1)))
(check-expect (expunge test-flat-employers "General" "Not in list")
              '(("Just" "Bob" 2)
                ("Just" "Emily" 1)
                ("Harmony" "Frank" 1)
                ("Harmony" "Chance" 3)
                ("Harmony" "Dennis" 2)
                ("Information" "Bob" 1)
                ("Information" "Alice" 2)))


;; ==== Question 4b ========================

;; (ranking-sort flatrankings) produces the same data but reordered so that the
;;   ranking IDs (EmpId) are in increasing alphabetical order, for any ties,
;;   order is determined by ranked IDs (StdId)
;; ranking-sort: (listof FlatRanking) -> (listof FlatRanking)
;; Examples:
(check-expect (ranking-sort test-flat-employers)
              '(("General" "Alice" 1)
                ("General" "Chance" 2)
                ("General" "Frank" 2)
                ("Harmony" "Chance" 3)
                ("Harmony" "Dennis" 2)
                ("Harmony" "Frank" 1)
                ("Information" "Alice" 2)
                ("Information" "Bob" 1)
                ("Just" "Bob" 2)
                ("Just" "Emily" 1)))

(define (ranking-sort flatrankings)
  (foldr (lambda (x appended-list) (foldr cons appended-list x)) empty
         (map
          (lambda (same-ranking-ids)
            (foldr
             (lambda (x appended-list) (foldr cons appended-list x)) empty
             (map (lambda (ranked-id)
                    (filter (lambda (same-ranking-id)
                              (string=? ranked-id
                                        (second same-ranking-id)))
                            same-ranking-ids))
                  (quicksort (map second same-ranking-ids) string<?))))
          (map (lambda (ranking-id)
                 (filter (lambda (flatranking)
                           (string=? ranking-id (first flatranking)))
                         flatrankings))
               (foldr (lambda (dedup-ranking-id unduplicated-list)
                        (cond [(empty? (filter
                                        (lambda (element)
                                          (equal? element dedup-ranking-id))
                                        unduplicated-list))
                               (cons dedup-ranking-id unduplicated-list)]
                              [else unduplicated-list]))
                      empty (quicksort (map first flatrankings) string<?))))))

;; Tests:
(check-expect (ranking-sort '()) '())
(check-expect (ranking-sort '(("A" "b" 0))) '(("A" "b" 0)))
(check-expect (ranking-sort '(("H" "b" 1)
                              ("H" "u" 2)
                              ("H" "a" 3)
                              ("H" "i" 4)))
              '(("H" "a" 3)
                ("H" "b" 1)
                ("H" "i" 4)
                ("H" "u" 2)))
(check-expect (ranking-sort '(("B" "b" 1)
                              ("A" "b" 2)
                              ("B" "a" 3)
                              ("A" "a" 4)))
              '(("A" "a" 4)
                ("A" "b" 2)
                ("B" "a" 3)
                ("B" "b" 1)))
(check-expect (ranking-sort '(("G" "c" 1)
                              ("H" "a" 2)
                              ("G" "a" 3)))
              '(("G" "a" 3)
                ("G" "c" 1)
                ("H" "a" 2)))


;; ==== Question 4c ========================

;; (unfold rankings) produces an equivalent ranking list in FlatRanking format
;; unfold: (listof Ranking) -> (listof FlatRanking)
;; Examples:
(check-expect (unfold test-employers) test-flat-employers)

(define (unfold rankings)
  (foldr (lambda (a b) (append a b)) empty
         (map (lambda (ranking)
                (map (lambda (ranked) (cons (first ranking) ranked))
                     (second ranking)))
              rankings)))

;; Tests:
(check-expect (unfold empty) empty)
(check-expect (unfold
               '(("Test" (("Apple" 1)))))
              '(("Test" "Apple" 1)))
(check-expect (unfold
               '(("Test" (("Apple" 1) ("Bees" 2) ("Cat" 3)))))
              '(("Test" "Apple" 1)
                ("Test" "Bees" 2)
                ("Test" "Cat" 3)))
(check-expect (unfold test-students)
              '(("Alice" "General" 1)
                ("Alice" "Information" 2)
                ("Bob" "Just" 1)
                ("Bob" "Information" 1)
                ("Chance" "Harmony" 1)
                ("Chance" "General" 2)
                ("Dennis" "Harmony" 1)
                ("Emily" "Just" 1)
                ("Frank" "General" 1)
                ("Frank" "Harmony" 1)))


;; ==== Question 4d ========================

;; (find key list-of-keys) produces the first element within
;;   list-of-keys that has a first element equal to key, false
;;   if there is no such element
;; find: Any (listof (listof Any)) -> (listof Any)
;; Examples:
(check-expect (find "Just" test-employers)
              '("Just" (("Bob" 2) ("Emily" 1))))
(check-expect (find "Bob" test-students)
              '("Bob" (("Just" 1) ("Information" 1))))
(check-expect (find "General" test-flat-employers)
              '("General" "Alice" 1))

(define (find key list-of-keys)
  (foldr (lambda (current-key rest-of-keys)
           (cond [(empty? current-key) rest-of-keys]
                 [(equal? key (first current-key)) current-key]
                 [else rest-of-keys]))
         false list-of-keys))

;; Tests:
(check-expect (find "Lucas" '()) false)
(check-expect (find "Lucas" '(())) false)
(check-expect (find "Lucas" '(("Lucas" 123))) '("Lucas" 123))
(check-expect (find "Lucas" '(()("Lucas" 123))) '("Lucas" 123))
(check-expect (find "Lucas" '(("Bob" cat)()("Cow" "milk"))) false)
(check-expect (find "Lucas" '(("Bob" cat)("Lucas" 123))) '("Lucas" 123))
(check-expect (find 3 '(("Bob" cat)("Lucas" 123))) false)
(check-expect (find 3 '(("Bob" cat)()("Lucas" 123)(3 brain))) '(3 brain))
(check-expect (find 'blue '(("Bob" cat)("Lucas" 123)(3 brain))) false)
(check-expect (find 'blue '((blue cat)("Lucas" 123)(3 brain))) '(blue cat))
(check-expect (find '(black) '((blue cat)("Lucas" 123)((black) "wow")))
              '((black) "wow"))
(check-expect (find '("black") '((blue cat)("Lucas" 123)(("black") "wow")))
              '(("black") "wow"))
(check-expect (find '() '((blue cat)("Lucas" 123)(() "wow"))) '(() "wow"))
(check-expect (find '() '((blue cat)("Lucas" 123)(() ((()))))) '(() ((()))))


;; ==== Question 4e ========================

;; (find-pref rankings ranking-id ranked-id) produces the Pref of
;;   ranking-id for ranked-id in rankings
;; find-pref: (listof Ranking) Id Id -> (listof Ranking)
;; requires: Pref of ranking-id for ranked-id must exist in rankings
;; Examples:
(check-expect (find-pref test-employers "General" "Chance") 2)

(define (find-pref rankings ranking-id ranked-id)
  (second
   (first
    (filter
     (lambda (ranked) (string=? (first ranked) ranked-id))
     (second (first
              (filter (lambda (ranking) (string=? (first ranking) ranking-id))
                      rankings)))))))

;; Tests:
(check-expect (find-pref test-employers "Just" "Emily") 1)
(check-expect (find-pref test-students "Alice" "Information") 2)
(check-expect (find-pref test-students "Emily" "Just") 1)