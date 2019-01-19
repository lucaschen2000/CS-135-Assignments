;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 08, Question 2
;; *****************************************
;;

;; ==== Question 2a ========================

;; (occurrences lon num) produces the number of times that num
;;   occurs in lon
;; occurrences: (listof Num) Num -> Nat
;; Examples:
(check-expect (occurrences '(1 2 5 2 5 3 1 5 5) 5) 4)

(define (occurrences lon num)
  (foldr (lambda (current rest)
           (cond [(= current num) (add1 rest)]
                 [else rest])) 0 lon))

;; Tests:
(check-expect (occurrences empty 5) 0)
(check-expect (occurrences '(0) 0) 1)
(check-expect (occurrences '(0) 1) 0)
(check-expect (occurrences '(1 2 3) 0) 0)
(check-expect (occurrences '(1 2 3) 3) 1)
(check-expect (occurrences '(1 2 1 3 1 4 1 1) 1) 5)
(check-expect (occurrences '(1.1 2 1.2 3 1.4 4 1 1) 1) 2)


;; ==== Question 2b ========================

;; (absolutely-odd loi) produces the sum of the absolute
;;   values of odd integers in loi
;; absolutely-odd: (listof Int) -> Nat
;; Examples:
(check-expect (absolutely-odd '(1 2 1 5 -1 -2 -5 0 3 7)) 23)

(define (absolutely-odd loi)
  (foldr + 0 (map abs (filter odd? loi))))

;; Tests:
(check-expect (absolutely-odd empty) 0)
(check-expect (absolutely-odd '(0)) 0)
(check-expect (absolutely-odd '(1)) 1)
(check-expect (absolutely-odd '(1 2 3)) 4)
(check-expect (absolutely-odd '(-1 -2 -3)) 4)
(check-expect (absolutely-odd '(-1 2 -3 4 -5)) 9)
(check-expect (absolutely-odd '(7 -4 -3 -7 -8)) 17)


;; ==== Question 2c ========================

;; (zip list1 list2) produces a list of pairs where the ith pair contains the
;;   ith element of list1 followed by the ith element of list2
;; zip: (listof Any) (listof Any) -> (listof (list Any Any))
;; requires: list1 and list2 are the same length
;; Examples:
(check-expect (zip '(w a t e r l o o) '(1 2 3 4 5 6 7 8))
              '((w 1) (a 2) (t 3) (e 4) (r 5) (l 6) (o 7) (o 8)))

(define (zip list1 list2)
  (map list list1 list2))

;; Tests:
(check-expect (zip empty empty) empty)
(check-expect (zip '(0) '(2)) '((0 2)))
(check-expect (zip '(()()) '(()())) '((()())(()())))
(check-expect (zip '((lst) hi) '(not-list (hi)))
              '(((lst) not-list) (hi (hi))))
(check-expect (zip '(1 2 3 4 5) '(A B C D E))
              '((1 A) (2 B) (3 C) (4 D) (5 E)))
(check-expect (zip '(3 2 1) '(c b a)) '((3 c)(2 b)(1 a)))


;; ==== Question 2d ========================

;; (unzip pairs) produces a list of two lists, the first contains the
;;   first element from each pair in pairs and the second contains the
;;   second element from each pair in pairs, in the original order
;; unzip: (listof (list Any Any)) -> (listof Any) (listof Any)
;; Examples:
(check-expect (unzip '((w 1) (a 2) (t 3) (e 4) (r 5) (l 6) (o 7) (o 8)))
              '((w a t e r l o o) (1 2 3 4 5 6 7 8)))

(define (unzip pairs)
  (list (foldr (lambda (current rest)
                 (cons (first current) rest)) empty pairs)
        (foldr (lambda (current rest)
                 (cons (second current) rest)) empty pairs)))

;; Tests:
(check-expect (unzip empty) (list empty empty))
(check-expect (unzip '((0 2))) '((0) (2)))
(check-expect (unzip '((()())(()()))) '((()()) (()())))
(check-expect (unzip '(((lst) hi) (not-list (hi))))
              '(((lst) not-list) (hi (hi))))
(check-expect (unzip '((1 A) (2 B) (3 C) (4 D) (5 E)))
              (list '(1 2 3 4 5) '(A B C D E)))
(check-expect (unzip '((3 c)(2 b)(1 a))) '((3 2 1) (c b a)))
(check-expect (unzip '((10 10) (11 11))) '((10 11)(10 11)))


;; ==== Question 2e ========================

;; (dedup lon) produces a new list with only the first occurence
;;   of each element of the original list
;; dedup: (listof Num) -> (listof Num)
;; Examples:
(check-expect (dedup '(1 5 2 5 3 7 3 8 9 1)) '(1 5 2 3 7 8 9))

(define (dedup lon)
  (foldl cons empty 
         (foldl (lambda (current rest)
                  (cond [(empty? (filter (lambda (element)
                                           (equal? element current)) rest))
                         (cons current rest)]
                        [else rest]))
                empty lon)))

;; Tests:
(check-expect (dedup empty) empty)
(check-expect (dedup '(0)) '(0))
(check-expect (dedup '(1.1 2.2 3.3 2.2 1.1 3.3)) '(1.1 2.2 3.3))
(check-expect (dedup '(0 0 1 1 2 2 3 3)) '(0 1 2 3))
(check-expect (dedup '(3 3 2 2 1 1 0 0)) '(3 2 1 0))
(check-expect (dedup '(8 1 5 2 4 5 3 2 5 2)) '(8 1 5 2 4 3))


;; ==== Question 2f ========================

;; (subsequence lst from to) produces the subsequence from lst that
;;   begins at index from and ends just before index to, indexing starts
;;   at 0
;; subsequence: (listof Any) Nat Nat -> (listof Any)
;; Examples:
(check-expect (subsequence '(A B C D E F G) 3 5) '(D E))

(define (subsequence lst from to)
  (map second
       (foldr (lambda (current rest)
                (cond [(>= (first current) to) empty]
                      [(>= (first current) from) (cons current rest)]
                      [else rest]))
              empty (map list (build-list (length lst) identity) lst))))

;; Tests:
(check-expect (subsequence empty 0 2) empty)
(check-expect (subsequence '(A) 0 0) empty)
(check-expect (subsequence '(A) 0 1) '(A))
(check-expect (subsequence '(100 101 102) 1 3) '(101 102))
(check-expect (subsequence '(() (hi) (bye)) 0 2) '(() (hi)))
(check-expect (subsequence '(A B C D E) 2 4) '(C D))
(check-expect (subsequence '(A B C D E F G) 4 4) '())
(check-expect (subsequence '(A B C D E F G) 4 2) '())
(check-expect (subsequence '(A B C D E F G H I J K) 0 999)
              '(A B C D E F G H I J K))