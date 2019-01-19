;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 08, Question 3
;; *****************************************
;;

;; (partition pred lst) outputs a two element list where the first element is
;;   a list of items in lst that satisfy pred and the second element
;;   is a list of items in lst that does not satisfy pred
;; partition: (X -> Bool) (listof X) -> (list (listof X) (listof X))
;; Examples:
(check-expect (partition (lambda (x) (= 0 (remainder x 4)))
                         '(1 2 3 4 5 6 7 8 9 10))
              '((4 8) (1 2 3 5 6 7 9 10)))

(define (partition pred lst)
  (local [;; (satisfies pred lst) outputs a list of the elements in lst
          ;;   that satisfy the predicate pred
          ;; satisfies: (X -> Bool) (listof X) -> (listof X)
          (define (satisfies pred lst)
            (cond [(empty? lst) empty]
                  [(pred (first lst))
                   (cons (first lst) (satisfies pred (rest lst)))]
                  [else (satisfies pred (rest lst))]))
          ;; (dissatisfies pred lst )outputs a list of the elements in lst
          ;;   that do not satisfy the predicate pred
          ;; dissatisfies: (X -> Bool) (listof X) -> (listof X)
          (define (dissatisfies pred lst)
            (cond [(empty? lst) empty]
                  [(not (pred (first lst)))
                   (cons (first lst) (dissatisfies pred (rest lst)))]
                  [else (dissatisfies pred (rest lst))]))]
    (list (satisfies pred lst) (dissatisfies pred lst))))

;; Tests:
(check-expect (partition boolean? empty) '(() ()))
(check-expect (partition zero? '())
              '(() ()))
(check-expect (partition odd? '(1))
              '((1) ()))
(check-expect (partition odd? '(0))
              '(() (0)))
(check-expect (partition odd? '(1 2 3 4 5 6 7 8 9 10))
              '((1 3 5 7 9) (2 4 6 8 10)))
(check-expect (partition even? '(1 2 3 4 5 6 7 8 9 10))
              '((2 4 6 8 10) (1 3 5 7 9)))
(check-expect (partition number? '(1 2 3 4 5 6 7 8 9 10))
              '((1 2 3 4 5 6 7 8 9 10) ()))
(check-expect (partition zero? '(1 2 3 4 5 6 7 8 9 10))
              '(() (1 2 3 4 5 6 7 8 9 10)))
(check-expect (partition list? '(() (a) (b) c d e))
              '((() (a) (b)) (c d e)))
(check-expect (partition empty? '(() (a) (b) c d e))
              '((()) ((a) (b) c d e)))
(check-expect (partition symbol? '(() (a) (b) c d e))
              '((c d e) (() (a) (b))))
