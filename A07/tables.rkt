;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tables) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 07, Question 4
;; *****************************************
;;

;; A Table is a (listof (listof Any))
;; requires: Every sublist of the table has the same length

(define mixed-table
  (list (list 3 (make-posn 4 2) "hello" false 4 -3)
        (list 3.2 #\q #\r "baby" -4.2 -6)
        (list '(1 2 3) 4 true false "quack" -9)
        (list "what" "is" "this" "even?" "Oy!" "minus 12")))

;; (transpose table) consumes table and produces a table with
;;   its columns and rows switched
;; transpose: Table -> Table
;; Examples:
(check-expect (transpose mixed-table)
              (list (list 3 3.2 '(1 2 3) "what")
                    (list (make-posn 4 2) #\q 4 "is")
                    (list "hello" #\r true "this")
                    (list false "baby" false "even?")
                    (list 4 -4.2 "quack" "Oy!")
                    (list -3 -6 -9 "minus 12")))

(define (transpose table)
  (local [;; (item-at-pos pos row) outputs the item at the (pos)th position 
          ;;    in row indexed from 0
          ;; item-at-pos: Nat (listof Any) -> Any
          (define (item-at-pos pos row)
            (cond [(= pos 0) (first row)]
                  [else (item-at-pos (sub1 pos) (rest row))]))]
    (local [;; (column col table) produces the (col)th column of
            ;;   table indexed from 0
            ;; column: Nat Table -> (listof Any)
            (define (column col table)
              (cond [(empty? table) empty]
                    [else (cons (item-at-pos col (first table))
                                (column col (rest table)))]))]
      (local [;; (transpose table) consumes table and produces a table with
              ;;   its columns and rows switched
              ;; transpose: Table -> Table
              (define (transpose/acc table n)
                (cond [(empty? table) empty]
                      [(>= n (length (first table))) empty]
                      [else (cons (column n table)
                                  (transpose/acc table (add1 n)))]))]
        (transpose/acc table 0)))))

;; Tests:
(check-expect (transpose empty) empty)
(check-expect (transpose (list empty)) empty)
(check-expect (transpose (list empty empty empty)) empty)
(check-expect (transpose (list (list 0))) (list (list 0)))
(check-expect (transpose (list (list empty) (list empty) (list empty)))
              (list (list empty empty empty)))
(check-expect (transpose (list (list empty empty empty)))
              (list (list empty) (list empty) (list empty)))
(check-expect (transpose (list '(1 2 3) '(A B C) '("Look" "at" "me")))
                         (list '(1 A "Look") '(2 B "at") '(3 C "me")))