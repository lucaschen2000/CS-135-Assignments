;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname paint-by-numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 06, Question 1
;; *****************************************
;;

;; A Grid is a (listof (listof (anyof 'B '-)))
;; requires: all sublists have the same length

(define fish-grid
  '((B B - - B B B B - - - B)
    (- B B B - - - - B - - -)
    (- B B B - - - - B B - -)
    (- B - - B B B B B - - -)
    (B B - - - - B - - - - -)))

;; ==== Question 1a ========================

;; (state-at-pos pos row) outputs the state of the (pos)th position 
;;    in row indexed from 0, if out of range, produces empty
;; state-at-pos: Nat (listof (anyof 'B '-)) -> (anyof 'B '- empty)
;; Examples:
(check-expect (state-at-pos 0 empty) empty)
(check-expect (state-at-pos 4 (first fish-grid)) 'B)

(define (state-at-pos pos row)
  (cond [(empty? row) empty]
        [(= pos 0) (first row)]
        [else (state-at-pos (sub1 pos) (rest row))]))

;; (column col grid) produces the (col)th column of
;;   grid indexed from 0, if out of range, produces empty
;; column: Nat Grid -> (listof (anyof 'B '-))
;; Examples:
(check-expect (column 0 fish-grid) '(B - - - B))

(define (column col grid)
  (cond [(empty? grid) empty]
        [(> col (length (first grid))) empty]
        [else (cons (state-at-pos col (first grid))
                    (column col (rest grid)))]))

;; Tests:
(check-expect (column 0 empty) empty)
(check-expect (column 5 empty) empty)
(check-expect (column 0 (list '(B))) '(B))
(check-expect (column 2 (list '(B B - - B - B -))) '(-))
(check-expect (column 2 fish-grid) '(- B B - -))
(check-expect (column 11 fish-grid) '(B - - - -))
(check-expect (column 32 fish-grid) empty)


;; ==== Question 1b ========================

;; (cells->tallies/acc line n) produces a list of tallies, each representing
;;   the number of adjacent black cells there are in line, using n as an
;;   an accumulator to count the number of adjacent black cells
;; cells->tallies/acc: (listof (anyof 'B '-)) Nat -> (listof Nat)
;; Examples and tests: see wrapper function cells->tallies

(define (cells->tallies/acc line n)
  (cond [(and (empty? line) (= n 0)) empty]
        [(empty? line) (list n)]
        [(symbol=? (first line) 'B)
         (cells->tallies/acc (rest line) (add1 n))]
        [(and (symbol=? (first line) '-) (= n 0))
         (cells->tallies/acc (rest line) n)]
        [else (cons n (cells->tallies/acc line 0))]))

;; (cells->tallies/acc line) produces a list of tallies, each representing
;;   the number of adjacent black cells there are in line
;; cells->tallies/acc: (listof (anyof 'B '-)) -> (listof Nat)
;; Examples:
(check-expect (cells->tallies '(-)) empty)
(check-expect (cells->tallies '(B B - - B B B B - - - B)) '(2 4 1))

(define (cells->tallies row)
  (cells->tallies/acc row 0))

;; Tests:
(check-expect (cells->tallies empty) empty)
(check-expect (cells->tallies '(B)) '(1))
(check-expect (cells->tallies '(B B B B B B)) '(6))
(check-expect (cells->tallies '(- - - - - -)) empty)
(check-expect (cells->tallies '(B B B B B B B B - - - -)) '(8))
(check-expect (cells->tallies '(- - - - - - - - B B B B)) '(4))
(check-expect (cells->tallies '(- - B B - - - - B B B -)) '(2 3))
(check-expect (cells->tallies '(- B - B - B)) '(1 1 1))

;; ==== Question 1c ========================

;; (puzzle-labels-horizontal grid) produces a list of list of tallies, 
;;   each list of tallies represents the number of adjacent black cells 
;;   there are in each row from the top to the bottom of grid
;; puzzle-labels-horizontal: Grid -> (listof (listof Nat))
;; Examples:
(check-expect (puzzle-labels-horizontal fish-grid)
              (list '(2 4 1) '(3 1) '(3 2) '(1 5) '(2 1)))

(define (puzzle-labels-horizontal grid)
  (cond [(empty? grid) empty]
        [else (cons (cells->tallies (first grid))
                    (puzzle-labels-horizontal (rest grid)))]))

;; (puzzle-labels-vertical/acc grid n) produces a list of list of tallies, each
;;   list of tallies represents the number of adjacent black cells there are in
;;   each column from the left to the right side of grid, (n indexes columns)
;; puzzle-labels-vertical/acc: Grid Nat -> (listof (listof Nat))
;; Examples:
(check-expect
 (puzzle-labels-vertical/acc fish-grid 0)
 (list '(1 1) '(5) '(2) '(2) '(1 1) '(1 1) '(1 2) '(1 1) '(3) '(1) empty '(1)))

(define (puzzle-labels-vertical/acc grid n)
  (cond [(empty? grid) empty]
        [(= (length (first grid)) n) empty]
        [else (cons (cells->tallies (column n grid))
                    (puzzle-labels-vertical/acc grid (add1 n)))]))

;; (puzzle-labels grid) produces a list of length two, the first element being
;;   a list of labels from the top to the bottom of grid and the second being
;;   a list of labels from the left to the right of grid
;; puzzle-labels: Grid -> (list (listof (listof Nat)) (listof (listof Nat)))
;; Examples:
(check-expect
 (puzzle-labels fish-grid)
 (list
  (list '(2 4 1) '(3 1) '(3 2) '(1 5) '(2 1))
  (list '(1 1) '(5) '(2) '(2) '(1 1) '(1 1) '(1 2) '(1 1) '(3) '(1) '() '(1))))

(define (puzzle-labels grid)
  (list (puzzle-labels-horizontal grid)
        (puzzle-labels-vertical/acc grid 0)))

;; Tests:
(check-expect (puzzle-labels empty) (list empty empty))
(check-expect
 (puzzle-labels (list '(-)))
 (list '(())
       '(())))
(check-expect
 (puzzle-labels (list '(B)))
 (list '((1))
       '((1))))
(check-expect
 (puzzle-labels (list '(- B - B)))
 (list '((1 1))
       '(() (1) () (1))))
(check-expect
 (puzzle-labels (list '(B)
                      '(-)
                      '(-)
                      '(B)))
 (list '((1) () () (1))
       '((1 1))))
(check-expect
 (puzzle-labels (list '(- B B -)
                      '(- B - B)
                      '(- B B B)
                      '(B - - -)))
 (list '((2) (1 1) (3) (1))
       '((1) (3) (1 1) (2))))
(check-expect
 (puzzle-labels (list '(- - - -)
                      '(- B B -)
                      '(- B B -)
                      '(- - - -)))
 (list '(() (2) (2) ())
       '(() (2) (2) ())))


 



