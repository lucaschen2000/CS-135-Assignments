;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 03, Question 3
;; *****************************************
;;

(define-struct square (row column))
;; A Square is a (make-square Nat Sym)
;; requires: 1 <= row <= 8
;;           column: (anyof 'a 'b 'c 'd 'e 'f 'g 'h)


;; ==== Question 3a ========================

(define-struct piece (pos unit))
;; A Piece is a (make-piece (make-square Nat Sym) Sym)
;; requires: unit: (anyof ’knight ’rook ’bishop ’queen)


;; ==== Question 3b ========================

;; my-piece-fn: Piece -> Any
(define (my-piece-fn chess-piece)
  (... (piece-pos chess-piece) ...
       (piece-unit chess-piece) ...))


;; ==== Question 3c ========================

;; (letter->number letter) outputs the position of a
;;   letter in the alphabet, with 'a starting at 1
;; letter->number: Sym -> Nat
;; requires: letter: (anyof 'a 'b 'c 'd 'e 'f 'g 'h)
;; Examples:
(check-expect (letter->number 'c) 3)

(define first-pos 1)
(define second-pos 2)
(define third-pos 3)
(define fourth-pos 4)
(define fifth-pos 5)
(define sixth-pos 6)
(define seventh-pos 7)
(define eight-pos 8)

(define (letter->number letter)
  (cond [(symbol=? letter 'a) first-pos]
        [(symbol=? letter 'b) second-pos]
        [(symbol=? letter 'c) third-pos]
        [(symbol=? letter 'd) fourth-pos]
        [(symbol=? letter 'e) fifth-pos]
        [(symbol=? letter 'f) sixth-pos]
        [(symbol=? letter 'g) seventh-pos]
        [else eight-pos]))

;; (rook-valid? chess-piece target) determines if a rook's movement is
;;   valid using the position of chess-piece as the starting position
;;   and the position of target as the target position
;; rook-valid?: Piece Square -> Bool
;; requires: unit = 'rook
;; Examples:
(check-expect (rook-valid? (make-piece (make-square 1 'c) 'rook)
                           (make-square 1 'd)) true)
(check-expect (rook-valid? (make-piece (make-square 1 'a) 'rook)
                           (make-square 2 'b)) false)

(define (rook-valid? chess-piece target)
  (or (= (square-row target) (square-row (piece-pos chess-piece)))
      (symbol=? (square-column target)
                (square-column (piece-pos chess-piece)))))

;; (bishop-valid? chess-piece target) determines if a bishop's movement
;;   is valid using the position of chess-piece as the starting position
;;   and the position of target as the target position
;; rook-valid?: Piece Square -> Bool
;; requires: unit = 'bishop
;; Examples:
(check-expect (bishop-valid? (make-piece (make-square 1 'a) 'bishop)
                             (make-square 2 'a)) false)
(check-expect (bishop-valid? (make-piece (make-square 1 'a) 'bishop)
                             (make-square 2 'b)) true)

(define (bishop-valid? chess-piece target)
  (= (abs (- (square-row target) (square-row (piece-pos chess-piece))))
     (abs (- (letter->number (square-column target))
             (letter->number (square-column (piece-pos chess-piece)))))))

;; (queen-valid? chess-piece target) determines if a queen's movement
;;   is valid using the position of chess-piece as the starting position
;;   and the position of target as the target position
;; rook-valid?: Piece Square -> Bool
;; requires: unit = 'queen
;; Examples:
(check-expect (queen-valid? (make-piece (make-square 1 'a) 'queen)
                            (make-square 2 'a)) true)
(check-expect (queen-valid? (make-piece (make-square 1 'a) 'queen)
                            (make-square 2 'b)) true)
(check-expect (queen-valid? (make-piece (make-square 1 'a) 'queen)
                            (make-square 2 'c)) false)

(define (queen-valid? chess-piece target)
  (or (rook-valid? chess-piece target) (bishop-valid? chess-piece target)))

;; (knight-valid? chess-piece target) determines if a knight's movement
;;   is valid using the position of chess-piece as the starting position
;;   and the position of target as the target position
;; knight-valid?: Piece Square -> Bool
;; requires: unit = 'knight
;; Examples:
(check-expect (knight-valid? (make-piece (make-square 1 'a) 'knight)
                             (make-square 3 'b)) true)
(check-expect (knight-valid? (make-piece (make-square 1 'a) 'knight)
                             (make-square 2 'b)) false)

(define valid-knight-square-distances-sum 5)

(define (knight-valid? chess-piece target)
  (or (= valid-knight-square-distances-sum
         (+ (sqr (- (square-row target) (square-row (piece-pos chess-piece))))
            (sqr (- (letter->number (square-column target))
                    (letter->number (square-column
                                     (piece-pos chess-piece)))))))
      (and (= (square-row target) (square-row (piece-pos chess-piece)))
           (symbol=? (square-column target)
                     (square-column (piece-pos chess-piece))))))

;; (valid-move? chess-piece target) determines if a position
;;   on the chessboard (denoted by target) is a valid move
;;   for a chess piece's current position (denoted by chess-piece)
;; valid-move?: Piece Square -> Bool
;; requires: unit: (anyof 'knight 'rook 'bishop 'queen)
;; Examples:
(check-expect (valid-move? (make-piece (make-square 1 'a) 'knight)
                           (make-square 2 'c)) true)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'rook)
                           (make-square 1 'b)) true)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'bishop)
                           (make-square 4 'd)) true)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'queen)
                           (make-square 8 'h)) true)

(define (valid-move? chess-piece target)
  (cond [(symbol=? 'knight (piece-unit chess-piece))
         (knight-valid? chess-piece target)]
        [(symbol=? 'rook (piece-unit chess-piece))
         (rook-valid? chess-piece target)]
        [(symbol=? 'bishop (piece-unit chess-piece))
         (bishop-valid? chess-piece target)]
        [else (queen-valid? chess-piece target)]))

;; Tests:
(check-expect (valid-move? (make-piece (make-square 7 'e) 'knight)
                           (make-square 7 'e)) true)
(check-expect (valid-move? (make-piece (make-square 4 'c) 'knight)
                           (make-square 6 'd)) true)
(check-expect (valid-move? (make-piece (make-square 1 'h) 'knight)
                           (make-square 1 'g)) false)
(check-expect (valid-move? (make-piece (make-square 3 'e) 'knight)
                           (make-square 6 'b)) false)
(check-expect (valid-move? (make-piece (make-square 8 'f) 'rook)
                           (make-square 8 'f)) true)
(check-expect (valid-move? (make-piece (make-square 6 'b) 'rook)
                           (make-square 2 'b)) true)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'rook)
                           (make-square 1 'd)) true)
(check-expect (valid-move? (make-piece (make-square 3 'c) 'rook)
                           (make-square 5 'e)) false)
(check-expect (valid-move? (make-piece (make-square 4 'f) 'bishop)
                           (make-square 4 'f)) true)
(check-expect (valid-move? (make-piece (make-square 7 'c) 'bishop)
                           (make-square 5 'e)) true)
(check-expect (valid-move? (make-piece (make-square 2 'e) 'bishop)
                           (make-square 5 'h)) true)
(check-expect (valid-move? (make-piece (make-square 8 'h) 'bishop)
                           (make-square 5 'g)) false)
(check-expect (valid-move? (make-piece (make-square 6 'g) 'queen)
                           (make-square 6 'g)) true)
(check-expect (valid-move? (make-piece (make-square 7 'c) 'queen)
                           (make-square 4 'c)) true)
(check-expect (valid-move? (make-piece (make-square 8 'e) 'queen)
                           (make-square 5 'h)) true)
(check-expect (valid-move? (make-piece (make-square 6 'c) 'queen)
                           (make-square 3 'e)) false)


;; ==== Question 3d ========================


;; (distance-check target) outputs the Manhattan
;;   distance between target and the position H1
;;   on a chessboard (number of squares away in both axes)
;; distance-check: Square -> Nat
;; Examples:
(check-expect (distance-check (make-square 5 'c)) 9)

(define H1-row 1)
(define H1-column-as-number 8)

(define (distance-check target)
  (+ (abs (- H1-row (square-row target)))
     (abs (- H1-column-as-number (letter->number (square-column target))))))

;; (number->letter number) outputs a letter corresponding with 
;;   its position (denoted by number) in the alphabet with
;;   'a starting at 1
;; letter->number: Nat -> Sym
;; requires: 1 <= number <= 8
;; Examples:
(check-expect (number->letter 1) 'a)

(define (number->letter number)
  (cond [(= number first-pos) 'a]
        [(= number second-pos) 'b]
        [(= number third-pos) 'c]
        [(= number fourth-pos) 'd]
        [(= number fifth-pos) 'e]
        [(= number sixth-pos) 'f]
        [(= number seventh-pos) 'g]
        [else 'h]))

;; (down-2-right-1 chess-piece) takes the position
;;   of chess-piece and translates it down 2 units
;;   and right 1 unit
;; down-2-right-1: Piece -> Square
;; requires: unit = 'knight
;; Examples:
(check-expect (down-2-right-1 (make-piece (make-square 4 'e) 'knight))
              (make-square 2 'f))

(define down-2 -2)
(define right-1 1)

(define (down-2-right-1 chess-piece)
  (make-square (+ (square-row (piece-pos chess-piece)) down-2)
               (number->letter
                (+ (letter->number
                    (square-column (piece-pos chess-piece))) right-1))))

;; (down-1-right-2 chess-piece) takes the position
;;   of chess-piece and translates it down 1 unit
;;   and right 2 units
;; down-2-right-1: Piece -> Square
;; requires: unit = 'knight
;; Examples:
(check-expect (down-1-right-2 (make-piece (make-square 4 'e) 'knight))
              (make-square 3 'g))

(define down-1 -1)
(define right-2 2)

(define (down-1-right-2 chess-piece)
  (make-square (+ (square-row (piece-pos chess-piece)) down-1)
               (number->letter
                (+ (letter->number
                    (square-column (piece-pos chess-piece))) right-2))))

;; (up-1-right-2 chess-piece) takes the position
;;   of chess-piece and translates it up 1 unit
;;   and right 2 units
;; down-2-right-1: Piece -> Square
;; requires: unit = 'knight
;; Examples:
(check-expect (up-1-right-2 (make-piece (make-square 1 'e) 'knight))
              (make-square 2 'g))

(define up-1 1)

(define (up-1-right-2 chess-piece)
  (make-square (+ (square-row (piece-pos chess-piece)) up-1)
               (number->letter
                (+ (letter->number
                    (square-column (piece-pos chess-piece))) right-2))))

;; (down-2-left-1 chess-piece) takes the position
;;   of chess-piece and translates it down 2 units
;;   and left 1 unit
;; down-2-right-1: Piece -> Square
;; requires: unit = 'knight
;; Examples:
(check-expect (down-2-left-1 (make-piece (make-square 4 'h) 'knight))
              (make-square 2 'g))

(define left-1 -1)

(define (down-2-left-1 chess-piece)
  (make-square (+ (square-row (piece-pos chess-piece)) down-2)
               (number->letter
                (+ (letter->number
                    (square-column (piece-pos chess-piece))) left-1))))

;; (optimal-knight-move chess-piece) takes the position
;;   of chess-piece and translates it depending on its
;;   distance away from H1 on a chessboard
;; down-2-right-1: Piece -> Square
;; requires: unit = 'knight
;; Examples:
(check-expect (optimal-knight-move (make-piece (make-square 4 'e) 'knight))
              (make-square 2 'f))
(check-expect (optimal-knight-move (make-piece (make-square 7 'g) 'knight))
              (make-square 5 'h))
(check-expect (optimal-knight-move (make-piece (make-square 2 'd) 'knight))
              (make-square 1 'f))

(define (optimal-knight-move chess-piece)
  (cond
    [(<= (distance-check (down-2-right-1 chess-piece))
         (distance-check (down-1-right-2 chess-piece)))
     (down-2-right-1 chess-piece)]
    [else (down-1-right-2 chess-piece)]))

;; (knight-next-move chess-piece) takes the position of
;;   chess-piece (a knight) and makes one move to get as close as possible
;;   to H1 on a chessboard, if it cannot get closer with
;;   one move, the output will be its current position
;; knight-next-move: Piece -> Square
;; Examples:
(check-expect (knight-next-move (make-piece (make-square 2 'g) 'knight))
              (make-square 2 'g))
(check-expect (knight-next-move (make-piece (make-square 8 'e) 'knight))
              (make-square 6 'f))

(define lower-bound-right-column 3)
(define upper-bound-right-column 8)
(define right-column 'h)
(define bottom-row 1)
(define lower-bound-bottom-row 1)
(define upper-bound-bottom-row 6)
(define corner-lower-bound-row 1)
(define corner-upper-bound-row 2)
(define corner-lower-bound-column 7)
(define corner-upper-bound-column 8)

(define (knight-next-move chess-piece)
  (cond [(and (<= lower-bound-right-column
                  (square-row (piece-pos chess-piece))
                  upper-bound-right-column)
              (symbol=? (square-column (piece-pos chess-piece)) right-column))
         (down-2-left-1 chess-piece)]
        [(and (= (square-row (piece-pos chess-piece)) bottom-row)
              (<= lower-bound-bottom-row
                  (letter->number (square-column (piece-pos chess-piece)))
                  upper-bound-bottom-row))
         (up-1-right-2 chess-piece)]
        [(and (<= corner-lower-bound-row
                  (square-row (piece-pos chess-piece))
                  corner-upper-bound-row)
              (<= corner-lower-bound-column
                  (letter->number (square-column (piece-pos chess-piece)))
                  corner-upper-bound-column))
         (piece-pos chess-piece)]
        [else (optimal-knight-move chess-piece)]))

;; Tests:

(check-expect (knight-next-move (make-piece (make-square 8 'h) 'knight))
              (make-square 6 'g))

(check-expect (knight-next-move (make-piece (make-square 5 'h) 'knight))
              (make-square 3 'g))

(check-expect (knight-next-move (make-piece (make-square 3 'h) 'knight))
              (make-square 1 'g))

(check-expect (knight-next-move (make-piece (make-square 1 'a) 'knight))
              (make-square 2 'c))

(check-expect (knight-next-move (make-piece (make-square 1 'c) 'knight))
              (make-square 2 'e))

(check-expect (knight-next-move (make-piece (make-square 1 'f) 'knight))
              (make-square 2 'h))

(check-expect (knight-next-move (make-piece (make-square 1 'h) 'knight))
              (make-square 1 'h))

(check-expect (knight-next-move (make-piece (make-square 1 'g) 'knight))
              (make-square 1 'g))

(check-expect (knight-next-move (make-piece (make-square 2 'h) 'knight))
              (make-square 2 'h))

(check-expect (knight-next-move (make-piece (make-square 3 'f) 'knight))
              (make-square 1 'g))

(check-expect (knight-next-move (make-piece (make-square 8 'a) 'knight))
              (make-square 6 'b))

(check-expect (knight-next-move (make-piece (make-square 4 'b) 'knight))
              (make-square 2 'c))

(check-expect (knight-next-move (make-piece (make-square 2 'd) 'knight))
              (make-square 1 'f))

(check-expect (knight-next-move (make-piece (make-square 2 'a) 'knight))
              (make-square 1 'c))

(check-expect (knight-next-move (make-piece (make-square 7 'e) 'knight))
              (make-square 5 'f))

(check-expect (knight-next-move (make-piece (make-square 3 'g) 'knight))
              (make-square 1 'h))

(check-expect (knight-next-move (make-piece (make-square 5 'e) 'knight))
              (make-square 3 'f))