;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname score) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 02, Question 1
;; *****************************************
;;

;; (tetris-score level lines-eliminated) outputs a score
;;   based on the current level and the number of lines
;;   eliminated in a game of Tetris
;; tetris-score: Nat Nat -> Nat
;; requires: lines-eliminated < 5
;; Examples:
(check-expect (tetris-score 10 4) 13200)
(check-expect (tetris-score 5 5) 0)

;; Scoring for lines
(define score-one-line 40)
(define score-two-lines 100)
(define score-three-lines 300)
(define score-four-lines 1200)

(define (tetris-score level lines-eliminated)
  (cond [(= lines-eliminated 1) (* score-one-line (+ level 1))]
        [(= lines-eliminated 2) (* score-two-lines (+ level 1))]
        [(= lines-eliminated 3) (* score-three-lines (+ level 1))]
        [(= lines-eliminated 4) (* score-four-lines (+ level 1))]
        [else 0]))

;; Tests:
(check-expect (tetris-score 0 0) 0)
(check-expect (tetris-score 5 1) 240)
(check-expect (tetris-score 100 2) 10100)
(check-expect (tetris-score 0 3) 300)
