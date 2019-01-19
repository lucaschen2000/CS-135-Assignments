;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(check-expect (cells->tallies '(-)) empty)
;(check-expect (cells->tallies '(B B - - B B B B - - - B)) '(2 4 1))

(define (group-b list)
  (cond [(empty? list) empty]
        [(symbol=? (first list) 'B)
         (cons (first list) (group-b (rest list)))]
        [else empty]))

(define (start-at list n)
  (cond [(= n 0) list]
        [else (start-at (rest list) (sub1 n))]))

(define (cells->tallies list)
  (cond [(empty? list) empty]
        [(symbol=? (first list) 'B)
         (cons (length (group-b list))
               (cells->tallies (start-at list (length (group-b list)))))]
        [else (cells->tallies (rest list))]))


(cells->tallies '(B B - - B B B B - - - B))
         