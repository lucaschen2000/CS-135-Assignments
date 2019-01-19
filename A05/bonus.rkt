;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 05, Question 5
;; *****************************************
;;

(define (hide-msg message code n)
  (cond [(empty? (string->list code)) empty]
        [(char=? (first (string->list message)) (first (string->list code)))
         (cons n (hide-msg (list->string (rest (string->list message)))
                           (list->string (rest (string->list code))) 0))]
        [else (hide-msg (list->string (rest (string->list message)))
                        code (add1 n))]))


(define (encode-msg message code)
  (hide-msg message code 0))