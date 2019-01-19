;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135coded) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 05, Question 2
;; *****************************************
;;

;; ==== Question 2a ========================

;; (get-char string position) outputs the nth character in
;;   string (denoted by position) with the first character
;;   starting at 0
;; get-char: Str Nat -> Char
;; Examples:
(check-expect (get-char "abcdefg" 0) #\a)

(define out-of-range #\*)
(define start-next-character-search 0)

(define (get-char string position)
  (cond [(empty? (string->list string)) out-of-range]
        [(= position start-next-character-search)
         (first (string->list string))]
        [else (get-char (list->string (rest (string->list string)))
                        (sub1 position))]))

;; Tests:
(check-expect (get-char "" 0) #\*)
(check-expect (get-char "" 4) #\*)
(check-expect (get-char "a" 0) #\a)
(check-expect (get-char "abcdefg" 3) #\d)
(check-expect (get-char "abcdefg" 20) #\*)
(check-expect (get-char "12345" 3) #\4)
(check-expect (get-char "       " 4) #\ )


;; ==== Question 2b ========================

;; A Decryptor is a (list Nat Nat Nat)

;; (decrypt hidden-message decoder) decrypts hidden-message using
;;   decoder to produce a list containing each character in the
;;   3-character secret message
;; decrypt: Str Decrpytor -> (listof Char)
;; Examples:
(check-expect (decrypt "abcdefg" (list 0 0 0)) (list #\a #\b #\c))
(check-expect (decrypt "abcdefg" (list 2 1 0)) (list #\c #\e #\f))
(check-expect (decrypt "abcdefg" (list 2 10 0)) (list #\c))

(define (decrypt hidden-message decoder)
  (cond [(empty? (string->list hidden-message)) empty]
        [(empty? decoder) empty]
        [(= (first decoder) start-next-character-search)
         (cons (first (string->list hidden-message))
               (decrypt (list->string (rest (string->list hidden-message)))
                        (rest decoder)))]
        [else (decrypt (list->string (rest (string->list hidden-message)))
                       (cons (sub1 (first decoder)) (rest decoder)))]))

;; (coded-3-char hidden-message decoder) decrypts hidden-message
;;   using decoder to produce a 3-character string. If a
;;   character is out of range, * is used in its place
;; coded-3-char: Str Decryptor -> Str
;; Examples:
(check-expect (coded-3-char "abcdefg" (list 1 1 1)) "bdf")

(define one-character 1)
(define two-character 2)
(define all-out-of-range "***")

(define (coded-3-char hidden-message decoder)
  (cond [(equal? (decrypt hidden-message decoder) empty) all-out-of-range]
        [(= (length (decrypt hidden-message decoder)) one-character)
         (list->string (cons (first (decrypt hidden-message decoder))
                             (cons out-of-range (cons out-of-range empty))))]
        [(= (length (decrypt hidden-message decoder)) two-character)
         (list->string (cons (first (decrypt hidden-message decoder))
                             (cons (second (decrypt hidden-message decoder))
                                   (cons out-of-range empty))))]
        [else (list->string (decrypt hidden-message decoder))]))

(check-expect (coded-3-char "" (list 0 0 0)) "***")
(check-expect (coded-3-char "" (list 1 2 3)) "***")
(check-expect (coded-3-char "abcdefg" (list 0 0 0)) "abc")
(check-expect (coded-3-char "abcdefg" (list 2 0 2)) "cdg")
(check-expect (coded-3-char "abc efg" (list 2 0 2)) "c g")
(check-expect (coded-3-char "abcdefg" (list 10 0 0)) "***")
(check-expect (coded-3-char "abcdefg" (list 2 10 0)) "c**")
(check-expect (coded-3-char "abcdefg" (list 3 0 17)) "de*")
(check-expect (coded-3-char "1234" (list 1 1 2)) "24*")
(check-expect (coded-3-char "       " (list 0 1 1)) "   ")
(check-expect (coded-3-char "ab" (list 0 0 0)) "ab*")
(check-expect (coded-3-char "a" (list 1 0 0)) "***")

        
;; ==== Question 2c ========================

;; (enc-possible? message code) determines if code
;;   can be hidden in message using a Decryptor
;; enc-possible?: Str Str -> Bool
;; requires: code to be a string of length 3
;; Examples:
(check-expect (enc-possible? "abcdefg" "bdg") true)

(define (enc-possible? message code)
  (cond [(empty? (string->list code)) true]
        [(empty? (string->list message)) false]
        [(equal? (first (string->list message))
                   (first (string->list code)))
         (enc-possible? (list->string (rest (string->list message)))
                        (list->string (rest (string->list code))))]
        [else (enc-possible?
               (list->string (rest (string->list message))) code)]))

(check-expect (enc-possible? "" "aaa") false)
(check-expect (enc-possible? "a" "bac") false)
(check-expect (enc-possible? "abcdefg" "bdg") true)
(check-expect (enc-possible? "abbababa" "aaa") true)
(check-expect (enc-possible? "abcdefg" "abz") false)
(check-expect (enc-possible? "3141569" "369") true)
(check-expect (enc-possible? "3141569" "123") false)
(check-expect (enc-possible? "     " "   ") true)