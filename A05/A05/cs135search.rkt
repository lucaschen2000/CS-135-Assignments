;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 05, Question 4
;; *****************************************
;;

;; A doc-list (DL) is one of:
;; * empty
;; * (cons Str DL)
;; requires: each doc (i.e. Str) only occurs once in the doc-list
;; the doc-list is in ascending alphabetical order

;; An Inverted List (IL) is one of:
;; * empty
;; * (cons (list Str DL) IL)
;; requires: each key (i.e. Str) only occurs once in the IL.
;; the keys occur in ascending alphabetical order in the IL.

;; ==== Question 4a ========================

;; (both first-doc-list second-doc-list) outputs a list
;;   of documents that occur in both first-doc-list and
;;   second-doc-list
;; both: DL DL -> DL
;; Examples:
(check-expect (both (list "a.txt" "b.txt" "c.txt") (list "b.txt" "c.txt"))
              (list "b.txt" "c.txt"))
(check-expect (both empty (list "a.txt")) empty)

(define (both first-doc-list second-doc-list)
  (cond [(or (empty? first-doc-list) (empty? second-doc-list)) empty]
        [(string=? (first first-doc-list) (first second-doc-list))
         (cons (first first-doc-list)
               (both (rest first-doc-list) (rest second-doc-list)))]
        [(string<? (first first-doc-list) (first second-doc-list))
         (both (rest first-doc-list) second-doc-list)]
        [(string>? (first first-doc-list) (first second-doc-list))
         (both first-doc-list (rest second-doc-list))]))

;; Tests:
(check-expect (both empty empty) empty)
(check-expect (both (list "a.txt") empty) empty)
(check-expect (both (list "a.txt") (list "b.txt")) empty)
(check-expect (both (list "a.txt") (list "a.txt")) (list "a.txt"))
(check-expect (both (list "b.txt") (list "b.txt" "c.txt")) (list "b.txt"))
(check-expect (both (list "a.txt" "c.txt") (list "b.txt" "c.txt" "d.txt"))
              (list "c.txt"))


;; ==== Question 4b ========================

;; (exclude first-doc-list second-doc-list) outputs a list
;;   of documents that occur in both first-doc-list and
;;   second-doc-list
;; both: DL DL -> DL
;; Examples:
(check-expect (exclude (list "a.txt" "b.txt" "c.txt") (list "b.txt" "c.txt"))
              (list "a.txt"))
(check-expect (exclude empty (list "a.txt")) empty)

(define (exclude first-doc-list second-doc-list)
  (cond [(empty? first-doc-list) empty]
        [(empty? second-doc-list) first-doc-list]
        [(string=? (first first-doc-list) (first second-doc-list))
         (exclude (rest first-doc-list) (rest second-doc-list))]
        [(string<? (first first-doc-list) (first second-doc-list))
         (cons (first first-doc-list)
               (exclude (rest first-doc-list) second-doc-list))]
        [(string>? (first first-doc-list) (first second-doc-list))
         (exclude first-doc-list (rest second-doc-list))]))

(check-expect (exclude empty empty) empty)
(check-expect (exclude (list "a.txt") empty) (list "a.txt"))
(check-expect (exclude (list "a.txt") (list "b.txt")) (list "a.txt"))
(check-expect (exclude (list "a.txt") (list "a.txt")) empty)
(check-expect (exclude (list "a.txt" "b.txt") (list "b.txt" "c.txt"))
              (list "a.txt"))
(check-expect (exclude (list "b.txt" "c.txt") (list "a.txt" "b.txt"))
              (list "c.txt"))
(check-expect (exclude (list "a.txt" "c.txt") (list "a.txt" "c.txt")) empty)
(check-expect (exclude (list "b.txt") (list "b.txt" "c.txt")) empty)
(check-expect (exclude (list "a.txt" "c.txt") (list "b.txt" "c.txt" "d.txt"))
              (list "a.txt"))


;; ==== Question 4c ========================

;; (search-str str an-IL) outputs the doc-list
;;   associated with a str in an-IL
;; search-str: Str IL -> DL
;; Examples:
(check-expect (search-str "cat"
                          (list (list "barks" (list "b.txt"))
                                (list "cat" (list "a.txt" "c.txt"))
                                (list "chases" (list "c.txt"))
                                (list "dog" (list "b.txt" "c.txt"))
                                (list "sleeps" (list "a.txt"))
                                (list "suddenly" (list "c.txt"))
                                (list "the" (list "a.txt" "b.txt" "c.txt"))))
                          (list "a.txt" "c.txt"))

(define (search-str str an-IL)
  (cond [(empty? an-IL) empty]
        [(string=? str (first (first an-IL))) (second (first an-IL))]
        [else (search-str str (rest an-IL))]))

;; (search filter str1 str2 an-IL) will output a doc-list that contains 
;;   both str1 and str2 if filter is 'both and will output a doc-list 
;;   that contains str1 but not str2 if filter is 'exclude
;; search: Sym Str Str IL -> DL
;; requires: filter: one of ('both 'exclude)
;; Examples:     

(define (search filter str1 str2 an-IL)
  (cond [(symbol=? filter 'both)
         (both (search-str str1 an-IL) (search-str str2 an-IL))]
        [else (exclude (search-str str1 an-IL) (search-str str2 an-IL))]))

(check-expect (search 'both "" "" empty) empty)
(check-expect (search 'exclude "" "" empty) empty)
(check-expect (search 'both "solo" ""
                      (list (list "solo" (list "a.txt" "b.txt"))))
              empty)
(check-expect (search 'exclude "solo" ""
                      (list (list "solo" (list "a.txt" "b.txt"))))
              (list "a.txt" "b.txt"))
(check-expect (search 'both "" "hi"
                      (list (list "" (list "a.txt" "b.txt"))
                            (list "hi" (list "b.txt"))))
              (list "b.txt"))
(check-expect (search 'exclude "" "hi"
                      (list (list "" (list "a.txt" "b.txt" "c.txt"))
                            (list "hi" (list "b.txt"))))
              (list "a.txt" "c.txt"))
(check-expect (search 'both "" ""
        (list (list "barks" (list "b.txt"))
              (list "cat" (list "a.txt" "c.txt"))
              (list "chases" (list "c.txt"))
              (list "dog" (list "b.txt" "c.txt"))
              (list "sleeps" (list "a.txt"))
              (list "suddenly" (list "c.txt"))
              (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)
(check-expect (search 'both "suddenly" "barks"
        (list (list "barks" (list "b.txt"))
              (list "cat" (list "a.txt" "c.txt"))
              (list "chases" (list "c.txt"))
              (list "dog" (list "b.txt" "c.txt"))
              (list "sleeps" (list "a.txt"))
              (list "suddenly" (list "c.txt"))
              (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)
(check-expect (search 'both "cat" "cat"
        (list (list "barks" (list "b.txt"))
              (list "cat" (list "a.txt" "c.txt"))
              (list "chases" (list "c.txt"))
              (list "dog" (list "b.txt" "c.txt"))
              (list "sleeps" (list "a.txt"))
              (list "suddenly" (list "c.txt"))
              (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt" "c.txt"))
(check-expect (search 'exclude "cat" "dog"
        (list (list "barks" (list "b.txt"))
              (list "cat" (list "a.txt" "c.txt"))
              (list "chases" (list "c.txt"))
              (list "dog" (list "b.txt" "c.txt"))
              (list "sleeps" (list "a.txt"))
              (list "suddenly" (list "c.txt"))
              (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))
(check-expect (search 'exclude "sleeps" ""
        (list (list "barks" (list "b.txt"))
              (list "cat" (list "a.txt" "c.txt"))
              (list "chases" (list "c.txt"))
              (list "dog" (list "b.txt" "c.txt"))
              (list "sleeps" (list "a.txt"))
              (list "suddenly" (list "c.txt"))
              (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))
(check-expect (search 'exclude "the" "the"
        (list (list "barks" (list "b.txt"))
              (list "cat" (list "a.txt" "c.txt"))
              (list "chases" (list "c.txt"))
              (list "dog" (list "b.txt" "c.txt"))
              (list "sleeps" (list "a.txt"))
              (list "suddenly" (list "c.txt"))
              (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)
(check-expect (search 'exclude "dog" "the"
        (list (list "barks" (list "b.txt"))
              (list "cat" (list "a.txt" "c.txt"))
              (list "chases" (list "c.txt"))
              (list "dog" (list "b.txt" "c.txt"))
              (list "sleeps" (list "a.txt"))
              (list "suddenly" (list "c.txt"))
              (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)


