;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 09, Question 1
;; *****************************************
;;

;; A Node is a Sym
;; A Graph is a (listof (list Node (listof Node)))

(define my-graph
  '((A (B C D))
    (B (E))
    (C (F))
    (D (F))
    (F ())))

;; A Route is a (listof Sym)

;; (neighbours n graph) produces list of neighbours of n in graph
;; neighbours: Node Graph -> (listof Node)
;; requires: n is a node in graph
;; Examples:
(check-expect (neighbours 'A my-graph) '(B C D))

(define (neighbours n graph)
  (cond [(symbol=? n (first (first graph))) (second (first graph))]
        [else (neighbours n (rest graph))]))

;; (valid-route?/list listof-nodes route graph) produces true if route is a
;;   valid path to traverse graph and false otherwise, using the nodes in
;;   listof-nodes to check if any are valid starting paths for route
;; valid-route?: Route Graph -> Bool
;; Examples:
(check-expect (valid-route?/list '(B C D) '(D F) my-graph) true)

(define (valid-route?/list listof-nodes route graph)
  (cond [(empty? listof-nodes) false]
        [(symbol=? (first listof-nodes) (first route))
         (valid-route? route graph)]
        [else (valid-route?/list (rest listof-nodes) route graph)]))

;; (valid-route? route graph) produces true if route is a valid
;;   path to traverse graph and false otherwise
;; valid-route?: Route Graph -> Bool
;; Examples:
(check-expect (valid-route? '(A D F) my-graph) true)

(define (valid-route? route graph)
  (cond [(empty? route) true]
        [(not (member? (first route) (map first graph))) false]
        [(empty? (rest route)) true]
        [else (valid-route?/list
               (neighbours (first route) graph) (rest route) graph)]))

;; Tests:
(check-expect (valid-route? '(A) '((A ()))) true)
(check-expect (valid-route? '(A) '((B))) false)
(check-expect (valid-route? '(A) my-graph) true)
(check-expect (valid-route? '(A A) my-graph) false)
(check-expect (valid-route? '(A B C) my-graph) false)
(check-expect (valid-route? '(A B C) empty) false)
(check-expect (valid-route? empty my-graph) true)
(check-expect (valid-route? empty empty) true)
(check-expect (valid-route? '(X Y Z) my-graph) false)
(define cyclic-graph
  '((A (B)) (B (C)) (C (A))))
(check-expect (valid-route? '(A B C) cyclic-graph) true)
(check-expect (valid-route? '(A B C A B C) cyclic-graph) true)
(check-expect (valid-route? '(C B A) cyclic-graph) false)
(define new-graph '((A (B))
                    (B (C D E))
                    (C ())
                    (D (E F))
                    (E ())
                    (F (C))))
(check-expect (valid-route? empty new-graph) true)
(check-expect (valid-route? '(A B D F C) new-graph) true)
(check-expect (valid-route? '(A B E) new-graph) true)
(check-expect (valid-route? '(D E) new-graph) true)
(check-expect (valid-route? '(A B C D) new-graph) false)
(check-expect (valid-route? '(C F D B A) new-graph) false)
