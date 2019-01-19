;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binary-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 06, Question 2
;; *****************************************
;;

;; A Binary Search Tree (BST) is one of:
;; * empty
;; * a Node

(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; requires: key > every key in left BST
;;           key < every key in right BST

(define my-bst (make-node 5
                 (make-node 3 empty empty)
                 (make-node 9 (make-node 7 empty empty) empty)))

(define another-bst (make-node 5
                      (make-node 3
                        (make-node 1 (make-node 0 empty empty)
                                     (make-node 2 empty empty))
                        (make-node 4 empty empty))
                      (make-node 9 empty empty)))     


;; ==== Question 2a ========================

;; (bst-count tree) produces the total number of nodes in tree
;; bst-count: BST -> Nat
;; Examples:
(check-expect (bst-count my-bst) 4)
(check-expect (bst-count another-bst) 7)

(define (bst-count tree)
  (cond [(empty? tree) 0]
        [else (+ 1 (bst-count (node-left tree))
                   (bst-count (node-right tree)))]))
;; Tests:
(check-expect (bst-count empty) 0)
(check-expect (bst-count (make-node 0 empty empty)) 1)
(check-expect (bst-count (make-node 4 (make-node 3 empty empty) empty)) 2)
(check-expect (bst-count (make-node 2 empty (make-node 3 empty empty))) 2)
(check-expect (bst-count (make-node 12
                           (make-node 10 empty empty)
                           (make-node 13 empty empty))) 3)
(check-expect (bst-count (make-node 12
                           (make-node 10 (make-node 0 empty empty) empty)
                           (make-node 13 empty empty))) 4)


;; ==== Question 2b ========================

;; (bst-add key tree) produces a new BST with key added to tree
;;   where the new node contains the key
;; bst-add: Nat BST -> BST
;; Examples:
(check-expect (bst-add 4 my-bst)
              (make-node 5
                 (make-node 3 empty (make-node 4 empty empty))
                 (make-node 9 (make-node 7 empty empty) empty)))
(check-expect (bst-add 7 another-bst)
              (make-node 5
                      (make-node 3
                        (make-node 1 (make-node 0 empty empty)
                                     (make-node 2 empty empty))
                        (make-node 4 empty empty))
                      (make-node 9 (make-node 7 empty empty) empty))) 

(define (bst-add key tree)
  (cond [(empty? tree) (make-node key empty empty)]
        [(> key (node-key tree)) (make-node (node-key tree) (node-left tree)
                                            (bst-add key (node-right tree)))]
        [(< key (node-key tree)) (make-node (node-key tree)
                                            (bst-add key (node-left tree))
                                            (node-right tree))]
        [else tree]))

;; Tests:
(check-expect (bst-add 0 empty) (make-node 0 empty empty))
(check-expect (bst-add 2 (make-node 0 empty empty))
              (make-node 0 empty (make-node 2 empty empty)))
(check-expect (bst-add 5 (make-node 4 (make-node 3 empty empty) empty))
              (make-node 4 (make-node 3 empty empty)
                           (make-node 5 empty empty)))
(check-expect (bst-add 4 (make-node 2 empty (make-node 3 empty empty)))
              (make-node 2 empty
                           (make-node 3 empty (make-node 4 empty empty))))
(check-expect (bst-add 4 (make-node 2 empty
                           (make-node 3 empty (make-node 4 empty empty))))
              (make-node 2 empty
                           (make-node 3 empty (make-node 4 empty empty))))
(check-expect (bst-add 9 (make-node 12
                           (make-node 10 empty empty)
                           (make-node 13 empty empty)))
              (make-node 12
                           (make-node 10 (make-node 9 empty empty) empty)
                           (make-node 13 empty empty)))
(check-expect (bst-add 5 (make-node 12
                           (make-node 10 (make-node 8 empty empty) empty)
                           (make-node 13 empty empty)))
              (make-node 12
                           (make-node 10 (make-node 8 (make-node 5 empty empty)
                                                    empty)
                                      empty)
                           (make-node 13 empty empty)))


;; ==== Question 2c ========================

;; (bst-height tree) outputs the maximum distance between the root of tree
;;   and its leaves
;; bst-height: BST -> Nat
;; Examples:
(check-expect (bst-height my-bst) 2)
(check-expect (bst-height another-bst) 3)

(define (bst-height tree)
  (cond [(empty? tree) 0]
        [(and (empty? (node-left tree)) (empty? (node-right tree))) 0]
        [else (add1 (max (bst-height (node-left tree))
                         (bst-height (node-right tree))))]))

;; Tests:
(check-expect (bst-height empty) 0)
(check-expect (bst-height (make-node 0 empty empty)) 0)
(check-expect (bst-height (make-node 4 (make-node 3 empty empty) empty)) 1)
(check-expect (bst-height (make-node 2 empty (make-node 3 empty empty))) 1)
(check-expect (bst-height (make-node 12
                           (make-node 10 empty empty)
                           (make-node 13 empty empty))) 1)
(check-expect (bst-height (make-node 12
                           (make-node 10 (make-node 0 empty empty) empty)
                           (make-node 13 empty empty))) 2)
(check-expect
 (bst-height
  (make-node 10
             (make-node 3 (make-node 0 empty empty)
                        (make-node 5 (make-node 4 empty empty)
                                   (make-node 8
                                              (make-node 6 empty
                                                         (make-node 7
                                                                    empty
                                                                    empty))
                                              empty)))
             (make-node 13 (make-node 12 (make-node 11 empty empty)
                                     empty) empty)))
 5)



;; ==== Question 2d ========================

;; (bst-balanced? tree) outputs true if tree is balanced through checking 
;;   the height difference between every left and right sub-tree of all nodes 
;;   to be less than 2, if not, returns false
;; bst-balanced?: BST -> Bool
;; Examples:
(check-expect (bst-balanced? my-bst) true)
(check-expect (bst-balanced? another-bst) false)

(define (bst-balanced? tree)
  (cond [(empty? tree) true]
        [(and (> (bst-height tree) 1)
              (or (empty? (node-left tree))
                  (empty? (node-right tree)))) false]
        [(< (abs (- (bst-height (node-left tree))
                    (bst-height (node-right tree)))) 2)
         (and (bst-balanced? (node-left tree))
              (bst-balanced? (node-right tree)))]
        [else false]))

;; Tests:
(check-expect (bst-balanced? empty) true)
(check-expect (bst-balanced? (make-node 0 empty empty)) true)
(check-expect
 (bst-balanced? (make-node 1 (make-node 0 empty empty) empty)) true)
(check-expect
 (bst-balanced? (make-node 2 empty (make-node 3 empty empty))) true)
(check-expect
 (bst-balanced? (make-node 5
                           (make-node 3 (make-node 1 empty empty)
                                      empty)
                           empty))
 false)
(check-expect (bst-balanced? (make-node 5
                                        (make-node 3 (make-node 1 empty empty)
                                                   empty)
                                        (make-node 6 empty empty)))
              true)
(check-expect
 (bst-balanced?
  (make-node 9 (make-node 5
                          (make-node 3
                                     (make-node 2
                                                (make-node 1 empty empty)
                                                empty)
                                     (make-node 4 empty empty))
                          (make-node 7 (make-node 6 empty empty)
                                     (make-node 8 empty empty)))
             (make-node 11
                        (make-node 10 empty empty)
                        (make-node 13
                                   (make-node 12 empty empty)
                                   (make-node 14 empty empty)))))
 true)
(check-expect
 (bst-balanced?
  (make-node 9 (make-node 5
                          (make-node 3
                                     (make-node 2
                                                (make-node 1 empty empty)
                                                empty)
                                     (make-node 4 empty empty))
                          (make-node 7
                                     (make-node 6 empty empty)
                                     (make-node 8 empty empty)))
             (make-node 11 empty
                        (make-node 13 (make-node 12 empty empty)
                                   (make-node 14 empty empty)))))
 false)
(check-expect
 (bst-balanced?
  (make-node 5 (make-node 2 (make-node 0 empty empty)
                          (make-node 3 empty
                                     (make-node 4 empty empty)))
             (make-node 8 empty (make-node 6 empty empty))))
 true)
(check-expect
 (bst-balanced?
  (make-node 5 (make-node 2 (make-node 0 empty empty)
                          (make-node 3 empty (make-node 4 empty empty)))
             (make-node 8 empty empty)))
 false)









