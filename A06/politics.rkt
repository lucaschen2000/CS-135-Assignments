;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname politics) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 06, Question 3
;; *****************************************
;;

;; A Perk is a (list Nat Str)
;; requires: Nat > 0

;; A Perk-list is either
;; * empty
;; * (cons (list Str (listof Perk)) Perk-list)
;; requires: Perks in (listof Perk) sorted by compliancy score
;; (non-increasing)

(define short-perklist
  (list
   (list "Peter Smith" (list
                        (list 83 "50,000 dollars campaign funding")
                        (list 32 "Public support by your company")
                        (list 13 "Opera tickets")))
   (list "Jennifer O'Brien"
         (list
          (list 137 "Position on the Board of Directors")
          (list 22 "Arranging photo-op with CEO")))
   (list "Steve Li" (list
                     (list 91 "Sponsored TV ads")
                     (list 56 "Invitation as keynote-speaker")
                     (list 9 "Positive press release in his favour")
                     (list 5 "Business dinner with CTO")))))

;; ==== Question 3a ========================

;; (merge-perks items perkline) combines two lists of Perks (items and
;;   perkline), if a perk in items with the same compliancy score is added
;;   to perklist, it takes precedence. Non-increasing order is maintained
;; merge-perks: (listof Perk) (listof Perk) -> (listof Perk)
;; Examples:
(check-expect
 (merge-perks (list (list 137 "Position on the Board of Directors")
                    (list 22 "Arranging photo-op with CEO"))
              (list (list 30 "Two free flights in company jet")
                    (list 3 "Guided company tour")))
 (list (list 137 "Position on the Board of Directors")
       (list 30 "Two free flights in company jet")
       (list 22 "Arranging photo-op with CEO")
       (list 3 "Guided company tour")))

(define (merge-perks items perkline)
  (cond [(empty? perkline) items]
        [(empty? items) perkline]
        [(< (first (first items)) (first (first perkline)))
         (cons (first perkline) (merge-perks items (rest perkline)))]
        [(>= (first (first items)) (first (first perkline)))
         (cons (first items) (merge-perks (rest items) perkline))]))

;; (add-perks politician items perklist) merges politician's list of perks
;;   in perklist with items if politician is in perklist and if politician does
;;   not exist, politician with his/her items is added to the end of perklist
;; add-perks: Str (listof Perk) Perk-list -> Perk-list
;; Examples:
(check-expect
 (add-perks "Jennifer O'Brien"
            (list (list 30 "Two free flights in company jet")
                  (list 3 "Guided company tour")) short-perklist)
 (list
  (list "Peter Smith" (list
                       (list 83 "50,000 dollars campaign funding")
                       (list 32 "Public support by your company")
                       (list 13 "Opera tickets")))
  (list "Jennifer O'Brien" (list
                            (list 137 "Position on the Board of Directors")
                            (list 30 "Two free flights in company jet")
                            (list 22 "Arranging photo-op with CEO")
                            (list 3 "Guided company tour")))
  (list "Steve Li" (list
                    (list 91 "Sponsored TV ads")
                    (list 56 "Invitation as keynote-speaker")
                    (list 9 "Positive press release in his favour")
                    (list 5 "Business dinner with CTO")))))

(define (add-perks politician items perklist)
  (cond [(empty? perklist) (list (list politician items))]
        [(string=? politician (first (first perklist)))
         (cons (list (first (first perklist))
                     (merge-perks items (second (first perklist))))
               (rest perklist))]
        [else (cons (first perklist)
                    (add-perks politician items (rest perklist)))]))

;; Tests:
(check-expect
 (add-perks "Jennifer O'Brien"
            (list (list 30 "Two free flights in company jet")
                  (list 3 "Guided company tour")) empty)
 (list (list "Jennifer O'Brien"
             (list (list 30 "Two free flights in company jet")
                   (list 3 "Guided company tour")))))

(check-expect
 (add-perks "Lucas Chen" (list (list 3 "Get free internet"))
            (list
             (list "Lucas Chen"
                   (list (list 5 "Become president of Waterloo")))))
 (list (list "Lucas Chen" (list (list 5 "Become president of Waterloo")
                                (list 3 "Get free internet")))))

(check-expect
 (add-perks "Lucas Chen" (list (list 5 "Become president of Waterloo"))
            (list (list "Kevin Li" (list (list 10 "Free figurine")))))
 (list (list "Kevin Li" (list (list 10 "Free figurine")))
       (list "Lucas Chen" (list (list 5 "Become president of Waterloo")))))

(check-expect
 (add-perks "Jennifer O'Brien"
            (list (list 30 "Two free flights in company jet")
                  (list 22 "Free burger")
                  (list 3 "Guided company tour")) short-perklist)
 (list
  (list "Peter Smith" (list
                       (list 83 "50,000 dollars campaign funding")
                       (list 32 "Public support by your company")
                       (list 13 "Opera tickets")))
  (list "Jennifer O'Brien" (list
                            (list 137 "Position on the Board of Directors")
                            (list 30 "Two free flights in company jet")
                            (list 22 "Free burger")
                            (list 22 "Arranging photo-op with CEO")
                            (list 3 "Guided company tour")))
  (list "Steve Li" (list
                    (list 91 "Sponsored TV ads")
                    (list 56 "Invitation as keynote-speaker")
                    (list 9 "Positive press release in his favour")
                    (list 5 "Business dinner with CTO")))))

(check-expect
 (add-perks "Lucas Chen" (list (list 16 "Play videogames")
                               (list 5 "Become president of Waterloo"))
 (list
  (list "Peter Smith" (list
                       (list 83 "50,000 dollars campaign funding")
                       (list 32 "Public support by your company")
                       (list 13 "Opera tickets")))
  (list "Jennifer O'Brien" (list
                            (list 137 "Position on the Board of Directors")
                            (list 30 "Two free flights in company jet")
                            (list 22 "Free burger")
                            (list 22 "Arranging photo-op with CEO")
                            (list 3 "Guided company tour")))
  (list "Steve Li" (list
                    (list 91 "Sponsored TV ads")
                    (list 56 "Invitation as keynote-speaker")
                    (list 9 "Positive press release in his favour")
                    (list 5 "Business dinner with CTO")))))
 (list
  (list "Peter Smith" (list
                       (list 83 "50,000 dollars campaign funding")
                       (list 32 "Public support by your company")
                       (list 13 "Opera tickets")))
  (list "Jennifer O'Brien" (list
                            (list 137 "Position on the Board of Directors")
                            (list 30 "Two free flights in company jet")
                            (list 22 "Free burger")
                            (list 22 "Arranging photo-op with CEO")
                            (list 3 "Guided company tour")))
  (list "Steve Li" (list
                    (list 91 "Sponsored TV ads")
                    (list 56 "Invitation as keynote-speaker")
                    (list 9 "Positive press release in his favour")
                    (list 5 "Business dinner with CTO")))
  (list "Lucas Chen" (list (list 16 "Play videogames")
                           (list 5 "Become president of Waterloo")))))


;; ==== Question 3b ========================

;; (what-perk score perkline) outputs what reward is given based on score
;;   and rewards available in perkline
;; what-perk: Nat (listof Perk) -> (anyof 'wristwatch Str)
;; Examples:
(check-expect (what-perk 25
                         (list (list 137 "Position on the Board of Directors")
                               (list 22 "Arranging photo-op with CEO")))
              "Arranging photo-op with CEO")

(define (what-perk score perkline)
  (cond [(empty? perkline) 'wristwatch]
        [(>= score (first (first perkline))) (second (first perkline))]
        [else (what-perk score (rest perkline))]))

;; (perk-received politician score perklist) outputs a a reward, 'wristwatch
;;   or 'smear-campaign depending on score and the politician who may or may
;;   not be in perklist
;; perk-received: Str Int Perk-list -> (anyof Str 'wristwatch 'smear-campaign)
;; Examples:


(define (perk-received politician score perklist)
  (cond [(< score 0) 'smear-campaign]
        [(empty? perklist) 'wristwatch]
        [(string=? politician (first (first perklist)))
         (what-perk score (second (first perklist)))]
        [else (perk-received politician score (rest perklist))]))

;; Tests:
(check-expect (perk-received "Lucas Chen" -1 empty) 'smear-campaign)
(check-expect (perk-received "Lucas Chen" 0 empty) 'wristwatch)
(check-expect (perk-received "Lucas Chen" 100
                 (list
                  (list "Lucas Chen"
                        (list
                         (list 101 "Become King of Food")
                         (list 77 "Become King of Burgers")
                         (list 36 "Become King of Fries")
                         (list 1 "Free Lollipop")))))
              "Become King of Burgers")
(check-expect (perk-received "Yolanda Yu" 100
                 (list
                  (list "Lucas Chen"
                        (list
                         (list 101 "Become King of Food")
                         (list 77 "Become King of Burgers")
                         (list 36 "Become King of Fries")
                         (list 1 "Free Lollipop")))))
              'wristwatch)
(check-expect (perk-received "Lucas Chen" 1
                 (list
                  (list "Lucas Chen"
                        (list
                         (list 101 "Become King of Food")
                         (list 77 "Become King of Burgers")
                         (list 36 "Become King of Fries")
                         (list 1 "Free Lollipop")))))
              "Free Lollipop")
(check-expect (perk-received "Lucas Chen" 50
          (list
           (list "Peter Smith" (list
            (list 83 "50,000 dollars campaign funding")
            (list 32 "Public support by your company")
            (list 13 "Opera tickets")))
           (list "Jennifer O'Brien"
                 (list
                  (list 137 "Position on the Board of Directors")
                  (list 30 "Two free flights in company jet")
                  (list 22 "Free burger")
                  (list 22 "Arranging photo-op with CEO")
                  (list 3 "Guided company tour")))
           (list "Steve Li" (list
                             (list 91 "Sponsored TV ads")
                             (list 56 "Invitation as keynote-speaker")
                             (list 9 "Positive press release in his favour")
                             (list 5 "Business dinner with CTO")))
           (list "Lucas Chen" (list (list 16 "Play videogames")
                                    (list 5 "Become president of Waterloo")))))
              "Play videogames")
(check-expect (perk-received "Jennifer O'Brien" 300 short-perklist)
              "Position on the Board of Directors")
(check-expect (perk-received "Jennifer O'Brien" 25 short-perklist)
              "Arranging photo-op with CEO")
(check-expect (perk-received "Jennifer O'Brien" 15 short-perklist)
              'wristwatch)
(check-expect (perk-received "Jennifer O'Brien" -15 short-perklist)
              'smear-campaign)
(check-expect (perk-received "Noton Thelist" 43 short-perklist)
              'wristwatch)
(check-expect (perk-received "Noton Thelist" -43 short-perklist)
              'smear-campaign)
(check-expect (perk-received "Steve Li" 12 short-perklist)
              "Positive press release in his favour")
(check-expect (perk-received "Peter Smith" -25 short-perklist) 'smear-campaign)


;; ==== Question 3c ========================

;; An Action is a (list Str Int Str)

(define-struct actionnode (name score actions left right))
;; An ActionNode is a (make-actionnode Str Int (listof Action)
;;                                     ActionSearchTree ActionSearchTree)
;; requires:
;;  (string<? x name) is true for every (actionnode-name x)
;;    in the left subtree
;;  (string>? x name) is true for every (actionnode-name x)
;;    in the right subtree
;;
;; An ActionSearchTree is one of:
;; * empty
;; * an ActionNode

(define short-ast (make-actionnode "Amanda Byers" -5 (list
    (list "Amanda Byers" -5 "Met with competitor"))
                                   empty empty))

;; (add-action action tree) either adds a politician, score, and their action
;;   to tree if politician doesn't exist or updates an exisiting politician's
;;   list of actions if the specified politician exists in tree with action
;; add-action: Action ActionSearchTree -> ActionSearchTree
;; Examples:
(check-expect (add-action (list "Kevin Li" 420 "Having a relaxing personality")
              empty)
              (make-actionnode "Kevin Li" 420
                (list (list "Kevin Li" 420 "Having a relaxing personality"))
                empty empty))

(define (add-action action tree)
  (cond [(empty? tree)
         (make-actionnode (first action) (second action)
          (list action)
          empty empty)]
        [(string=? (first action) (actionnode-name tree))
         (make-actionnode (actionnode-name tree)
                          (+ (second action) (actionnode-score tree))
                          (cons action (actionnode-actions tree))
                          (actionnode-left tree) (actionnode-right tree))]
        [(string<? (first action) (actionnode-name tree))
         (make-actionnode (actionnode-name tree) (actionnode-score tree)
                          (actionnode-actions tree)
                          (add-action action (actionnode-left tree))
                          (actionnode-right tree))]
        [(string>? (first action) (actionnode-name tree))
         (make-actionnode (actionnode-name tree) (actionnode-score tree)
                          (actionnode-actions tree)
                          (actionnode-left tree)
                          (add-action action (actionnode-right tree)))]))

;; Tests:
(check-expect
 (add-action (list "Amanda Byers" -5 "Met with competitor") empty) short-ast)
(check-expect
 (add-action (list "Steve Li" 12 "Plays golf with your second cousin")
             short-ast)
 (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor")) empty
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))
                    empty empty)))
(check-expect
 (add-action (list "Abby Ames" 5 "Plays scoot with the bully")
             short-ast)
 (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty) empty))

(check-expect
 (add-action (list "Abby Ames" 13 "Visits the next door bully")
  (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))
                    empty empty)))
 (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 18 (list
                    (list "Abby Ames" 13 "Visits the next door bully")
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))
                    empty empty)))

(check-expect
 (add-action (list "Mister Mads" 69 "Plays badminton with Ron")
  (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))
                    empty empty)))
 (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))

     (make-actionnode
      "Mister Mads" 69
      (list (list "Mister Mads" 69 "Plays badminton with Ron")) empty empty)
     empty)))

(check-expect
 (add-action (list "Mister Mads" -33 "Hates math")
  (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))

     (make-actionnode
      "Mister Mads" 69
      (list (list "Mister Mads" 69 "Plays badminton with Ron")) empty empty)
     empty)))
 (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))

     (make-actionnode
      "Mister Mads" 36
      (list (list "Mister Mads" -33 "Hates math")
            (list "Mister Mads" 69 "Plays badminton with Ron")) empty empty)
     empty)))

(check-expect
 (add-action (list "Mister Mads" -33 "Hates math")
  (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))

     (make-actionnode
      "Mister Mads" 69
      (list (list "Mister Mads" 69 "Plays badminton with Ron")) empty empty)
     empty)))
 (make-actionnode
  "Amanda Byers" -5
  (list (list "Amanda Byers" -5 "Met with competitor"))
    (make-actionnode
     "Abby Ames" 5 (list
                    (list "Abby Ames" 5 "Plays scoot with the bully"))
                    empty empty)
    (make-actionnode
     "Steve Li" 12 (list
                    (list "Steve Li" 12 "Plays golf with your second cousin"))
     (make-actionnode
      "Mister Mads" 36
      (list (list "Mister Mads" -33 "Hates math")
            (list "Mister Mads" 69 "Plays badminton with Ron")) empty empty)
     empty)))

(check-expect
 (add-action (list "Amanda Byers" 7
                   "Argued on talk radio against raising minimum wage")
             short-ast)
 (make-actionnode
  "Amanda Byers" 2
  (list (list
         "Amanda Byers" 7 "Argued on talk radio against raising minimum wage")
        (list "Amanda Byers" -5 "Met with competitor")) empty empty))


;; ==== Question 3d ========================

;; (name-and-score tree) outputs a list of length two with the first element
;;   being a string (politician name) and the second being an integer (score)
;;   of the current node (a.k.a parent node in tree)
;; name-and-score: ActionSearchTree -> (list Str Int)
;; requires: tree is nonempty
;; Examples:
(check-expect (name-and-score
               (make-actionnode "Pedram" 500
                                (list (list "Pedram" 500 "Eats strawberries"))
                                empty empty))
              (list "Pedram" 500))

(define (name-and-score tree)
  (list (actionnode-name tree) (actionnode-score tree)))

;; (flatten-sort-my-tree tree) flattens tree and alphabetically sorts the names
;;   of politicians in tree alone with their score
;; flatten-sort-my-tree: ActionSearchTree -> (listof (list Str Int))
;; Examples:
(check-expect (flatten-sort-my-tree
 (make-actionnode "Amanda Byers" -5
                 (list (list "Amanda Byers" -5 "Met with competitor")) empty
  (make-actionnode "Steve Li" 12
    (list (list "Steve Li" 12 "Plays golf with your second cousin"))
    (make-actionnode "Jennifer O'Brien" 25
                     (list
                      (list "Jennifer O'Brien" 30 "Pushed major contract 
                                                          for your company")
                      (list "Jennifer O'Brien" 5 "Mentioned your company on
                                                          morning TV")
                      (list "Jennifer O'Brien" -10 "Questioned your leadership
                                                        in public"))
                     empty empty) empty)))
 (list (list "Amanda Byers" -5)
       (list "Jennifer O'Brien" 25)
       (list "Steve Li" 12)))

(define (flatten-sort-my-tree tree)
  (cond [(empty? tree) empty]
        [(and (empty? (actionnode-left tree))
              (actionnode? (actionnode-right tree)))
         (append (list (name-and-score tree))
                 (flatten-sort-my-tree (actionnode-right tree)))]
        [(and (empty? (actionnode-right tree))
              (actionnode? (actionnode-left tree)))
         (append (flatten-sort-my-tree (actionnode-left tree))
                 (list (name-and-score tree)))]
        [(and (actionnode? (actionnode-left tree))
              (actionnode? (actionnode-right tree)))
         (append (flatten-sort-my-tree (actionnode-left tree))
                 (list (name-and-score tree))
                 (flatten-sort-my-tree (actionnode-right tree)))]
        [else (list (name-and-score tree))]))

;; (find-perk politician-scores perklist) outputs a list of politicians
;;   with their corresponding perk received depending on politician-scores
;;   and perklist in alphabetical order
;; find-perk: (listof (list Str Int)) Perk-list ->
;;            (listof (list Str (anyof 'wristwatch 'smear-campaign Str)))
;; Examples:
(check-expect
 (find-perk
  (list (list "Amanda Byers" -5)
        (list "Jennifer O'Brien" 25)
        (list "Steve Li" 12))
(list
 (list "Jennifer O'Brien" (list
 (list 137 "Position on the Board of Directors")
 (list 30 "Two free flights in company jet")
 (list 22 "Arranging photo-op with CEO")
 (list 3 "Guided company tour")))
 (list "Steve Li" (list
 (list 91 "Sponsored TV ads")
 (list 56 "Invitation as keynote-speaker")
 (list 9 "Positive press release in his favour")
 (list 5 "Business dinner with CTO")))))
(list
  (list "Amanda Byers" 'smear-campaign)
  (list "Jennifer O'Brien" "Arranging photo-op with CEO")
  (list "Steve Li" "Positive press release in his favour")))

(define (find-perk politician-scores perklist)
  (cond [(empty? politician-scores) empty]
        [else (cons (list (first (first politician-scores))
                          (perk-received (first (first politician-scores))
                                         (second (first politician-scores))
                                         perklist))
                    (find-perk (rest politician-scores) perklist))]))

;; (perk-list ast perklist) outputs a list of politicians in ast
;;   with their corresponding perk received depending on perklist
;;   in alphabetical order
;; perk-list: ActionSearchTree Perk-list ->
;;            (listof (list Str (anyof 'wristwatch 'smear-campaign Str)))
;; Examples:
(check-expect (perk-list (make-actionnode "Amanda Byers" -5
 (list (list "Amanda Byers" -5 "Met with competitor")) empty
 (make-actionnode "Steve Li" 12
 (list (list "Steve Li" 12 "Plays golf with your second cousin"))
 (make-actionnode "Jennifer O'Brien" 25
 (list (list "Jennifer O'Brien" 30 "Pushed major contract for your company")
 (list "Jennifer O'Brien" 5 "Mentioned your company on morning TV")
 (list "Jennifer O'Brien" -10 "Questioned your leadership in public"))
 empty empty) empty))
 (list
 (list "Jennifer O'Brien" (list
 (list 137 "Position on the Board of Directors")
 (list 30 "Two free flights in company jet")
 (list 22 "Arranging photo-op with CEO")
 (list 3 "Guided company tour")))
 (list "Steve Li" (list
 (list 91 "Sponsored TV ads")
 (list 56 "Invitation as keynote-speaker")
 (list 9 "Positive press release in his favour")
 (list 5 "Business dinner with CTO")))))
(list
 (list "Amanda Byers" 'smear-campaign)
 (list "Jennifer O'Brien" "Arranging photo-op with CEO")
 (list "Steve Li" "Positive press release in his favour")))

(define (perk-list ast perklist)
  (find-perk (flatten-sort-my-tree ast) perklist))

;; Tests:
(check-expect (perk-list empty empty) empty)
(check-expect (perk-list (make-actionnode "Amanda Byers" -5
 (list (list "Amanda Byers" -5 "Met with competitor")) empty empty) empty)
              (list (list "Amanda Byers" 'smear-campaign)))
(check-expect (perk-list (make-actionnode "Amanda Byers" 5
 (list (list "Amanda Byers" 5 "Met with friend")) empty empty) empty)
              (list (list "Amanda Byers" 'wristwatch)))
(check-expect (perk-list empty (list
 (list "Jennifer O'Brien" (list
 (list 137 "Position on the Board of Directors")
 (list 30 "Two free flights in company jet")
 (list 22 "Arranging photo-op with CEO")
 (list 3 "Guided company tour")))
 (list "Steve Li" (list
 (list 91 "Sponsored TV ads")
 (list 56 "Invitation as keynote-speaker")
 (list 9 "Positive press release in his favour")
 (list 5 "Business dinner with CTO")))))
              empty)
(check-expect (perk-list
               (make-actionnode "Amanda Byers" 140
 (list (list "Amanda Byers" 140 "Met with my mom")) empty empty)
       (list
        (list "Amanda Byers"
              (list (list 137 "Position on the Board of Directors")))))
 (list (list "Amanda Byers" "Position on the Board of Directors")))
(check-expect (perk-list
               (make-actionnode "Amanda Byers" -5
 (list (list "Amanda Byers" -5 "Met with my mom")) empty empty)
       (list
        (list "Amanda Byers"
              (list (list 137 "Position on the Board of Directors")))))
 (list (list "Amanda Byers" 'smear-campaign)))
(check-expect (perk-list
               (make-actionnode "Amanda Byers" 140
 (list (list "Amanda Byers" 140 "Met with my mom"))
 (make-actionnode "Abby Ames" 130 (list "Abby Ames" 57 "Played Coup with me")
                  empty empty) empty)
       (list
        (list "Amanda Byers"
              (list (list 137 "Position on the Board of Directors")))
        (list "Abby Ames"
              (list (list 57 "Played Coup with me")))))
 (list (list "Abby Ames" "Played Coup with me")
       (list "Amanda Byers" "Position on the Board of Directors")))

(check-expect (perk-list
               (make-actionnode "Amanda Byers" 140
 (list (list "Amanda Byers" 140 "Met with the President"))
 (make-actionnode "Abby Ames" 130 (list "Abby Ames" 57 "Played Coup with me")
                  empty empty)
 (make-actionnode "Yolanda Yu" -20 (list "Yolanda Yu" -20 "Made fun of me")
                  empty empty))
       (list
        (list "Amanda Byers"
              (list (list 137 "Position on the Board of Directors")))
        (list "Abby Ames"
              (list (list 57 "Gets to play Coup with me")))
        (list "Yolanda Yu"
              (list (list 20 "Receives a sandwich")))))
 (list
  (list "Abby Ames" "Gets to play Coup with me")
  (list "Amanda Byers" "Position on the Board of Directors")
  (list "Yolanda Yu" 'smear-campaign)))

(check-expect (perk-list
               (make-actionnode "Amanda Byers" 140
 (list (list "Amanda Byers" 140 "Met with the President"))
 (make-actionnode "Abby Ames" 130 (list "Abby Ames" 57 "Played Coup with me")
                  empty empty)
 (make-actionnode "Yolanda Yu" -20 (list "Yolanda Yu" -20 "Made fun of me")
 (make-actionnode "Oliver Song" 30
                  (list "Oliver Song" 30 "Switched to my sequence" empty)
                  empty empty) empty))
       (list
        (list "Amanda Byers"
              (list (list 137 "Position on the Board of Directors")))
        (list "Abby Ames"
              (list (list 57 "Gets to play Coup with me")))
        (list "Oliver Song"
              (list (list 40 "Switched to my sequence")))
        (list "Yolanda Yu"
              (list (list 20 "Receives a sandwich")))))
 (list
  (list "Abby Ames" "Gets to play Coup with me")
  (list "Amanda Byers" "Position on the Board of Directors")
  (list "Oliver Song" 'wristwatch)
  (list "Yolanda Yu" 'smear-campaign)))

(check-expect (perk-list
               (make-actionnode "Amanda Byers" 90
 (list (list "Amanda Byers" 90 "Met with the Vice-President"))
 (make-actionnode "Abby Ames" 130 (list "Abby Ames" 57 "Played Coup with me")
                  empty empty)
 (make-actionnode "Yolanda Yu" -20 (list "Yolanda Yu" -20 "Made fun of me")
 (make-actionnode "Oliver Song" 30
                  (list "Oliver Song" 30 "Switched to my sequence" empty)
                  empty empty) empty))
       (list
        (list "Amanda Byers"
              (list (list 137 "Position on the Board of Directors")
                    (list 85 "Position as Project Manager")))
        (list "Abby Ames"
              (list (list 57 "Gets to play Coup with me")))
        (list "Oliver Song"
              (list (list 40 "Switched to my sequence")))
        (list "Yolanda Yu"
              (list (list 20 "Receives a sandwich")))))
 (list
  (list "Abby Ames" "Gets to play Coup with me")
  (list "Amanda Byers" "Position as Project Manager")
  (list "Oliver Song" 'wristwatch)
  (list "Yolanda Yu" 'smear-campaign)))








