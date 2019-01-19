;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname immigration) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Lucas Chen (20761547)
;; CS 135 Fall 2018
;; Assignment 02, Question 2
;; *****************************************
;;

;; ==== Question 2a ========================

;; (age-score age) outputs a score based on an applicant's age
;; age-score: Nat -> Nat
;; Examples:
(check-expect (age-score 19) 95)
(check-expect (age-score 0) 0)

;; Scores given for age groups
(define age-score-dependents 0)
(define age-score-20-to-29 100)

(define (age-score age)
  (cond [(or (< age 18)(> age 48)) age-score-dependents]
        [(<= 18 age 19) (* age 5)]
        [(and (>= age 20)(<= age 29)) age-score-20-to-29]
        [else (* (- 49 age) 5)]))

;; (education-score education) outputs a score
;;   based on an applicant's education level
;; education-score: Sym -> Nat
;; requires: education is any of: 'graduate, 'undergraduate or 'highschool
;; Examples
(check-expect (education-score 'graduate) 126)
(check-expect (education-score 'highschool) 28)

;; Score awarded for education level
(define graduate-score 126)
(define undergraduate-score 112)
(define highschool-score 28)

(define (education-score education)
  (cond [(symbol=? education 'graduate) graduate-score]
        [(symbol=? education 'undergraduate) undergraduate-score]
        [else highschool-score]))

;; (language-score proficiency) outputs a score
;;   based on an applicant's language proficiency
;; language-score: Nat -> Nat
;; requires: 1 <= proficiency <= 10
;; Examples:
(check-expect (language-score 10) 116)
(check-expect (language-score 3) 0)

;; Score awarded for language proficiency
(define lang-score-9/10 116)
(define lang-score-8 88)
(define lang-score-7 64)
(define lang-score-1-to-7 0)

(define (language-score proficiency)
  (cond [(<= 9 proficiency 10) lang-score-9/10]
        [(= proficiency 8) lang-score-8]
        [(= proficiency 7) lang-score-7]
        [else lang-score-1-to-7]))

;; (work-score experience) outputs a score based
;;   on an applicant's work experience (in years)
;; work-score: Nat -> Nat
;; Examples:
(check-expect (work-score 0) 0)
(check-expect (work-score 5) 70)

;; Score awarded based on work experience
(define work-score-no-experience 0)
(define work-score-1-year 35)
(define work-score-2/3-years 56)
(define work-score-more-than-3-years 70)

(define (work-score experience)
  (cond [(= experience 0) work-score-no-experience]
        [(= experience 1) work-score-1-year]
        [(<= experience 3) work-score-2/3-years]
        [else work-score-more-than-3-years]))

;; (job-score job-offer) outputs a score based
;;   on the job status of the applicant in Canada
;; job-score: Bool -> Nat
;; Examples:
(check-expect (job-score true) 200)
(check-expect (job-score false) 0)

;; Score awarded for job offers
(define score-with-job-offer 200)
(define score-without-job-offer 0)

(define (job-score job-offer)
  (cond [job-offer score-with-job-offer]
        [else score-without-job-offer]))

;; (pr-cec-score age education proficiency experience job-offer)
;;   outputs a score based on an applicant's age, education level,
;;   language proficiency, job experience (in years), and their job status
;; pr-cec-score: Nat Sym Nat Nat Bool -> Nat
;; requires: education is any of 'graduate, 'undergraduate or 'highschool
;;           1 <= proficiency <= 10
;; Examples:
(check-expect (pr-cec-score 40 'graduate 6 0 false) 171)
(check-expect (pr-cec-score 25 'undergraduate 6 3 true) 468)

(define (pr-cec-score age education proficiency experience job-offer)
  (+ (age-score age) (education-score education)
     (language-score proficiency)(work-score experience)
     (job-score job-offer)))

;; Tests:
(check-expect (pr-cec-score 16 'highschool 1 0 false) 28)
(check-expect (pr-cec-score 18 'highschool 6 1 false) 153)
(check-expect (pr-cec-score 19 'undergraduate 7 2 false) 327)
(check-expect (pr-cec-score 20 'undergraduate 8 3 false) 356)
(check-expect (pr-cec-score 29 'graduate 9 4 true) 612)
(check-expect (pr-cec-score 30 'graduate 10 5 true) 607)
(check-expect (pr-cec-score 48 'graduate 1 6 true) 401)
(check-expect (pr-cec-score 49 'graduate 1 7 true) 396)
 

;; ==== Question 2b ========================

;; (pr-cec-eligible? age education proficiency experience job-offer)
;;   outputs a boolean indicating whether or not an applicant
;;   is eligible for the Canadian Experience Class category (at least
;;   350 points for permanent residency)
;; pr-cec-score: Nat Sym Nat Nat Bool -> Nat
;; requires: education is any of 'graduate, 'undergraduate or 'highschool
;;           1 <= proficiency <= 10
;; Examples:
(check-expect (pr-cec-eligible? 40 'graduate 6 0 false) false)
(check-expect (pr-cec-eligible? 25 'undergraduate 6 3 true) true)

(define (pr-cec-eligible? age education proficiency experience job-offer)
  (cond [(>= (pr-cec-score age education
                          proficiency experience job-offer) 350) true]
        [else false]))

;; Tests:
(check-expect (pr-cec-eligible? 16 'highschool 1 0 false) false)
(check-expect (pr-cec-eligible? 18 'highschool 6 1 false) false)
(check-expect (pr-cec-eligible? 19 'undergraduate 7 2 false) false)
(check-expect (pr-cec-eligible? 20 'undergraduate 8 3 false) true)
(check-expect (pr-cec-eligible? 29 'graduate 9 4 true) true)
(check-expect (pr-cec-eligible? 30 'graduate 10 5 true) true)
(check-expect (pr-cec-eligible? 48 'graduate 1 6 true) true)
(check-expect (pr-cec-eligible? 49 'graduate 1 7 true) true)