#lang racket
(require rackunit)
(require rackunit/text-ui)

;CS 270
;Homework 7
;Professor B. Char, M. Boady,  J. Johnson, S. Earth, and G. Long
;Name: Tram Phan

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;Each of the below questions has two parts.
;First, you will be asked to write a Racket function to solve a problem.
;Secondly, you will be asked to prove by induction that your
;Racket code has some property.


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 1a (10 points)
;Write a recursive function to compute
;the sum( 6*x^2 , x = 1..n) for a given n
;You must write a recursive function.
;If you use any iterative commands (for/loop/sum/etc you will receive a 0)

; Computes sum( 6*x^2 , x = 1..n)
; Input:  n an integer >= 1
; Output: an integer, the result of the summation
;Question 1
(define (spec_sum n)
  (if (= n 1) (* 6 (* 1 1))
      (+ (* 6 (* n n )) (spec_sum (- n 1)))))


;Test Bed
(display "Question 1a spec_sum Tests (5 points)\n")
(define-test-suite test_spec_sum
  (check-equal? (spec_sum 1) 6)
  (check-equal? (spec_sum 2) 30)
  (check-equal? (spec_sum 3) 84)
  (check-equal? (spec_sum 4) 180)
  (check-equal? (spec_sum 5) 330)
  (check-equal? (spec_sum 6) 546)
  (check-equal? (spec_sum 7) 840)
  (check-equal? (spec_sum 8) 1224)
  (check-equal? (spec_sum 9) 1710)
  (check-equal? (spec_sum 10) 2310)
)
(define q1a_score (- 10 (run-tests test_spec_sum 'verbose)))

;Question 1b (10 points)
;Prove by induction that
;For all integers n >= 1 -> (spec_sum n) = 2n^3+3n^2+n

;Give your proof below in comments
;We anchored at x = 1
;LHS
;(spec_sum 1) ; Premise of Base Case LHS
;(if (= 1 1) (* 6 (* 1 1)) (+ (* 6 (* 1 1 )) (spec_sum (- 1 1)))); Def of spec_num
;(if #t (* 6 (* 1 1))....); Eval equal
;(* 6 (* 1 1)); Eval if
;6 ; Math

;RHS
;2(1)^3+3(1)^2+(1);Preise of Base case RHS
;6; Math
;Since LHS = RHS, the base case is established

;Leap Case
;Inductive Hypothesis: Assume that for a value k we have (spec_sum k) = 2k^3+3k^2+k
;LHS
;(spec_sum (+ k 1)); Premise of Leap Case LHS
;(if (= (+ k 1) 1) (* 6 (* 1 1)) (+ (* 6 (* (+ k 1) (+ k 1) )) (spec_sum (- (+ k 1) 1)))); Def of spec_num
;(if #f ...); Eval equal
;(+ (* 6 (* (+ k 1) (+ k 1) )) (spec_sum (- (+ k 1) 1)));Eval if
;(+ (* 6 (* (+ k 1) (+ k 1) )) (spec_sum k)); Math
;(6 * (k+1) * (k+1)) + 2k^3 + 3k^2 + k ; invoke IH
;(6k + 6)(k + 1) +  2k^3 + 3k^2 + k; Math
;6k^2 + 12k + 6 +  2k^3 + 3k^2 + k; Math
;2k^3 + 9k^2 + 13k + 6; Math

;RHS
;2(k+1)^3+3(k+1)^2+(k+1); Premise of Leap Case RHS
;2(k^3+3k^2+3k+1) + 3(k^2 + 2k + 1) + k + 1; Math
;2k^3 + 6k^2 + 6k + 2 + 3k^2 + 6k + 3 + k + 1; Math
;2k^3 + 9k^2 + 13k + 6 ; Math
;Since LHS = RHS, the leap case is established
;Both the base case and leap case have been demonstrated, thus by POMI, we have
;For all integers n >= 1 -> (spec_sum n) = 2n^3+3n^2+n

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 2 (10 points)
; Write a recursive function evenzeros to check if a list of integers
; contains an even number of zeros.
; Don't forget the base case and the necessary recursion. 

; Check if a list contains an even number of zeros
; Input:  L is a list of integers.
; Output: a boolean value which is true when an even number of the elements
;          in L is equal to zero and false otherwise.
; 0 is even, so the Null list should return true
;Question 2
(define (evenzeros X)
  (if (null? X) #t
      (if (equal? (first X) 0)
          (if (evenzeros (rest X)) #f #t)
          (evenzeros (rest X)))))

;Test Bed
(display "Question 2a evenzeros Tests (10 points)\n")
(define-test-suite test_even_zeros
  (check-equal? (evenzeros '()) #t)
  (check-equal? (evenzeros '(1)) #t)
  (check-equal? (evenzeros '(0)) #f)
  (check-equal? (evenzeros '(0 0)) #t)
  (check-equal? (evenzeros '(7 0)) #f)
  (check-equal? (evenzeros '(1 -2)) #t)
  (check-equal? (evenzeros '(0 0 1)) #t)
  (check-equal? (evenzeros '(4 0 1)) #f)
  (check-equal? (evenzeros '(1 0 8)) #f)
  (check-equal? (evenzeros '(0 11 0 -9)) #t)
)
(define q2a_score (- 10 (run-tests test_even_zeros 'verbose)))
;Question 2b (10 points)
;Prove by induction
;For all lists E with an even number of zeros (evenzeros E)=#t
;For all lists O with an odd number of zeros (evenzeros O)=#f
;Hint:
;You will need 1 base case and 4 leap cases!
;The four leap cases are: (cons 0 E), (cons x E), (cons 0 O), (cons x O)
;where x!=0
;All the RHS should be *really* short!

;Base Case
;We anchored at x = null
;LHS
;(evenzeros null);Premise of Base Case LHS
;(if (null? null) #t)...); Def of evenzeros
;(if #t #t..); Eval null
;#t ; Eval if

;RHS
;#t ; Premise of Base Case RHS
;Since LHS = RHS, the base case is established

;Leap Case
;Inductive Hypothesis: Assume that 
;(cons 0 E)= #f where E has an even number of zeros
;(cons x E)= #t where E has an even number of zeros and x!=0
;(cons 0 O)= #t where 0 has an odd number of zeros
;(cons x O)= #f where 0 has an odd number of zeros and x!=0

;Case 1:(cons 0 E)
;LHS
;(evenzeros (cons 0 E)); Premise of Leap Case LHS
;(if (null? (cons 0 E) #t (if ....); Def of evenzeros
;(if #f #t....); Eval null
;(if (equal? (first (cons 0 E)) 0)....); Eval if
;(if (equal? (0 0)....); Eval first/cons
;(if #t....); Eval equal
;(if (evenzeros (rest (cons 0 E))) #f #t)...); Eval if
;(if (evenzeros E) #f #t...); Eval rest/cons
;(if #t #f #t...); Invoke IH
;#f ; Eval if

;RHS
;#f ; Premise of Leap Case RHS
;Since LHS = RHS, the leap case 1 is established

;Case 2: (cons x E)
;(evenzeros (cons x E)); Premise of Leap Case LHS
;(if (null? (cons x E) #t (if ....); Def of evenzeros
;(if #f #t....); Eval null
;(if (equal? (first (cons x E)) 0)....); Eval if
;(if (equal? (x 0)....); Eval first/cons
;(if #f...); Eval equal
;(evenzeros (rest (cons x E)) ; Eval if
;(evenzeros E) ; Eval rest/cons
;#t ; Invoke IH

;RHS
;#t ; Premise of Leap Case RHS
;Since LHS = RHS, the leap case 2 is established

;Case 3: (cons 0 O)
;LHS
;(evenzeros (cons 0 O)); Premise of Leap Case LHS
;(if (null? (cons 0 O) #t (if ....); Def of evenzeros
;(if #f #t....); Eval null
;(if (equal? (first (cons 0 O)) 0)....); Eval if
;(if (equal? (0 0)....); Eval first/cons
;(if #t....); Eval equal
;(if (evenzeros (rest (cons 0 O))) #f #t)...); Eval if
;(if (evenzeros O) #f #t...); Eval rest/cons
;(if #f #f #t...); Invoke IH
;#t ; Eval if

;RHS
;#t ; Premise of Leap Case RHS
;Since LHS = RHS, the leap case 3 is established

;Case 4: (cons x O)
;(evenzeros (cons x O)); Premise of Leap Case LHS
;(if (null? (cons x O) #t (if ....); Def of evenzeros
;(if #f #t....); Eval null
;(if (equal? (first (cons x O)) 0)....); Eval if
;(if (equal? (x 0)....); Eval first/cons
;(if #f...); Eval equal
;(evenzeros (rest (cons x O)) ; Eval if
;(evenzeros O) ; Eval rest/cons
;#f ; Invoke IH

;RHS
;#f ; Premise of Leap Case RHS
;Since LHS = RHS, the leap case 4 is established

;All the base case and leap cases have been demonstrated, thus by POMI, we have
;For all lists E with an even number of zeros (evenzeros E)=#t
;For all lists O with an odd number of zeros (evenzeros O)=#f

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Q3a (10 Points)
;Write a recursive function duplicate that takes every element in a list
;and makes a second copy of the item.
;For example if we started with (1 2 3)
;then the duplicated list would be (1 1 2 2 3 3)

; Duplicates Elements in a list
; Input:  X a list
; Output: A new list with two copies of even value in X
;Question 3
(define (duplicate X)
  (if (null? X)
      '()
      (cons (first X) (cons (first X) (duplicate (rest X)))))) 
;end
(display "Question 3a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '()) '())
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5))
                '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5))
)
(define q3a_score (- 10 (run-tests test_duplicate 'verbose)))

;Q3b (10 Points)
;Supposed x is the number of elements in L
;Prove By Induction
;For all lists we have (length (duplicate L)) = 2x

;You may use the following properties of length
;Length Lemma 1: (length '()) = 0 
;Length Lemma 2: If a is an object and B is a list
;(length (cons a B)) = (+ 1 (length B))
;You may Justify lines by saying [By Length Lemma 1]

;Base Case
;We anchored at L = null
;x=0 because there is no element in the empty list
;LHS
;(length (duplicate null)); Premise of Base Case LHS
;(length (if (null? null) '()...); Def of duplicate
;(length (if #t '()...); Eval null
;(length '()); Eval if
;0 ; By Length Lemma 1

;RHS
;2*(0); Premise of Base Case RHS
;0;Math
;Since LHS = RHS, the base case is established

;Leap Case
;Inductive Hypothesis: Assume that k is the number of elements in L, we have (length (duplicate L)) = 2k

;LHS
;(length (duplicate (cons a L) ; Premise of Leap Case LHS
;(length (if (null? (cons a L) '() (cons...); Def of duplicate
;(length (if #f '()...); Eval null
;(length ((cons (first (cons a L)) (cons (first (cons a L)) (duplicate (rest (cons a L))))))); Eval if
;(length ( cons a (cons a (duplicate (rest(cons a L))))); Eval first/cons
;(length (cons a (cons a (duplicate L))); Eval rest/cons
;(1+(length (cons a (duplicate L)))) By Length Lemma 2
;(1 + 1+(length (duplicate L))); By Length Lemma 2
;1 + 1 + 2k ; invoke IH
;2k + 2; Math

;RHS
;2*(k+1) ; Premise of Leap Case RHS
; 2k + 2; Math
;Since LHS = RHS, the leap case  is established

;Both the base case and leap case have been demonstrated, thus by POMI, we have
;For all lists we have (length (duplicate L)) = 2x

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 4a (10pts)
;Write a recursive function (cut_end L) that removes the last element from the list

; Removes the last element in a list
; Input:  X non-empty a list
; Output: A new list with the last element removed
;Question 4
(define (cut_end L)
  (if (null? (rest L)) '()
      (cons (first L) (cut_end (rest L)))))

(display "Question 4a cut_end Tests (10 points)\n")
(define-test-suite test_cut_end
  (check-equal? (cut_end '(1)) '())
  (check-equal? (cut_end '(1 2)) '(1))
  (check-equal? (cut_end '(3 4 5)) '(3 4))
  (check-equal? (cut_end '( (1) (2) (3) )) '( (1) (2) ))
  (check-equal? (cut_end '((1 2 3 4))) '())
  (check-equal? (cut_end '((1 2) (3 4))) '((1 2)))
  (check-equal? (cut_end '(9 9 8)) '(9 9))
  (check-equal? (cut_end '(AND A B)) '(AND A))
  (check-equal? (cut_end '(NOT X)) '(NOT))
)
(define q4a_score (- 10 (run-tests test_cut_end 'verbose)))

;Question 4b
;Supposed x represents the number of elements in L
;Prove by Induction that
;For all lists with length >= 1 we have (length (cut_end L)) = x-1
;You may use the properties of length from Question 3

;Base Case
;We anchored at length L = 1

;LHS
;(length (cut_end L)) ; Premise of Base Case LHS
;(length (if (null? (rest L)) '()...)); Def of cut_end
;(length (if #t '()...)); Eval null
;(length '()); Eval if
;0; By Length Lemma 1

;RHS
;1-1 ; Premise of Base Case RHS
;0 ; Math
;Since LHS = RHS, the base case is established

;Leap Case
;Inductive Hypothesis: Assume that for any list L with (length L)= k (where k >= 1), we have (length (cut_end L)) = k-1

;LHS
;(length (cut_end cons a L)); Premise of Leap Case LHS
;(length (if (null? (rest (cons a L))) '() ...); Def of cut_end
;(length (if (null? L) '() (cons...);Eval rest/cons
;(length (if #f ....); Eval null
;(length (cons (first (cons a L)) (cut_end (rest (cons a L))))); Eval if
;(length (cons a (cut_end ...); Eval first/cons
;(length (cons a (cut_end L))); Eval rest/cons
;(+ 1 (length (cut_end L))); By Length Lemma 2
;1 + k - 1; Invoke IH
;k ; Math

;RHS
;k + 1 - 1; Premise of Leap Case RHS
;k ; Math
;Since LHS = RHS, the leap case  is established

;Both the base case and leap case have been demonstrated, thus by POMI, we have
;For all lists with length >= 1 we have (length (cut_end L)) = x-1


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 5a (10pts)
;Write a recursive function (add_pairs L)
;that adds pairs of numbers.
;You may assume the length of L will always be even.

; Adds pairs of numbers
; Input:  L a list (the list must have even length)
; Output: A new list with pairs of elements added together.
;Question 5
(define (add_pairs L)
  (if (null? L) '()
      (cons (+ (first L)(first (rest L)))
            (add_pairs (rest (rest L))))))

(display "Question 5a add_pairs Tests (10 points)\n")
(define-test-suite test_add_pairs
  (check-equal? (add_pairs '()) '())
  (check-equal? (add_pairs '(1 2)) '(3))
  (check-equal? (add_pairs '(1 2 3 4)) '(3 7))
  (check-equal? (add_pairs '(2 2 2 2)) '(4 4))
  (check-equal? (add_pairs '(0 -1 -2 3)) '(-1 1))
  (check-equal? (add_pairs '(1 1 1 1)) '(2 2))
  (check-equal? (add_pairs '(1 2 3 4 5 6 7 8)) '(3 7 11 15))
  (check-equal? (add_pairs '(9 9 9 9 9 9)) '(18 18 18))
  (check-equal? (add_pairs '(7 3 4 6 5 5)) '(10 10 10))
  (check-equal? (add_pairs '(-9 9 -8 8)) '(0 0))
  
)
(define q5a_score (- 10 (run-tests test_add_pairs 'verbose)))

;Question 5b
;Suppose n represents the number of elements in L
;Prove by induction that
;for all lists of even length it is true that (length (add_pairs L)) = n/2
;You may use the properties of length from Question 3

;Base Case
;We anchored at length L = null
;So n = 0 since L is empty list

;LHS
;(length (add_pairs null)) ; Premise of Base Case LHS
;(length (if (null? L) '()...); Def of add_paris
;(length (if #t...); Eval null
;(length '()); Eval if
;0 ; By Length Lemma 1

;RHS
;0/2 ; Premise of Base Case RHS
;0; Math
;Since LHS = RHS, the base case is established

;Leap Case
;Inductive Hypothesis: Assume that for any list L with (length L)= k (where k is an even number),
; we have (length (add_pairs L)) = k/2

;LHS
;(length (add_pairs (cons a (cons b L)))); Premise of Leap Case LHS
;(length (if (null? (cons a (cons b L))) '() (cons (+ (first (cons a (cons b L)))...);Def of add_pairs
;(length (if #f ...)); Eval null
;(length (cons (+ (first (cons a (cons b L)))(first (rest (cons a (cons b L))))) (add_pairs (rest (rest (cons a (cons b L))))))); Eval if
;(length (cons (+ a  (first (rest (cons a (cons b L)....); Eval first/cons
;(length (cons (+ a (first (cons b L...) ; Eval rest
;(length (cons (+ a b)(add_pairs (rest (rest (cons a (cons b L))))))); Eval first/cons
;(length (cons (+ a b) (add_pairs (rest (cons b L))))); Eval rest
;(length (cons (+ a b) (add_pairs L))); Eval rest/cons
;(+ 1 (length (add_pairs L))); By Length Lemma 2
;1 + k/2 ; Invoke IH
;(2 + k)/2 ; Math

;RHS
;(k + 2)/2 ; Premise of Leap Case RHS
;(2 + k)/2 ; Math
;Since LHS = RHS, the leap case is established

;Both the base case and leap case have been demonstrated, thus by POMI, we have
;For all lists of even length it is true that (length (add_pairs L)) = n/2

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")


(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")