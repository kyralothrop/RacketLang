#lang racket
; Kyra Lothrop
; 101145872

; Question 1
(define (count-multiples numbers num)
  (cond
    [(null? numbers) 0]
    [(= (modulo (car numbers) num ) 0) (+ 1 (count-multiples (cdr numbers) num))]
    [else (count-multiples (cdr numbers) num)]
  )
 )

(count-multiples '(1 2 3 4 5 6) 1) ; returns 6
(count-multiples '(1 2 3 4 5 6) 2) ; returns 3
(count-multiples '(1 2 3 4 5 6) 3) ; returns 2
(count-multiples '(1 2 3 4 5 6) 7) ; returns 0


; Question 2
(define (count-multiples-iter numbers num)
  (define (count-multiples-iter-additional numbers num result)
    (cond
      [(null? numbers) result]
      [(= (modulo (car numbers) num ) 0) (count-multiples-iter-additional (cdr numbers) num (+ 1 result))]
      [else (count-multiples-iter-additional (cdr numbers) num result)]
    )
   )
  (count-multiples-iter-additional numbers num 0)
 )


(count-multiples-iter '(1 2 3 4 5 6) 1) ; returns 6
(count-multiples-iter '(1 2 3 4 5 6) 2) ; returns 3
(count-multiples-iter '(1 2 3 4 5 6) 3) ; returns 2
(count-multiples-iter '(1 2 3 4 5 6) 7) ; returns 0

; Question 3
(define (deep-list-remove criteria list)
  (cond
    [(empty? list) empty]
    [(list? (car list)) (cons (deep-list-remove criteria (car list)) (deep-list-remove criteria (cdr list)))]
    [(not (criteria (car list))) (cons (car list) (deep-list-remove criteria (cdr list)))]
    [ else (deep-list-remove criteria (cdr list))]
   )
)

(deep-list-remove (lambda (x) (= x 0)) (list 0 1 2 3))  ; returns (1 2 3)
(deep-list-remove (lambda (x) (< x 4)) '(7 2 (3 4 (5 6))))   ; returns '(7 ( 4 (5 6)))