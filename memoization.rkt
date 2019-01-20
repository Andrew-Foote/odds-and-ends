#lang racket

(define (my-map f xs)
  (lambda (i)
    (f (xs i))))

(define (my-sequence i)
  (match i
    (0 1)
    (1 4)
    (2 9)))

(define (display-and-square x)
  (displayln x)
  (* x x))

(displayln "squared-list:")

; displays 1, 4 and 9
(define squared-list (map display-and-square (list 1 4 9)))

(displayln "my-sequence-squared created:")
  
; nothing displayed
(define my-sequence-squared (my-map display-and-square my-sequence))

(displayln "my-sequence-squared accessed:")

(displayln (my-sequence-squared 0)) ; displays 1 and 1
(displayln (my-sequence-squared 1)) ; displays 4 and 16
(displayln (my-sequence-squared 2)) ; displays 9 and 81

(define (memoize mapping keys)
  (define cases (map (lambda (key)
     (list key (mapping key))) keys))
  
  (lambda (key)
    (eval (append
      (list 'match key)
      cases
      (list (list '_ (list mapping key)))))))

(displayln "my-sequence-squared-2 created:")

; displays nothing
(define my-sequence-squared-2 (my-map display-and-square my-sequence))

(displayln "my-sequence-squared-3 created:")

; displays 1 and 4
(define my-sequence-squared-3 (memoize my-sequence-squared-2 (list 0 1)))

(displayln "my-sequence-squared-3 accessed:")

(my-sequence-squared-3 1) ; displays 16
(my-sequence-squared-3 2) ; displays 9 and 81
