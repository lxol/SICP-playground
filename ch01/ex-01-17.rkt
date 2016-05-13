#|

*Exercise 1.17:*

The exponentiation algorithms in this section are based on performing
exponentiation by means of repeated multiplication. In a similar way,
one can perform integer multiplication by means of repeated addition.
The following multiplication procedure (in which it is assumed that
our language can only add, not multiply) is analogous to the `expt'
procedure:

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

This algorithm takes a number of steps that is linear in `b'. Now
suppose we include, together with addition, operations `double', which
doubles an integer, and `halve', which divides an (even) integer by 2.
Using these, design a multiplication procedure analogous to
`fast-expt' that uses a logarithmic number of steps.

|#

(require rackunit)

(define (double v)
  (* 2 v))

(define (halve v)
  (/ v 2))

(define (aaa v~) 0)

(define (mult/iter a b)
  (define (aux acc a# b#)
    (cond ((= b# 1) (+ acc a#))
          ((even? b#) (aux  acc (double a#) (halve b#)))
          (else (aux (+ acc a#) a# (- b# 1)))))
  (aux 0 a b))

(check-equal? (double 3) 6)
(check-equal? (halve 6) 3)
(check-equal? (mult/iter  1 1) 1)
(check-equal? (mult/iter  1 2) 2)
(check-equal? (mult/iter  2 3) 6)
(check-equal? (mult/iter  2 4) 8)

(define (mult/rec a b)
  (cond ((= b 1) a)
        ((even? b) (mult/rec (double a) (halve b)) )
        (else (+ a (mult/rec a (- b 1))))))
(check-equal? (mult/rec  1 1) 1)
(check-equal? (mult/rec  1 2) 2)
(check-equal? (mult/rec  2 3) 6)
(check-equal? (mult/rec  2 4) 8)
