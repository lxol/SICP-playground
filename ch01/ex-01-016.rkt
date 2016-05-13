#|

*Exercise 1.16:*

Design a procedure that evolves an iterative exponentiation process
that uses successive squaring and uses a logarithmic number of steps,
as does `fast-expt'. (Hint: Using the observation that (b^(n/2))^2
= (b^2)^(n/2), keep, along with the exponent n and the base b, an
additional state variable a, and define the state transformation in
such a way that the product a b^n is unchanged from state to state. At
the beginning of the process a is taken to be 1, and the answer is
given by the value of a at the end of the process. In general, the
technique of defining an "invariant quantity" that remains unchanged
from state to state is a powerful way to think about the design of
iterative algorithms.)

|#

(require rackunit)

(define (fast-exp b n )
  (define (aux acc b1 n1)
    (cond ((= n1 0) 1)
          ((= n1 1) (* b1 acc))
          ((even? n1) (aux  acc (* b1 b1) (/ n1 2) ))
          (else (* b1  (aux acc b1 (- n1 1))))
          ))
  (aux 1 b n))

(check-equal? (fast-exp 10 0) 1)
(check-equal? (fast-exp 10 1) 10)
(check-equal? (fast-exp 10 2) 100)
(check-equal? (fast-exp 10 3) 1000)
(check-equal? (fast-exp 10 4) 10000)
(check-equal? (fast-exp 10 5) 100000)
