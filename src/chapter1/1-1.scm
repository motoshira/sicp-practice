;; 1-1

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess)
             x))
     0.01))

(define (improve guess x)
  (/ (+ guess (/ x guess))
     2))
