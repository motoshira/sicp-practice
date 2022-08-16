;; 1-1

;; 1-4

(define (square x)
  (* x x))

(define (get-sum-of-square-of-two-big-mums x y z)
  (if (> x z)
      (if (> z y)
          (+ (square x)
             (square z))
          (+ (square x)
             (square y)))
      (if (> x y)
          (+ (square x)
             (square z))
          (+ (square y)
             (square z)))))

;; 1-8 calc cube-root

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess)
             x))
     0.01))

(define (improve guess x)
  (/ (+ (* guess 2)
        (/ x guess guess))
     3))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x)
                 x)))

;; 1-10

(define (ackermann x y)
  (cond
   ((zero? y) 0)
   ((zero? x) (* y 2))
   ((= y 1) 2)
   (else (ackermann (- x 1)
                    (ackermann x (- y 1))))))

(define (divide-count x)
  (if (zero? (remainder x 2))
      (+ (divide-count (quotient x 2))
         1)
      0))

;; 1-11

(define (f n)
  (cond
   ((< n 3) n)
   (else
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3)))))))

(define (f-iter-sub count a b c)
  (if (zero? count)
      a
      (f-iter-sub (- count 1)
                  (+ a
                     (* 2 b)
                     (* 3 c))
                  a
                  b)))

(define (f-iter n)
  (cond
   ((< n 3) n)
   (else
    (f-iter-sub (- n 2)
                2
                1
                0))))

;; 1-12

(define (pascal n k)
  (cond
   ((or (zero? k)
        (= n k))
    1)
   (else
    (+ (pascal (- n 1)
               k)
       (pascal (- n 1)
               (- k 1))))))

;; 1-13

(define (fast-expt x power)
  (cond
   ((zero? power) 1)
   ((even? power)
    (* (fast-expt x (/ power 2))
       (fast-expt x (/ power 2))))
   (else
    (* x (fast-expt x (- power 1))))))

(define (fast-expt-sub x pow acc)
  (cond
   ((zero? pow) acc)
   ((even? pow)
    (fast-expt-sub (* x x)
                   (/ pow 2)
                   acc))
   (else
    (fast-expt-sub x
                   (- pow 1)
                   (* acc x)))))

(define (fast-expt-iter x power)
  (fast-expt-sub x power 1))

;; 1-21

(define (find-smallest-divider x cand)
  (cond
   ((> cand (sqrt x)) x)
   ((zero? (remainder x cand)) cand)
   (else (find-smallest-divider x (1+ cand)))))

(define (smallest-divisor x)
  (find-smallest-divider x 2))

(define (prime? x)
  (= (smallest-divisor x) x))
