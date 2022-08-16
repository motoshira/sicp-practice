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
  (and (> x 1)
       (= (smallest-divisor x) x)))

;; 1-22

(define (report-time elapsed-time)
  (display "*** ")
  (display elapsed-time)
  (newline))

(define (start-prime-test n start-time)
  (begin
    (prime? n)
    (report-time (/ (exact->inexact
                     (- (get-internal-real-time)
                        start-time))
                    internal-time-units-per-second))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (get-internal-real-time)))

;; 1-28

(define (expmod base exp mod)
  (cond
   ((zero? exp) 1)
   ((even? exp) (remainder (square (expmod base (/ exp 2) mod))
                           mod))
   (else (remainder (* base (expmod base (- exp 1) mod))
                    mod))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (1+ (random (- n 2)))))

(define (fast-prime? n times)
  (cond
   ((zero? times) #t)
   ((fermat-test n) (fast-prime? n (- times 1)))
   (else #f)))

(define (expmod-revised base exp mod)
  (define (disp e)
    (cond
     ((zero? e) 1)
     ((even? e)
      (let* ((half (disp (/ e 2)))
             (squared (remainder (square half) mod)))
        ;; ここでresが1なら自明でない平方根が見つかったということなので
        ;; シグナルとして0を返しておく
        (if (and (not (= half 1))
                 (not (= half (- mod 1)))
                 (= squared 1))
            0
            squared)))
     (else (remainder (* base (disp (- e 1)))
                      mod))))
  (disp exp))

(define (miller-rabin-test n times)
  (if (zero? times)
      #t
      (let ((a (+ (random (- n 2))
                  1)))
        (if (= (expmod-revised a (- n 1) n)
               1)
            (miller-rabin-test n (- times 1))
            #f))))

;; 1-29

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (simpson-integral f a b n)
  (unless (even? n)
    (error "N must be even"))
  (let ((h (/ (- b a) n)))
    (define (g k)
      (* (f (+ a (* k h)))
         (if (odd? k)
             4
             2)))
    (exact->inexact
     (* (+ (sum g 1 inc n)
           (g 0)
           (g n))
        (/ h 3)))))

;; 1-30

(define (sum-iter term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a)
              (+ acc
                 (term a)))))
  (iter a 0))

; for test sum-iter

(define (integral-iter f a b dx)
  (* (sum-iter f
               (+ a (/ dx 2.0))
               (lambda (x) (+ x dx))
               b)
     dx))

;; 1-31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a)
              (* acc (term a)))))
  (iter a 1))

(define (factorial x)
  (product identity 1 inc x))

(define (guess-pi n)
  (* (product-iter
      (lambda (x)
        (exact->inexact
         (/ (* x
               (+ x 2))
            (* (+ x 1)
               (+ x 1)))))
      2
      (lambda (x) (+ x 2))
      (* (+ n 1) 2))
     4))

;; 1-32

;; a

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a)
              (combiner acc
                        (term a)))))
  (iter a null-value))

;; b

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; 1-33

(define (filtered-accumulate combiner filter-fn null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a)
              (combiner acc
                        (if (filter-fn a)
                            (term a)
                            null-value)))))
  (iter a null-value))

(define (sum-squared-prime a b)
  (filtered-accumulate + prime? 0 square a inc b))

(define (product-of-coprime n)
  (define (coprime? x)
    (= (gcd n x) 1))
  (filtered-accumulate * coprime? 1 identity 1 inc (- n 1)))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.000001)

(define (println x)
  (display x)
  (newline))

(define (fixed-point f first-guess)
  (define (close-enough? x)
    (< (abs (- x (f x)))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      ;; (println next)
      (if (close-enough? next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (y)
    (average y (f y))))

(define (my-sqrt x)
  (fixed-point (average-damp
                (lambda (y) (/ x y)))
               1.0))

(define golden-ratio
  (fixed-point (lambda (x)
                 (average x (+ 1.0 (/ 1.0 x))))
               1.0))

;; 1-36

(define (f x)
  (expt x x))

(define guessed-answer
  (let ((x 1.1))
    (fixed-point
     (lambda (y)
       (/ (+ y
             (/ (log 1000)
                (log y)))
          2))
     x)))

;; 1-37

(define (cont-frac n d k)
  (define (sub i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (sub (inc i))))))
  (sub 1))

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (if (zero? i)
        acc
        (iter (dec i)
              (/ (n i)
                 (+ (d i)
                    acc)))))
  (iter k 0))

;; 1-38

(define (guess-exp k)
  (define (d i)
    (if (or (= i 1)
            (positive? (remainder (inc i) 3)))
        1
        (* (ceiling (/ i 3)) 2)))
  (+ (cont-frac-iter
      (const 1.0)
      d
      k)
     2.0))

;; 1-39

(define (tan-cf x k)
  (define (sub i acc)
    (if (zero? i)
        acc
        (sub (dec i)
             (/ (expt x i)
                (- (dec (* i 2))
                   acc)))))
  (sub k 0.0))

;; 1-43

(define (deriv g dx)
  (lambda (x)
    (/ (- (g (+ x dx))
          (g x))
       dx)))

(define (newton-transform g dx)
  (lambda (x)
    (- x
       (/ (g x)
          ((deriv g dx) x)))))

(define (newtons-method g guess dx)
  (fixed-point (newton-transform g dx) guess))

(define (sqrt-revised x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0
   0.000000001))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; 1-41

(define (double f)
  (lambda (x)
    (f (f x))))

;; 1-42

(define (compose f g)
  (lambda (x)
    (f (g x))))

;; 1-43

(define (repeated f n)
  (define (build-fn i acc)
    ;; (println (list i acc))
    (if (zero? i)
        acc
        (build-fn (dec i)
                  (compose f acc))))
  (build-fn n identity))

;; 1-44

(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-n-times f n dx)
  (define (build i)
    (if (zero? i)
        f
        (smooth (build (dec i))
                dx)))
  (build n))

;; 1-45

(define (get-timer)
  (let ((start (get-internal-real-time)))
    (lambda ()
      (/ (- (get-internal-real-time)
            start)
         internal-time-units-per-second))))

(define (test-fixed-point x guess timeout)
  (define (close-enough? x)
    (< (abs (- x (f x)))
       tolerance))
  (define (try guess timer)
    (let ((next (f guess)))
      ;; (println next)
      (if (close-enough? next)
          next
          (if (< (timer) timeout)
              (try next timer)
              #f))))
  (try guess (get-timer)))

(define (log-of-two x)
  (/ (log x)
     (log 2)))

(define (n-root x n)
  (fixed-point
   (repeated (average-damp
              (lambda (y)
                (/ x (expt y (dec n)))))
             (round (log-of-two n)))
   1.0))
