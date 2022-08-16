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
