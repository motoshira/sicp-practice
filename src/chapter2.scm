;; 2-1

(define (make-rat n d)
  (let* ((g (gcd n d))
         (sig (/ (* n d) (abs (* n d))))
         (n (* (abs (/ n g)) sig))
         (d (abs (/ d g))))
    (cons n d)))

;; 2-2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint-segment p1 p2)
  (let ((mx (/ (+ (x-point p1) (x-point p2)) 2))
        (my (/ (+ (y-point p1) (y-point p2)) 2)))
    (make-point mx my)))

;; 2-3

;; 2つの線分で定義する

(define (make-rect s1 s2)
  (list s1 s2))

(define (rect-s1 rect)
  (car rect))

(define (rect-s2 rect)
  (cadr rect))

(define (segment-length segment)
  (let* ((start (start-segment segment))
         (end (end-segment segment))
         (dx (- (x-point start)
                (x-point end)))
         (dy (- (y-point start)
                (y-point end))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (rect-radius rect)
  (* (+ (segment-length (rect-s1 rect))
        (segment-length (rect-s2 rect)))
     2))

(define (rect-area rect)
  (* (segment-length (rect-s1 rect))
     (segment-length (rect-s2 rect))))

;; 中点と

(define (make-rectang p0 p1 p2 p3)
  (list p0 p1 p2))

(define (rectang-p0 rectang)
  (car rectang))

(define (rectang-p1 rectang)
  (cadr rectang))

(define (rectang-p2 rectang)
  (caddr rectang))

(define (rectang-radius rectang)
  (* (+ (segment-length (make-segment (rectang-p0 rectang)
                                      (rectang-p1 rectang)))
        (segment-length (make-segment (rectang-p0 rectang)
                                      (rectang-p2 rectang))))
     2))

(define (rectang-area rectang)
  (* (segment-length (make-segment (rectang-p0 rectang)
                                   (rectang-p1 rectang)))
     (segment-length (make-segment (rectang-p0 rectang)
                                   (rectang-p2 rectang)))))

;; 2-4

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car m)
  (m (lambda (x y) x)))

(define (my-cdr m)
  (m (lambda (x y) y)))

;; 2-5

(define (println x)
  (display x)
  (newline))

(define (num-cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (num-car n)
  (define (rec n acc)
    (if (zero? (remainder n 2))
        (rec (quotient n 2) (+ acc 1))
        acc))
  (rec n 0))

(define (num-cdr n)
  (define (rec n acc)
    (if (zero? (remainder n 3))
        (rec (quotient n 3) (+ acc 1))
        acc))
  (rec n 0))

;; 2-6

(define (to-int f)
  ((f identity) 0))

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-one n)
  (lambda (g)
    (lambda (x)
      (g ((n g) x)))))

;; 2-7

(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; 2-8

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (+ (upper-bound x)
                    (lower-bound y))))

;; 2-10

(define (contain-zero? interval)
  (<= (lower-bound interval)
      0
      (upper-bound interval)))

(define (inverse-intarval interval)
  (when (contain-zero? interval)
    (error "This interval contains zero:" interval))
  (let ((p1 (/ 1.0 (lower-bound interval)))
        (p2 (/ 1.0 (upper-bound interval))))
    (make-interval (min p1 p2)
                   (max p1 p2))))

(define (div-interval x y)
  (mul-interval x (inverse-intarval y)))

;; 2-11

(define (mul-interval-revised x y)
  (cond
   ((positive? (lower-bound x))
    (cond
     ((positive? (lower-bound y))
      (make-interval (* (lower-bound x)
                        (lower-bound y))
                     (* (upper-bound x)
                        (upper-bound y))))
     ((contain-zero? y)
      (make-interval (* (upper-bound x)
                        (lower-bound y))
                     (* (upper-bound x)
                        (upper-bound y))))
     (else
      (make-interval (* (upper-bound x)
                        (lower-bound y))
                     (* (lower-bound x)
                        (upper-bound y))))))
   ((contain-zero? x)
    (cond
     ((positive? (lower-bound y))
      (make-interval (* (lower-bound x)
                        (upper-bound y))
                     (* (upper-bound x)
                        (upper-bound y))))
     ((contain-zero? y)
      (make-interval x y))
     (else
      (make-interval (* (upper-bound x)
                        (lower-bound y))
                     (* (lower-bound x)
                        (lower-bound y))))))
   (else
    (cond
     ((positive? (lower-bound y))
      (make-interval (* (upper-bound x)
                        (upper-bound y))
                     (* (lower-bound x)
                        (upper-bound y))))
     ((contain-zero? y)
      (make-interval (* (lower-bound x)
                        (upper-bound y))
                     (* (upper-bound x)
                        (upper-bound y))))
     (else
      (make-interval (* (upper-bound x)
                        (upper-bound y))
                     (* (lower-bound x)
                        (lower-bound y))))))))

;; 2-12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i))
     2))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-percent c w)))

(define (percent i)
  (/ (width i) (center i)))

;; 2-17

(define (last-pair xs)
  (if (null? (cdr xs))
      (car xs)
      (last-pair (cdr xs))))

;; 2-18

(define (my-reverse xs)
  (define (rec xs acc)
    (if (null? xs)
        acc
        (rec (cdr xs)
             (cons (car xs) acc))))
  (rec xs '()))

;; 2-19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomiation coins)
  (car coins))

(define (except-first-denomiations coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

(define (cc amount coin-values)
  (cond
   ((= amount 0) 1)
   ((or (negative? amount)
        (no-more? coin-values))
    0)
   (else
    (+ (cc amount
           (except-first-denomiations coin-values))
       (cc (- amount (first-denomiation coin-values))
           coin-values)))))

;; 2-20

(define (same-parity x . xs)
  (cons x
        (if (or (null? xs)
                (null? (cdr xs)))
            '()
            (apply same-parity (cdr xs)))))

;; 2-21

(define (square x) (* x x))

(define (square-list xs)
  (if (null? xs)
      '()
      (cons (square (car xs))
            (square-list (cdr xs)))))

(define (square-list-revised xs)
  (map square xs))

;; 2-23

(define (my-for-each f xs)
  (unless (null? xs)
    (f (car xs))
    (my-for-each f (cdr xs))))
