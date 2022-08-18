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

;; 2-8

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (+ (upper-bound x)
                    (lower-bound y))))
