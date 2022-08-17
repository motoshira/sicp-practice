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
