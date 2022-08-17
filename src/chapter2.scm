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
