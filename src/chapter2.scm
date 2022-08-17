;; 2-1

(define (make-rat n d)
  (let* ((g (gcd n d))
         (sig (/ (* n d) (abs (* n d))))
         (n (* (abs (/ n g)) sig))
         (d (abs (/ d g))))
    (cons n d)))
