(define (map-stream f s)
    (if (null? s)
      nil
      (cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define multiples-of-three
  (cons-stream 3 (map-stream (lambda (x) (+ 3 x)) multiples-of-three)))

(define (rle s)
  (define (insert-or-incr v xs)
    (cond
      ((null? xs) (cons-stream `(,v 1) nil))
      ((equal? v (car (car xs)))
        (cons-stream
          `(,(car (car xs)) ,(+ 1 (car (cdr (car xs)))))
          (cdr-stream xs)
        ))
      (#t (cons-stream (car xs) (insert-or-incr v (cdr-stream xs))))))
  (define (helper s acc)
    (if (null? s)
      acc
      (helper (cdr-stream s) (insert-or-incr (car s) acc))))
  (helper s nil))
