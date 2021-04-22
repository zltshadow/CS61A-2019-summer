;;;;;;;;;;;;;;;
;; Questions ;;
;;;;;;;;;;;;;;;

; Scheme

(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s)))

(define (caddr s)
  (car (cdr (cdr s))))

;; Haskell: nub
(define (unique s)
  (if (null? s)
    nil
    (cons
      (car s)
      (unique
        (filter
          (lambda (x) (not (eq? x (car s))))
          (cdr s))))))

;; Haskell: cons-all first rests = (first :) <$> rests
(define (cons-all first rests)
  (map (lambda (s) (cons first s)) rests))

;; List all ways to make change for TOTAL with DENOMS
;;
;; listChange :: Int -> [Int] -> [[Int]]
;; listChange m xs = helper m xs
;;   where
;;   helper :: Int -> [Int] -> [[Int]]
;;   helper _ [] = []
;;   helper 0 _ = [[]]
;;   helper n (y : ys) =
;;     if n < y
;;       then helper n ys
;;       else fmap (y :) (helper (n - y) xs) <> listChange n ys
;;
;; main :: IO ()
;; main = do
;;   print $ listChange 10 [25, 10, 5, 1]
;;   print $ listChange 5 [4, 3, 2, 1]
(define (list-change total denoms)
  (define (helper n xs)
    (if (null? xs)
      nil
      (if (= 0 n)
        '(nil)
        (if (< n (car xs))
          (helper n (cdr xs))
          (append
            (cons-all (car xs) (helper (- n (car xs)) denoms))
            (list-change n (cdr xs)))))))
  (helper total denoms))

; Tail recursion

(define (replicate x n)
  (define (helper n acc)
    (if (= n 0)
      acc
      (helper (- n 1) (cons x acc))))
  (helper n nil))

(define (accumulate combiner start n term)
  (define (range n)
    (define (helper x)
      (if (> x n)
        nil
        (cons x (helper (+ 1 x)))))
    (helper 1))
  (define (reduce-default combiner lst default)
    (reduce combiner (cons default lst)))
  (reduce-default combiner (map term (range n)) start))


(define (accumulate-tail combiner start n term)
  (define (helper x acc)
    (if (> x n)
      acc
      (helper (+ 1 x) (combiner acc (term x)))))
  (helper 1 start))


; Macros

(define-macro (list-of map-expr for var in lst if filter-expr)
  `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst)))
