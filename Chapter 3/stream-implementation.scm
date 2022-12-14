#lang sicp
;(delay <exp>) is a special form
;equivalent to (lambda () <exp>)
;This can possibly be memoized also

;(cons-stream <a> <b>) is a special form
;equivalent to (cons <a> (delay <b>))

;(define (force delayed-object)
  ;(delayed-object))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (apply-map procstream . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       ((stream-car procstream) . (map stream-car argstreams))
       (apply-map (cdr procstream) . (cdr argstreams)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define (print-n s n)
  (if (> n 0)
      (begin (display (stream-car s))
             (display ",")
             (print-n (stream-cdr s) (- n 1)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (constant-stream const)
  (cons-stream const (constant-stream const)))

(define (partial-sums s)
  (add-streams (constant-stream (stream-car s))
              (cons-stream 0
                           (partial-sums (stream-cdr s)))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 ))))


(define (integrate-series s)
  (mul-streams (stream-map (lambda (n) (/ 1 n)) integers) s))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream
                  (mul-series (stream-cdr s) (invert-unit-series s))
                  -1)))

(define (div-series num denom)
  (let ((start (stream-car denom)))
    (if (= start 0)
        (error "Zero series division -- DIV-SERIES")
        (scale-stream
         (mul-series num
                     (invert-unit-series
                      (scale-stream denom (/ 1 start))))
         (/ 1 start)))))

(define (stream-limit stream tol)
  (let ((first (stream-car stream))
        (second (stream-car (stream-cdr stream))))
    (if (< (abs (- first second)) tol)
        second
        (stream-limit (cdr stream tol)))))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s)
           (stream-cdr t) weight)
   weight)))

(define (ordered-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
    (ordered-pairs (stream-cdr s) t))))

(define (triples s t u)
  (define (triple-interleave s1 s2 s3)
    (interleave (interleave s1 s2) s3))
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (triple-interleave
    (stream-map (lambda (p) (list (stream-car s) (car p) (cadr p)))
                (pairs (stream-cdr t) (stream-cdr u)))
    (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                (stream-cdr u))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
    

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay (dy)) y0 dt))
  (define (dy) (stream-map f y))
    y)

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand-init 10)

(define (random-generator command-stream)
    (define (helper start commands)
      (define (extract m current-val)
        (if (pair? m) (car m)
          (rand-update current-val)))
      (cons-stream start
                   (helper (extract (stream-car commands) start)
                           (stream-cdr commands))))
  (helper rand-init command-stream))
                   

