(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . objs) objs)
(define (id obj) obj)

(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda arg (apply func (cons arg1 arg))))
(define (compose f g) (lambda (arg) (f (g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr func acc lst)
  (if (null? lst)
      acc
      (func (car lst) (foldr func acc (cdr lst)))))

(define (foldl func acc lst)
  (if (null? lst)
      acc
      (foldl func (func acc (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

;iterate func on init until (pred init) evaluates to true
(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (max lst) (fold (lambda (old new) (if (> old new) old new)) (car lst) (cdr lst)))
(define (min lst) (fold (lambda (old new) (if (< old new) old new)) (car lst) (cdr lst)))

(define (length lst) (fold (lambda (acc cur) (+ acc 1)) 0 lst))
(define (reverse lst) (fold (flip cons) '() lst))

(define (map func lst) (foldr (lambda (cur acc) (cons (func cur) acc)) '() lst))
(define (filter pred lst) (foldr (lambda (cur acc) (if (pred cur) (cons cur acc) acc)) '() lst))

(define (memq obj lst)
    (if (null? lst) 
        #f
        (if (eq? obj (car lst))
        lst
        (memq obj (cdr lst)))))

(define (memv obj lst)
    (if (null? lst)
        #f
        (if (eqv? obj (car lst))
            lst
            (memv obj (cdr lst)))))

(define (member obj lst)
    (if (null? lst)
        #f
        (if (equal? obj (car lst))
            lst
            (member obj (cdr lst)))))

(define (assq obj lst)
    (if (null? lst)
        #f
        (if (eq? obj (car (car lst)))
            (car lst)
            (assq obj (cdr lst)))))

(define (assv obj lst)
    (if (null? lst)
        #f
        (if (eqv? obj (car (car lst)))
            (car lst)
            (assq obj (cdr lst)))))

(define (assoc obj lst)
    (if (null? lst)
        #f
        (if (eqv? obj (car (car lst)))
            (car lst)
            (assoc obj (cdr lst)))))
