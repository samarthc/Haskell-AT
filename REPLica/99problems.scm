;P01: Find the last box of a list
(define (last lst)
    (if (null? lst)
        lst
        (if (null? (cdr lst))
            (car lst)
            (last (cdr lst)))))

;P02: Find the last-but-one box of a list
(define (init lst)
    (if (null? lst)
        lst
        (if (null? (cdr lst))
            '()
            (cons (car lst) (init (cdr lst))))))

(define (second-last lst)
    (if (null? lst)
        lst
        (if (null? (cdr lst))
            lst
            (last (init lst)))))

;P03: Find the kth element of a list (first is k=0)
(define (elem-at lst index)
    (if (< index 1)
        (car lst)
        (elem-at (cdr lst) (- index 1))))

;P04: Find the number of elements of a list
;(length lst)

;P05: Reverse a list
;(reverse lst)

;P06: Find out whether a list is a palindrome
(define (palindrome? lst)
    (if (null? lst)
        #t
        (if (null? (cdr lst))
            #t
            (if (equal? (car lst) (last lst))
                (palindrome? (init (cdr lst)))
                #f))))

(define (palindrome-string? str) (palindrome? (string->list str)))

;P07: Flatten a nested list structure
(define (++ l1 l2)
    (if (null? l2)
        l1
        (if (null? l1)
            l2
            (cons (car l1) (++ (cdr l1) l2)))))

(define (flatten lst)
    (if (null? lst)
        lst
        (if (not (list? (car lst)))
            (cons (car lst) (flatten (cdr lst)))
            (++ (flatten (car lst)) (flatten (cdr lst))))))

;P08: Eliminate consecutive duplicates of list elements
(define (compress lst)
    (if (null? lst)
        '()
        (foldr compress-helper '() lst)))

(define (compress-helper cur acc)
    (if (null? acc)
        (cons cur acc)
        (if (equal? cur (car acc)) acc (cons cur acc))))

;P09: Pack consecutive duplicates of list elements into sublists
(define (pack lst) (foldr pack-helper '() lst))
(define (pack-helper cur acc)
    (if (null? acc)
        (cons (cons cur '()) acc)
        (if (equal? cur (car (car acc)))
            (cons (cons cur (car acc)) (cdr acc))
            (cons (cons cur '()) acc))))

;P10: Run-length encoding of a list
(define (encode lst) (map encode-single (pack lst)))
(define (encode-single lst) (cons (length lst) (cons (car lst) '())))

;P11: Modified run-length encoding
(define (encode-modified lst) (map encode-single-modified (pack lst)))
(define (encode-single-modified lst)
    (if (= (length lst) 1)
        (car lst)
        (encode-single lst)))

;P12 Decode a run-length encoded list
(define (decode lst) (flatten (map decode-single lst)))
(define (decode-single lst)
    (if (list? lst)
        (replicate (car lst) (cdr lst))
        (cons lst '())))

(define (replicate times thing)
    (if (<= times 0)
        '()
        (cons thing (replicate (- times 1) thing))))

;P13: Run-length encoding of a list (direct solution)
(define (encode-direct lst) (foldr encode-helper '() lst))
(define (encode-helper cur acc)
    (if (null? acc)
        (cons cur acc)
        (if (list? (car acc))
            (if (equal? (car (cdr (car acc))) cur)
                (cons (cons (+ 1 (car (car acc))) (cons cur '())) (cdr acc))
                (cons cur acc))
            (if (equal? cur (car acc))
                (cons (cons 2 (cons cur '())) (cdr acc))
                (cons cur acc)))))

;P14: Duplicate the elements of a list
(define (duplicate lst) (flatten (map (curry replicate 2) lst)))

;P15: Replicate the elements of a list a given number of times
(define (repli lst num) (flatten (map (curry replicate num) lst)))

;P16: Drop every nth element from a list
(define (drop lst num) (flatten (map (curry take (- num 1)) (group lst num))))
(define (take num lst)
    (if (or (<= num 0) (null? lst))
        '()
        (cons (car lst) (take (- num 1) (cdr lst)))))
(define (group lst num) (reverse (map reverse (fold group-helper '() lst))))
(define (group-helper acc cur)
    (if (null? acc)
        (cons (cons cur '()) acc)
        (if (= (length (car acc)) 3)
            (cons (cons cur '()) acc)
            (cons (cons cur (car acc)) (cdr acc)))))

;P17: Split a list into two parts; the length of the first part is given
(define (split lst num)
    (if (or (null? lst) (<= num 0))
        (cons '() (cons lst '()))
        (cons (cons (car lst) (car (split (cdr lst) (- num 1)))) (cdr (split (cdr lst) (- num 1))))))

;P18: Extract a slice from a list (Indexing starts from 0, both indices included
(define (slice lst start end) (car (split (car (cdr (split lst start))) (+ (- end start) 1))))

;P19: Rotate a list n places to the left
(define (rotate lst num)
    (if (= num 0)
        lst
        (if (> num 0)
            (rotate (++ (cdr lst) (cons (car lst) '())) (- num 1)
            (rotate (cons (last lst) (init lst)) (+ num 1))))))

;P20: Remove the kth element from a list (indexing from 0)
(define (remove-at lst num)
    (if (< num 1)
        (cdr lst)
        (cons (car lst) (remove-at (cdr lst) (- num 1)))))

;P21: Insert an element at a given position into a list
(define (insert-at elem lst num)
    (if (<= num 1)
        (cons elem lst)
        (cons (car lst) (insert-at elem (cdr lst) (- num 1)))))

;P22: Create a list containing all integers within a given range
(define (range start end)
    (if (= end start)
        (cons start '())
        (if (> end start)
            (cons start (range (+ start 1) end))
            (cons start (range (- start 1) end)))))

;P23: Extract a given number of randomly selected elements from a list

;pop returns a pair with the first element the element at given index of lst, and the second element the remaining list (indexing from 0)
(define (pop lst index)
    (if (or (null? lst) (< index 0) (> index (- (length lst) 1)))
        lst
        (cons (elem-at lst index) (remove-at lst index))))
(define (rnd-select lst num)
    (take num (rnd-helper lst num (random (length lst)))))

(define (rnd-helper lst num index)
    (if (or (< index 0) (>= index (length lst)) (> num (length lst)))
        lst
        (cons (car (pop lst index)) (rnd-helper (cdr (pop lst index)) (- num 1) (random (- (length lst) 1))))))

;P24: Draw N different random numbers from the set 1..M
(define (lotto-select n m) (rnd-select (range 1 m) n))

;P25: Generate a random permutation of the elements of a list
(define (rnd-permu lst) (permu-helper lst (random (length lst))))
(define (permu-helper lst index)
    (if (null? lst)
        lst
        (cons (car (pop lst index)) (permu-helper (cdr (pop lst index)) (random (length lst))))))

;P26: Generate the combinations of K distinct objects chosen from the N elements of a list

