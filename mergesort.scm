(define split-at (lambda (l i)
    (cond
        ((equal? i 0) (cons () l))
        (else
            (let ((res (split-at (cdr l) (- i 1))))
                (cons (cons (car l) (car res)) (cdr res)))))))

(define ratchet-floor (lambda (n)
    (if (integer? n)
        n
        (- n 0.5))))

(define merge (lambda (l1 l2)
    (cond
        ((null? l1) l2)
        ((null? l2) l1)
        (else (let
                ((vals (if (< (car l1) (car l2)) (list (car l1) (cdr l1) l2) (list (car l2) (cdr l2) l1))))
            (cons (car vals) (merge (car (cdr vals)) (car (cdr (cdr vals))))))))))

(define merge-sort (lambda (l)
    (cond
        ((null? l) l)
        ((equal? (length l) 1) l)
        (else
            (let
                    ((i (ratchet-floor (/ (length l) 2))))
                (let
                        ((l2 (split-at l i)))
                    (merge (merge-sort (car l2)) (merge-sort (cdr l2)))))))))

(define mylist '(3 4 2 1 5))

(display (merge-sort mylist))
(newline)
