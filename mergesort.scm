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

(define merge-sort (lambda (l)
    (cond
        ((null? l) l)
        ((equal? (length l) 1) l)
        (else
            (let
                    ((i (ratchet-floor (/ (length l) 2))))
                (let
                        ((l2 (split-at l i)))
                    (append (merge-sort (car l2)) (merge-sort (cdr l2)))))))))

(define mylist '(3 4 2 1 5))

(display (merge-sort mylist))
(newline)
