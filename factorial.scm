(define factorial (lambda (x)
    (if (not (integer? x))
        (error "not an integer")
        (cond
            ((negative? x) (error "negative factorial"))
            ((equal? x 0) 1)
            (else (* x (factorial (- x 1))))))))

(display (factorial 5))
; (display (factorial 1))
; (display (factorial 0))
; (display (factorial +i))
; (display (factorial -1))
(newline)
