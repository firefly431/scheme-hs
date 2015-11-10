(define-syntax foo
    (syntax-rules ()
        ((foo a...) (display a...))))

(foo 1 2)
