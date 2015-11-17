(define assert (lambda (x) (if x () (begin (display "FAIL") (newline))) x))
(define assert_not (lambda (x) (if x (begin (display "FAIL") (newline))) x))

;;; testing "and" macro
(assert_not (and #t #f))
(assert_not (and #f #t))
(assert (and #t #t #t))
(assert_not (and #f (display "FAIL\n")))
;;; testing "or" macro
(assert (or #t #f))
(assert (or #f #t))
(assert_not (or #f #f #f))
(assert (or #t (display "FAIL\n")))
;;; testing "let" macro
(assert (let ((x 1) (y 5)) (equal? x 1)))
(assert (let ((x 2) (y 2)) (equal? y 2)))
(assert (let ((x 1) (y 5) (z 5)) (and (equal? x 1) (equal? y 5) (equal? z 5))))
;;; testing "cond" macro
(assert (let ((x 5))
    (cond
        ((equal? x 5) #t)
        ((equal? x 3) #f)
        (else #f))))
(assert (let ((x 3))
    (cond
        ((equal? x 5) #f)
        ((equal? x 3) #t)
        (else #f))))
(assert (let ((x 1))
    (cond
        ((equal? x 5) #f)
        ((equal? x 3) #f)
        (else #t))))
(assert (equal? 8 (let ((x 7))
    (cond
        (x => (lambda (y) (+ y 1)))
        (else #f)))))
(assert (equal? #f (let ((x #f))
    (cond
        (x => (lambda (y) (+ y 1)))
        (else #f)))))
;;; testing "case" macro
;;; doesn't work until I implement memv
#|
; this doesn't work in standard Scheme because macros are hygienic
(define eqv? equal?)
(assert (equal? 'composite (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite))))

(assert_not (case (* 1 2)
    ((4) 'a)
    ((5) 'b)))

(assert (equal? 'consonant (letrec ((a #\a) (e #\e) (i #\i) (o #\o) (u #\u) (w #\w) (y #\y) (letter c)) (case (letter)
    ((a e i o u) 'vowel)
    ((w y) 'semivowel)
    (else 'consonant)))))
|#
;;; testing "do" macro
; this is really not used very often
; does not work because it unwraps greedily
; (do ((x 1 1) (y 1 1)) ((and (equal? x 5) (equal? y 5)) (+ x y)) (display x) (display y) (newline))
;;; testing "letrec" macro
; does not work because of non-lexical scope
(assert (letrec ((is-even? (lambda (n)
                       (or (equal? 0 n)
                           (is-odd? (- 1 n)))))
                 (is-odd? (lambda (n)
                      (and (> n 0)
                           (is-even? (- 1 n))))))
    (is-odd? 11)))
