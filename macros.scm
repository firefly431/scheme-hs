(define-syntax letrec
  (syntax-rules ()
    ((_ ((var init) ...) . body)
     (let ()
       (define var init)
       ...
       (let () . body)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
        (test expr ...)
        command ...)
    (letrec
      ((loop
        (lambda (var ...)
          (if test
              (begin
                (if #f #f)
                expr ...)
              (begin
                command
                ...
                (loop (do "step" var step ...)
                      ...))))))
      (loop init ...)))
   ((do "step" x)
    x)
   ((do "step" x y)
    y)))
