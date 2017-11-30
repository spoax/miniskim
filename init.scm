
; Some helper functions

(define caar (lambda (x)    (car (car x))))
(define cdar (lambda (x)    (cdr (car x))))
(define cddr (lambda (x)    (cdr (cdr x))))
(define cadr (lambda (x)    (car (cdr x))))
(define caddr (lambda (x)   (car (cddr x))))
(define cadddr (lambda (x)  (car (cdr (cddr x)))))

(define pi 3.1415928)
(define e  2.718281828459)

(define map (lambda (f lst) (if (null? lst) (quote ()) (cons (f (car lst)) (map f (cdr lst))))))

(define range (lambda (n m) (if (zero? (- m n)) (quote ()) (cons n (range (+ n 1) m)))))

(define sqr (lambda (n) (* n n)))

