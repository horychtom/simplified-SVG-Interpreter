#lang racket
(require rackunit)
(require racket/trace)

(provide execute)


;------SVG tag functions-----------------------
(define (get-circle-tag args)
    (let ([x (list-ref args 0)]
          [y (list-ref args 1)]
          [r (list-ref args 2)]
          [style (list-ref args 3)])
    (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x y r style))
)

(define (get-rect-tag args)
    (let ([x (list-ref args 0)]
          [y (list-ref args 1)]
          [width (list-ref args 2)]
          [height (list-ref args 3)]
          [style (list-ref args 4)])
    (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y width height style))
)

(define (get-line-tag args)
    (let ([x1 (list-ref args 0)]
          [y1 (list-ref args 1)]
          [x2 (list-ref args 2)]
          [y2 (list-ref args 3)]
          [style (list-ref args 4)])
    (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style))
)
;-----------------------------------------------

;-------SYNTAX functions------------------------
(define (is-if-statement expr)
    (eq? 'if (car expr))
)

(define (is-when-statement expr)
    (eq? 'when (car expr))
)

(define (is-boolean-expr expr)
    (define op (car expr))
    (if (list? (member op (list '= '< '>))) #t #f)
)

(define (is-primitive-call expr)
    (define id (car expr))
    (if (list? (member id (list 'circle 'rect 'line))) #t #f )
)
;-----------------------------------------------

;-------EVALUATION functions--------------------

 (define (eval-num-expr expr)
    (let ([op (if (not(number? expr)) (car expr) expr)]
          [args (if (not (number? expr)) (cdr expr) expr)])

        (cond
            ([number? op] op)
            ([eq? '+ op] 
                (apply + (map eval-num-expr args)))
            ([eq? '- op] 
                (apply - (map eval-num-expr args)))
            ([eq? '* op] 
                (apply * (map eval-num-expr args)))
            ([eq? '/ op] 
                (apply / (map eval-num-expr args)))
            ([eq? 'floor op] 
                (apply floor (map eval-num-expr args)))
            ([eq? 'cos op] 
                (apply cos (map eval-num-expr args)))
            ([eq? 'sin op] 
                (apply sin (map eval-num-expr args)))
        )
    )
 )

 (define (eval-bool expr)
    (let ([op (car expr)]
          [arguments (cdr expr)])
          
          (cond
            ([eq? op '=] (apply = (map eval-num-expr arguments)))
            ([eq? op '>] (apply > (map eval-num-expr arguments)))
            ([eq? op '<] (apply < (map eval-num-expr arguments)))
          )
    )
)
 
(define (eval-when expr prg acc)

    (define (iter exprs accu)
        (if (null? exprs)
            accu
            (iter (cdr exprs) (string-append accu (car exprs))))
    )

    (let ([bool-expr (car expr)]
          [expressions (cdr expr)])

         (if (eq? #t (eval-bool bool-expr))
            (iter (map (curryr eval-expr prg acc) expressions) acc)
            acc
        )
    )
)

(define (eval-args expr)
    (if (string? expr)
         expr 
        (eval-num-expr expr))
)

(define (eval-if expr prg acc)
    (let ([bool-expr (list-ref expr 1)]
          [then-expr (list-ref expr 2)]
          [else-expr (list-ref expr 3)])

            (if (eq? #t (eval-bool bool-expr))
            (eval-expr then-expr prg acc)
            (eval-expr else-expr prg acc)))
)

;the only function which changes the acc
(define (eval-app expr acc)
    (let ([svg-op (car expr)]
          [args (cdr expr)])

        (cond
            ([eq? svg-op 'circle]
                (string-append acc (get-circle-tag (map eval-args args))))
            ([eq? svg-op 'rect]
                (string-append acc (get-rect-tag (map eval-args args))))
            ([eq? svg-op 'line]
                (string-append acc (get-line-tag (map eval-args args))))
        )
    )
)


;-----------PRG-FUNCTIONS-----------------------

(define (replace lst symbol value)
    (map (lambda (x) (if (list? x) (replace x symbol value) (if (eq? symbol x) value x))) lst)
)

;-----------------------------------------------



(define (get-bindings symb-args num-args)
     (map (lambda (x y) (list x y)) symb-args num-args)
 )


(define (get-f f-name prg)
    (car (filter (lambda (x) (eq? (car (cadr x)) f-name)) prg))
)

(define (get-binded-exprs body pairs)
    (if (null? pairs)
        body
        (get-binded-exprs (replace body (car (car pairs)) (car (cdr (car pairs)))) (cdr pairs))
    )
)



 (define (eval-f-call f-call prg acc)
    (define (iter exprs accu)
        (if (null? exprs)
            accu
            (iter (cdr exprs) (string-append accu (car exprs))))
    )

    (let* ([f-name (car f-call)]
          [num-args (map eval-args (cdr f-call))]
          [definition (get-f f-name prg)]
          [symb-args (cdr (cadr definition))]
          [body (cdr (cdr definition))]
          [binded-pairs (get-bindings symb-args num-args)]
          [new-body (get-binded-exprs body binded-pairs)]
          )
          (iter (map (curryr eval-expr prg acc) new-body) acc)
    )
 )



;basic expression semaphore
(define (eval-expr expr prg acc)
    (cond
        ([eq? #t (is-if-statement expr)] (eval-if expr prg acc))
        ([eq? #t (is-when-statement expr)] (eval-when (cdr expr) prg acc))
        ([eq? #t (is-primitive-call expr)] (eval-app expr acc))
        (else (eval-f-call expr prg acc))
    )
)

;-----------------------------------------------


;-----------EXECUTION---------------------------

(define (wrap-output width height str)
    (define output (format "<svg width=\"~a\" height=\"~a\">" width height))
    (string-append (string-append output str) "</svg>")
)

(define (execute width height prg expr)
        (wrap-output width height (eval-expr expr prg ""))
)

;------------------------------------------------


;---------------TESTS------------------------------------------------------------
;just in case if i changed it 
(check-equal? (is-if-statement '(if 2 4)) #t "err in syntax function")
(check-equal? (is-boolean-expr '(= 3 a)) #t "err in syntax function")
(check-equal? (is-boolean-expr '(+ 3 a)) #f "err in syntax function")

;num-expression evaluation:
(check-equal? (eval-num-expr '(/ (+ 1 2) (+ 3 4)) ) 3/7 "num-expr err :(")
(check-equal? (eval-num-expr '(+ 1 2)) 3 "num-expr err :(")
(check-equal? (eval-num-expr '(* 1 2 3)) 6 "num-expr err :(")
(check-equal? (eval-num-expr '(floor (/ (* 1 2 3) (+ 2 5)))) 0 "num-expr err :(")

;bool-expression evaluation:
(check-equal? (eval-bool '(= (+ 1 2) (+ 3 4)) ) #f "bool-expr err :(")
(check-equal? (eval-bool '(= (+ 1 2) (+ 3 0) (+ 2 1)) ) #t "bool-expr err :(")
(check-equal? (eval-bool '(> (+ 1 2) (+ 2 0)) ) #t "bool-expr err :(")

;general eval testing
(check-equal? (execute 400 400 '() '(when (> 2 1)
                                (circle 200 200 (floor (/ 200 3)) "fill:red")
                                (circle 200 200 (floor (/ 200 3)) "fill:red")
                                 (circle 200 200 (floor (/ 200 3)) "fill:blue")))
"<svg width=\"400\" height=\"400\"><circle cx=\"200\" cy=\"200\" r=\"66\" style=\"fill:red\"/><circle cx=\"200\" cy=\"200\" r=\"66\" style=\"fill:red\"/><circle cx=\"200\" cy=\"200\" r=\"66\" style=\"fill:blue\"/></svg>"
"failed"
)

(check-equal? (execute 400 400 '() '(circle 200 200 (floor (/ 200 3)) "fill:red"))
"<svg width=\"400\" height=\"400\"><circle cx=\"200\" cy=\"200\" r=\"66\" style=\"fill:red\"/></svg>"
"failed eval - go back to evaluation phase"
 )

 ;utils
 (check-equal? (get-bindings '(a b c) '(1 2 3)) '((a 1) (b 2) (c 3)) "binding-error")

 