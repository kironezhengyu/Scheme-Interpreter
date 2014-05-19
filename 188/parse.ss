; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
;@author qinz
;@date apr 15th 2014

(load "chez-init.ss")

   (define-datatype expression expression?
  (var-exp
   (id symbol?))
  (app-exp
   (rator expression?)
   (rand (list-of expression?)))
  (lit-exp
   (id scheme-value?))
  (lambda-exp
   (id (list-of scheme-value?))
   (body expression?))
  (no-parens-lambda-exp
   (id symbol?)
   (body expression?))
  (improper-lambda-exp
    (id list?)
    (sym symbol?)
    (body  expression?))
  (let-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body list?))
  (let*-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?)))
  (letrec-exp
   (vars (list-of symbol?))
   (vals (list-of (list-of symbol?)))
   (bodies (list-of expression?))
   (letrec-bodies (list-of scheme-value?)))
  (named-let-exp
   (name symbol?)
   (ids (list-of symbol?))
   (values (list-of expression?))
   (body expression?))
  (set!-exp
   (id symbol?)
   (body expression?))
  (define-exp
    (id symbol?)
    (body expression?))
  (begin-exp
  (exps (list-of scheme-value?)))
  (ref-exp
    (id symbol?))
  (if-exp
   (test expression?)
   (true expression?)
   (false expression?))
  (no-else-if-exp
   (test scheme-value?)
   (true scheme-value?))
  (cond-exp
    (body list?))
  (or-exp
    (body list?))
  (and-exp
    (body list?))
  (case-exp 
    (test list?)
    (body list?))
  (while-exp
      (test expression?)
      (bodys (list-of expression?)))
  )

(define proper-list?
  (lambda (l)
    (cond [(null? l) #t]
          [(pair? l) (proper-list? (cdr l))]
          [else #f])))

(define contains?
  (lambda (l n)
    (if (null? l)
        #f
        (if (equal? (car l) n)
            #t
            (contains? (cdr l) n)))))
(define set?
  (lambda (l)
    (if (null? l)
        #t
        (let set?? ((l1 (list (car l))) (l2 (cdr l)))
          (cond
           ((null? l2) #t)
           ((contains? l1 (car l2)) #f)
           (else (set?? (cons (car l2) l1) (cdr l2))))))))


(define validLambda?
  (lambda (expr)
    (cond
     ; Check to see that we have at least lambda, an argument, and expressions
     [(< (length expr) 3)
      (eopl:error  'parse-expression "Incorrect length in ~s" expr)]
     ; Make sure the arguments are valid
     [(not (or (symbol? (cadr expr))
         (null? (cadr expr))
         (and (or (list? (cadr expr)) (pair? (cadr expr)))
        (or (pair? (cadr expr)) (andmap symbol? (cadr expr))))))
      (eopl:error  'parse-expression "Incorrect argument list in ~s" expr)]
     ; Make sure each variable only occurs once
     [(if (list? (cadr expr))
    (not (set? (cadr expr)))
    #f)
      (eopl:error  'parse-expression "Each variable may only occur once in ~s" expr)]
     [else #t])))


(define andmap
  (lambda (ls)
    (if (null? ls)
        #t
        (and (car ls) (andmap (cdr ls))))))

(define proper-list?
  (lambda (ls)
    (cond
      ((null? ls) #t)
      ((and (list? ls) (proper-list? (cdr ls))))
      (else #f))))

(define contain-multiple?
  (lambda (ls)
    (cond
      ((null? ls) #f)
      ((member (car ls) (cdr ls)) #t)
      (else (contain-multiple? (cdr ls))))))

(define parse-parms
    (lambda (ls)
      (if (symbol? ls)
            ls
            (let ([t (parse-parms (cdr ls))])
                (if (symbol? t)
                    (cons (list (car ls)) (list (list t)))
                    (cons (cons (car ls) (car t)) (cdr t)))))))

;Problem 2
(define parse-exp
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((boolean? datum) (lit-exp datum))
      ((string? datum) (lit-exp datum))
      ((vector? datum) (lit-exp datum))
      ((number? datum) (lit-exp datum))
      ((not (proper-list? datum)) (eopl:error 'parse-exp "Error in parse-exp: Improper list in ~s" datum))
      ((pair? datum)
       (cond
         ((not (proper-list? datum)) (lit-exp datum))
         
         ((eqv? (car datum) 'quote) 
          (if (equal? (length datum) 2)
              (lit-exp  (cadr datum))
              (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))

  [(eqv? (car datum) 'lambda)
   (if (validLambda? datum)
    (cond 
       [(symbol? (cadr datum)) (no-parens-lambda-exp(cadr datum)
     (begin-exp(map parse-exp (cddr datum))))]
     [(list? (cadr datum)) (lambda-exp  (cadr datum)
       (begin-exp(map parse-exp (cddr datum))))]
     [(pair? (cadr datum))
   (let ([t (parse-parms (cadr datum))])
     (improper-lambda-exp (car t) (caadr t)  (begin-exp(map parse-exp (cddr datum)))))]))]
         
         ((eqv? (car datum) 'let)
          (cond
            ((and (> (length datum) 2) (proper-list? (cadr datum)) 
                  (andmap (map proper-list? (cadr datum)))
                  (andmap (map (lambda (ls) (equal? (length ls) 2)) (cadr datum)))
                  (andmap (map (lambda (ls) (symbol? (car ls)))(cadr datum)))
                  (not (contain-multiple? (map car (cadr datum))))) 
             (let-exp (map parse-exp (map car (cadr datum))) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum))))
            ((and (> (length datum) 2) (proper-list? (caddr datum)) 
                  (andmap (map proper-list? (caddr datum))) 
                  (andmap (map (lambda (ls) (equal? (length ls) 2)) (caddr datum))) 
                  (andmap (map (lambda (ls) (symbol? (car ls)))(caddr datum))) 
                  (not (contain-multiple? (map car (caddr datum)))) (symbol? (cadr datum))) 
             (named-let-exp (cadr datum) (map car (caddr datum)) (map parse-exp (map cadr (caddr datum))) (parse-exp (cadddr datum))))
            (else (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum))))
         
         ((eqv? (car datum) 'let*)
          (if (and (> (length datum) 2) (proper-list? (cadr datum)) 
                  (andmap (map proper-list? (cadr datum)))
                  (andmap (map (lambda (ls) (equal? (length ls) 2)) (cadr datum)))
                  (andmap (map (lambda (ls) (symbol? (car ls)))(cadr datum)))
                  (not (contain-multiple? (map car (cadr datum)))))
              (let*-exp (map parse-exp (map car (cadr datum))) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))
              (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))
         
         ((eqv? (car datum) 'letrec)
          (if (validate-let datum)
              (letrec-exp (map car (cadr datum)) (map cadadr (cadr datum)) (map parse-exp (map (lambda(x) (car (cddadr x))) (cadr datum)))
                (map parse-exp (cddr datum)))
              (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))
         
         ((eqv? (car datum) 'set!)
          (if (not (equal? (length datum) 3))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)
              (set!-exp (cadr datum) (parse-exp (caddr datum)))))

         ((eqv? (car datum) 'define)
          (if (not (equal? (length datum) 3))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)
              (define-exp (cadr datum) (parse-exp (caddr datum)))))

         ((eqv? (car datum) 'if)
          (if (or (< (length datum) 3) (> (length datum) 4))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)
              (if (null? (cdddr datum)) ;w/o else
                  (no-else-if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))
                  (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))))) ;if w/ else
         ((eqv? (car datum) 'cond)
          (cond-exp (map (lambda (x) (list (parse-exp (car x)) (parse-exp (cadr x)))) (cdr datum))))
    [(eq? (car datum) 'case)
    (case-exp (parse-exp (cadr datum))
      (letrec ([helper (lambda (ls)
        (if (null? (cdr ls))
          (if (eq? (caar ls) 'else)
            (list (list (caar ls) (parse-exp (cadar ls))))
            (list (list (caar ls) (parse-exp (cadar ls)))))
        (cons (list (caar ls) (parse-exp (cadar ls))) (helper (cdr ls)))))])
  (helper (cddr datum))))]



  [(eqv? (car datum) 'while)
    (while-exp (parse-exp (cadr datum))
        (map parse-exp (cddr datum)))]

          ; ((eqv? (car datum) 'case)
          ; (case-exp (parse-exp (cadr datum)) (map (lambda (x) (list (parse-exp (car x)) (parse-exp (cadr x)))) (cddr datum))))
         ((eqv? (car datum) 'begin)
          (begin-exp (map (lambda (x) (parse-exp x)) (cdr datum))))
         ((eqv? (car datum) 'or)
          (or-exp (map (lambda (x) (parse-exp x)) (cdr datum))))
         ((eqv? (car datum) 'and)
          (and-exp (map (lambda (x) (parse-exp x)) (cdr datum))))
         (else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum))))))
      (else (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))))

(define validate-let
  (lambda (datum)
    (cond
      ((symbol? (cadr datum)) (eopl:error 'parse-exp "Error in parse-exp : Declarations must be lists in ~s" datum))
      ((null? (cadr datum)) (eopl:error 'parse-exp "Error in parse-exp: No declarations made in ~s" datum))
      ((null? (cddr datum)) (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum))
      ((null? (caadr datum)) (eopl:error 'parse-exp "Error in parse-exp: No declarations made in ~s" datum))
      ((not (list? (caadr datum))) (eopl:error 'parse-exp "Error in parse-exp : Declarations must be lists in ~s" datum))
      ((not (andmap (map (lambda (ls) (equal? (length ls) 2)) (cadr datum)))) (eopl:error 'parse-exp "Error in parse-exp: Incorrect length of declarations in ~s" datum))
      ((not (andmap (map symbol? (map car (cadr datum))))) (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax, declarations must be symbols in ~s" datum))
      ((contain-multiple? (map car (cadr datum))) (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax, declarations must not repeat symbols in ~s" datum))
      (else #t))))




(define unparse-exp
  (lambda (exp)
    (cases expression exp
      (var-exp (id) id)
      (lit-exp (id) id)
      
      (let-exp (ids values body)
               (append (list 'let (recover-let (map unparse-exp ids) (map unparse-exp values))) (map unparse-exp body)))
      (named-let-exp (name ids values body)
                     (append (list 'let (unparse-exp name) (recover-let (map unparse-exp ids) (map unparse-exp values))) (map unparse-exp body)))
      (let*-exp (ids values body)
               (append (list 'let* (recover-let (map unparse-exp ids) (map unparse-exp values))) (map unparse-exp body)))
      (letrec-exp (ids values body)
               (append (list 'letrec (recover-let (map unparse-exp ids) (map unparse-exp values))) (map unparse-exp body)))
      
      (lambda-exp (id body)
        (append (list 'lambda (map unparse-exp id))
          (map unparse-exp body)))
      (no-parens-lambda-exp (id body)
                            (append (list 'lambda id)
                                    (map unparse-exp body)))
      (improper-lambda-exp (id body)
                           (append (list 'lambda (unparse-exp id))
                                    (map unparse-exp body)))
      
      (set!-exp (id body)
                (list 'set! id (unparse-exp body)))
      
      (if-exp (test true false)
              (list 'if (unparse-exp test) (unparse-exp true) (unparse-exp false)))
      (no-else-if-exp (test true)
              (list 'if (unparse-exp test) (unparse-exp true)))
      
      (app-exp (rator rand)
        (append (list (unparse-exp rator)) (map unparse-exp rand)))
      
      (else (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax in ~s" exp)))))

(define recover-let
  (lambda (ids vals)
    (if (null? ids)
        '()
        (cons (list (car ids) (car vals)) (recover-let (cdr ids) (cdr vals))))))





