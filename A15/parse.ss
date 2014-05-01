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
   (id (list-of expression?))
   (body (list-of expression?)))
  (no-parens-lambda-exp
   (id symbol?)
   (body (list-of expression?)))
  (improper-lambda-exp
   (ids expression?)
   (body (list-of expression?)))

  (let-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?)))
  (let*-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?)))
  (letrec-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?)))
  (named-let-exp
   (name expression?)
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?)))
  
  (set!-exp
   (id symbol?)
   (body expression?))
  
  (if-exp
   (test expression?)
   (true expression?)
   (false expression?))
  (no-else-if-exp
   (test expression?)
   (true expression?))
  )

(define scheme-value?
  (lambda (value)
    #t))


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
              (lit-exp datum)
              (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))
         
         ((eqv? (car datum) 'lambda)
          (if (> (length datum) 2)
              (if (symbol? (cadr datum))
                  (no-parens-lambda-exp (cadr datum) (map parse-exp (cddr datum)))
                  (if (and (proper-list? (cadr datum)) (andmap (map symbol? (cadr datum))) (not (contain-multiple? (cadr datum))))
                      (lambda-exp (map parse-exp (cadr datum)) (map parse-exp (cddr datum)))
                      (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)))
         
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
             (named-let-exp (var-exp (cadr datum)) (map parse-exp (map car (caddr datum))) (map parse-exp (map cadr (caddr datum))) (map parse-exp (cdddr datum))))
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
              (letrec-exp (map parse-exp (map car (cadr datum))) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))
              (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))
         
         ((eqv? (car datum) 'set!)
          (if (not (equal? (length datum) 3))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)
              (set!-exp (cadr datum) (parse-exp (caddr datum)))))
         
         ((eqv? (car datum) 'if)
          (if (or (< (length datum) 3) (> (length datum) 4))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)
              (if (null? (cdddr datum)) ;w/o else
                  (no-else-if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))
                  (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))))) ;if w/ else
         
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






