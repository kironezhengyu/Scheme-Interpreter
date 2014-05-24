
; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.
;@author qinz
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define scheme-value? (lambda (v) #t))

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lit-exp
   (id scheme-value?))
  (lambda-exp
   (vars list?)
   (bodies expression?))
  (begin-exp
   (bodies (list-of expression?)))
  (app-exp
   (exps (list-of expression?)))
  (if-exp
   (condition expression?)
   (if-else (list-of expression?)))
  (cond-exp
   (bodies (list-of (list-of expression?))))
  (case-exp
   (id expression?)
   (cases (list-of scheme-value?)))
  (let-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (nlet-exp
   (name symbol?)
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (set-exp
   (var symbol?)
   (val expression?))
  (and-exp
   (bodies (list-of expression?)))
  (or-exp
   (bodies (list-of expression?)))
  (while-exp
	(test-exp expression?)
	(bodies expression?))
  (define-exp
    (sym symbol?)
    (val expression?))
  (exit-exp 
  	(args (list-of expression?)))
  (break-exp
   (bodies (list-of expression?)))
  (call/cc-exp
	(body expression?))
)


(define parse-exp
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(vector? datum) (lit-exp datum)]
	  [(null? datum) (lit-exp datum)]
	  [(eqv? datum '#t) (lit-exp datum)]
	  [(eqv? datum '#f) (lit-exp datum)]
	  [(string? datum) (lit-exp datum)]
	  [(pair? datum)
	   (cond [(not (list? datum)) (eopl:error 'parse-exp "Error while parsing list: Not a valid list ~s" datum)]
		 [(eq? (car datum) 'call/cc)
			(call/cc-exp (parse-exp (cadr datum)))]
		 [(eq? (car datum) 'exit)
		  (exit-exp (map parse-exp (cdr datum)))]
		 [(eq? (car datum) 'begin)
		  (begin-exp (map parse-exp (cdr datum)))]
		 [(eq? (car datum) 'break)
		  (break-exp (map parse-exp (cdr datum)))]
		 [(eq? (car datum) 'quote)
		  (if (not (= (length datum) 2)) (eopl:error 'parse-exp "Error while parsing quote: Invalid length: ~s" datum)
		      (lit-exp (cadr datum)))]
		 [(eq? (car datum) 'lambda)
		  (cond [(not-lambda-length? datum)(eopl:error 'parse-exp "Error while parsing lambda: Invalid length: ~s"  datum)]
			[(invalid-formals-lambda? datum) (eopl:error 'parse-exp "Error while parsing lambda: Formals must be symbols: ~s" (cadr datum))]
			[else (if (= (length datum) 3)
				  (lambda-exp (list (cadr datum))
					      (parse-exp (caddr datum)))
				  (lambda-exp (list (cadr datum))
					      (begin-exp (map parse-exp (cddr datum)))))])]
		 [(eq? (car datum) 'if)
		  (cond [(not-if-length? datum) (eopl:error 'parse-exp "Error while parsing if: Invalid length: ~s" datum)]
			[else (if-exp (parse-exp (cadr datum))
				      (map parse-exp (cddr datum)))])]
		 [(eq? (car datum) 'cond)
		  (cond [(< (length datum) 2) (eopl:error 'parse-exp "Error while parsing cond: Invalid length: ~s" datum)]
			[(not (valid-cond-sublists? (cdr datum))) (eopl:error 'parse-exp "Error while parsing cond: Invalid condition format: ~s" datum)]
			[else (cond-exp (map (lambda (x) (list (parse-exp (car x)) (parse-exp (cadr x)))) (cdr datum)))])]
		 [(eq? (car datum) 'case)
		  
		  (cond [(< (length datum) 3) (eopl:error 'parse-exp "Error while parsing case: Invalid length: ~s" datum)]
			[(not (proper-cases? (cddr datum))) (eopl:error 'parse-exp "Error while parsing case: Invalid case syntax: ~s" datum)]
			[else (case-exp (parse-exp (cadr datum)) (map (lambda (x) (if (eqv? (car x) 'else) (list (parse-exp (car x)) (begin-exp (map parse-exp (cdr x))))
											(list (map parse-exp (car x)) (begin-exp (map parse-exp (cdr x)))))) (cddr datum)))])]
		 [(eq? (car datum) 'let*)
		  (cond [(< (length datum) 3) (eopl:error 'parse-exp "Error while parsing let*: Invalid length: ~s" datum)]
			[(not (list? (cadr datum))) (eopl:error 'parse-exp "Error while parsing let*: 1st argument is not a list: ~s" (cadr datum))]
			[(not (proper-sublists? (cadr datum))) (eopl:error 'parse-exp "Error while parsing let*: Invalid sublist in 1st argument: ~s" (cadr datum))]
			[else (if (= (length datum) 3)
				  (let*-exp (map car (cadr datum))
					   (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
				           (parse-exp (caddr datum)))
				  (let*-exp (map car (cadr datum))
					   (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
					   (begin-exp (map parse-exp (cddr datum)))))])]
		 [(eq? (car datum) 'letrec)
		  (cond [(< (length datum) 3) (eopl:error 'parse-exp "Error while parsing letrec: Invalid length: ~s" datum)]
			[(not (list? (cadr datum))) (eopl:error 'parse-exp "Error while parsing letrec: 1st argument is not a list: ~s" (cadr datum))]
			[(not (proper-sublists? (cadr datum))) (eopl:error 'parse-exp "Error while parsing letrec: Invalid sublist in 1st argument: ~s" (cadr datum))]
			[else (letrec-exp (map car (cadr datum))
					  (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
					  (begin-exp (map parse-exp (cddr datum))))])]
		 [(and (eq? (car datum) 'let) (symbol? (cadr datum)))
		  (cond [(< (length datum) 4) (eopl:error 'parse-exp "Error while parsing named let: Invalid length: ~s" datum)]
			[(not (list? (caddr datum))) (eopl:error 'parse-exp "Error while parsing named let: 2nd argument is not a list: ~s" (caddr datum))]
			[(not (proper-sublists? (caddr datum))) (eopl:error 'parse-exp "Error while parsing named let: Invalid sublist in 2nd argument: ~s" (caddr datum))]
			[else (nlet-exp (cadr datum)
					(map car (caddr datum))
					(map (lambda (x) (parse-exp (cadr x))) (caddr datum))
					(begin-exp (map parse-exp (cdddr datum))))])]
		 [(eq? (car datum) 'let)
		  (cond [(< (length datum) 3) (eopl:error 'parse-exp "Error while parsing let: Invalid length: ~s" datum)]
			[(not (list? (cadr datum))) (eopl:error 'parse-exp "Error while parsing let: 1st argument is not a list: ~s" (cadr datum))]
			[(not (proper-sublists? (cadr datum))) (eopl:error 'parse-exp "Error while parsing let: Invalid sublist in 1st argument: ~s" (cadr datum))]
			[else (if (= (length datum) 3)
				  (let-exp (map car (cadr datum))
					   (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
				           (parse-exp (caddr datum)))
				  (let-exp (map car (cadr datum))
					   (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
					   (begin-exp (map parse-exp (cddr datum)))))])]
		 [(eq? (car datum) 'set!)
		  (cond [(not (= (length datum) 3)) (eopl:error 'parse-exp "Error while parsing set!: Invalid length: ~s" datum)]
			[(not (symbol? (cadr datum))) (eopl:error 'parse-exp "Error while parsing set!: 1st argument must be a symbol: ~s" (cadr datum))]
			[else (set-exp (cadr datum)
				       (parse-exp (caddr datum)))])]
		 [(eq? (car datum) 'and)
		  (and-exp (map parse-exp (cdr datum)))]
		 [(eq? (car datum) 'or)
		  (or-exp (map parse-exp (cdr datum)))]
		 [(eq? (car datum) 'while)
			(while-exp (parse-exp (cadr datum)) (begin-exp (map parse-exp (cddr datum))))]
		 [(eq? (car datum) 'define)
		  (cond [(not (= (length datum) 3)) (eopl:error 'parse-expresssion "Error while parsing define: Invalid length: ~s" datum)]
			[(not (symbol? (cadr datum))) (eopl:error 'parse-exp "Error while parsing define: First argument must be a symbol: ~s" (cadr datum))]
			[else (define-exp (cadr datum) (parse-exp (caddr datum)))])]
		 [else (app-exp 
				(map parse-exp datum))])]
	  [else (eopl:error 'parse-exp "Invalid concrete syntac ~s" datum)])))


  
(define unparse-exp
  (lambda (exp)
    (cases expression exp
	   [var-exp (id) id]
	   [lit-exp (id) id]
	   [break-exp (bodies) (cons 'break (unparse-exp bodies))]
	   [begin-exp (bodies)
		    (cons 'begin (map unparse-exp bodies))]
	   [lambda-exp (vars bodies)
		       (cons 'lambda
			     (cons (car vars)
			     (map unparse-exp bodies)))]
	   [app-exp (exps)
			  (map unparse-exp exps)]
	   [if-exp (condition if-true if-false)
		    (if (null? (unparse-exp if-false))
			(list 'if
			      (unparse-exp condition)
			      (unparse-exp if-true))
			(list 'if
			      (unparse-exp condition)
			      (unparse-exp if-true)
			      (unparse-exp if-false)))]
	   [cond-exp (bodies)
		     (cons 'cond (map (lambda (x) (list (unparse-exp (car x)) (unparse-exp (cadr x)))) bodies))]
	   [let-exp (vars vals bodies)
		    (cons 'let
			  (cons (linklet vars vals '())
				(map unparse-exp bodies)))]
	   [let*-exp (vars vals bodies)
		     (cons 'let*
			   (cons (linklet vars vals '())
				 (map unparse-exp bodies)))]
	   [letrec-exp (vars vals bodies)
		       (cons 'letrec
			     (cons (linklet vars vals '())
				   (map unparse-exp bodies)))]
	   [nlet-exp (name vars vals bodies)
		     (cons 'let
			   (cons name
				 (cons (linklet vars vals '())
				       (map unparse-exp body))))]
	   [set-exp (var val)
		    (list 'set! var (unparse-exp val))]
	   [and-exp (bodies)
		    (cons 'and (map unparse-exp bodies))]
	   [or-exp (bodies)
		   (cons 'or (map unparse-exp bodies))]
	   [exit-exp ()
		     (list 'exit)])))

(define linklet
  (lambda (vars vals r)
    (if (null? vars) r
	(linklet (cdr vars) (cdr vals) (append r (list (list (car vars) (unparse-exp (car vals)))))))))
	  

(define not-lambda-length?
  (lambda (datum)
    (if(< (length datum) 3) #t #f)))

(define invalid-formals-lambda?
  (lambda (datum)
    (cond [(symbol? (cadr datum)) #f]
	  [(list? (cadr datum)) (not (symbols-list? (cadr datum)))]
	  [(pair? (cadr datum)) (not (symbols-pair? (cadr datum)))]
	  [else #t])))

(define symbols-list?
  (lambda (x)
    (if (null? x) #t
	(and (or (symbol? (car x)) (and (list? (car x)) (= (length (car x)) 2) (eqv? (caar x) 'ref))) (symbols-list? (cdr x))))))

(define symbols-pair?
  (lambda (x)
    (cond [(symbol? x) #t]
	  [(pair? x) (and (symbol? (car x)) (symbols-pair? (cdr x)))]
	  [else #f])))

(define not-if-length?
  (lambda (datum)
    (if (or (= (length datum) 3) (= (length datum) 4)) #f #t)))

(define proper-sublists?
  (lambda (x)
    (cond [(null? x) #t]
	  [(list? (car x)) (and (and (= (length (car x)) 2) (symbol? (caar x))) (proper-sublists? (cdr x)))]
	  [else #f])))

(define valid-cond-sublists?
  (lambda (x)
    (cond [(null? x) #t]
	  [(and (equal? (caar x) 'else) (not (null? (cdr x)))) #f]
	  [(= (length (car x)) 2) (valid-cond-sublists? (cdr x))]
	  [else #f])))

(define proper-cases?
  (lambda (x)
    (cond [(null? x) #t]
	  [(and (equal? (caar x) 'else) (not (null? (cdr x)))) #f]
	  [(and (list? (car x)) (or (eqv? (caar x) 'else) (list? (caar x))) (> (length (car x)) 1) (not (null? (caar x)))) (proper-cases? (cdr x))]
	  [else #f])))


(define syntax-expand
  (lambda (expr)
    (cases expression expr
	   [let-exp (syms vals bodies)
		    (app-exp (cons (lambda-exp (list syms) (syntax-expand bodies))
			     (map syntax-expand vals)))]
	   [lambda-exp (syms bodies)
		       (lambda-exp syms (syntax-expand bodies))]
	   [begin-exp (bodies)
		      (begin-exp (map syntax-expand bodies))]
	   [break-exp (bodies)
		      (break-exp  (map syntax-expand bodies))]
	   [app-exp (exps)
		    (app-exp (map syntax-expand exps))]
	   [if-exp (condition if-else)
		   (if-exp (syntax-expand condition) (map syntax-expand if-else))]
	   [set-exp (var val)
		    (set-exp var (syntax-expand val))]
		[cond-exp (bodies)
			(cond-exp (map (lambda (x) (list (syntax-expand (car x)) (syntax-expand (cadr x)))) bodies))]
		[and-exp (bodies)
			 (if (null? bodies)
			     (lit-exp #t)
			     (syntax-expand (and-convert (cdr (reverse bodies)) (car (reverse bodies)))))]
		[or-exp (bodies)
			(if (null? bodies)
			    (lit-exp #f)
			    (syntax-expand (or-convert (cdr (reverse bodies)) (car (reverse bodies)))))]
		[let*-exp (vars vals bodies)
			  (syntax-expand (let-convert (reverse vars) (reverse vals) bodies))]
		[letrec-exp (vars vals bodies)
			    (letrec-exp vars (map syntax-expand vals) (syntax-expand bodies))]
		[case-exp (id cases)
			(case-exp (syntax-expand id)
				  (map (lambda (x) (if (equal? (car x) (parse-exp 'else)) (list (syntax-expand (car x)) (syntax-expand (cadr x)))(list (map syntax-expand (car x)) (syntax-expand (cadr x))))) cases))]
		[while-exp (test-exp bodies)
			(while-exp (syntax-expand test-exp) (syntax-expand bodies))]
		[define-exp (sym val)
		  (define-exp sym (syntax-expand val))]
		[call/cc-exp (body)
		  (call/cc-exp (syntax-expand body))]
		[nlet-exp (name vars vals bodies)
		  (letrec-exp (cons name vars) (map syntax-expand (cons (lambda-exp (list vars) (syntax-expand bodies)) vals)) (app-exp (cons (var-exp name) (map syntax-expand vals))))]	
		[else expr])))
	   
(define let-convert
  (lambda (vars vals r)
    (if (null? vars)
	r
	(let-convert (cdr vars) (cdr vals) (let-exp (list (car vars)) (list (car vals)) r)))))

(define and-convert
  (lambda (bodies r)
    (if (null? bodies)
	r
	(and-convert (cdr bodies) (if-exp (car bodies) (list r (lit-exp #f)))))))

(define or-convert
  (lambda (bodies r)
    (if (null? bodies)
	r
	(or-convert (cdr bodies) (if-exp (car bodies) (list (car bodies) r))))))
