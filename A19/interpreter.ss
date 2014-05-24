
; top-level-eval evaluates a form in the global environment

;tail positions
; and — last expression
; begin — last expression
; case — last expression in each clause
; cond — last expression in each clause, and the call to a => procedure is a tail call
; do — last result expression
; if — “true” and “false” leg expressions
; lambda — last expression in body
; let, let*, letrec, let-syntax, letrec-syntax — last expression in body
; or — last expression


; tail positioned eval-exp
(define eval-expressions-cps
  (lambda (exps cont env)
    (if (null? exps)
      (apply-k cont '())
      (eval-exp (car exps) (eval-exps-cont (cdr exps) cont env) env))))      




;cps-version
(define eval-exp
  (lambda (exp cont env)
    (cases expression exp
	   [var-exp (id) (apply-k cont (apply-env env id))]
	   [lit-exp (id) (apply-k cont id)]
	   [exit-exp (args) 
      (eval-expressions-cps args (exit-cont) env)]
	  [call/cc-exp (consumer)
    ;  (display "consumer") (newline)
    ;  (pretty-print consumer)
    ; (newline)
    ; (display "cont")(newline)
    ; (pretty-print cont)
      (eval-exp consumer (call-cont cont) env)
     ]

   ; [call/cc-exp (consumer)
   ;  (eval-exp consumer 
   ;          (apply-k cont (closure  bodies env))
   ;             (lambda (f)
   ;              (display '!!!!!!f)
   ;              (display f)
   ;               (f (lambda (v k^) (cont v))))
   ;             env

   ;             )
   ;   ]
	   [begin-exp (bodies)
		    (eval-expressions-cps bodies (begin-cont cont) env)]
	   [if-exp (condition if-else) 
			(eval-exp condition (if-cont if-else cont env) env)]
	   [cond-exp (bodies)
		     (verify-in-order bodies env cont)]
	   [lambda-exp (vars bodies)
		       (apply-k cont (closure (car vars) bodies env))]
	   [app-exp (exps)
			(eval-exp (car exps) (app1-cont exps env cont) env)]
	   [letrec-exp (vars vals bodies)
	           (eval-expressions-cps vals (letrec-cont vars bodies env cont) env)]
	   [set-exp (var val)
		    (eval-exp val (set-cont env var cont) env)]

	   [case-exp (id cases)
			(eval-case id cases env cont)]
	   [while-exp (test-exp bodies)
		      (whileHelp test-exp bodies env cont)]
	   [define-exp (sym val)
		  (if (null? env)
		      (eval-exp val (extend-global-env-cont sym cont) env)
		      (eval-exp (expand-syntax val) (set-cont env sym cont) env) )]
	   [break-exp (bodies)
		      (eval-expressions-cps bodies init-cont env)]
	   [else (eopl:error 'eval-exp "Unknown exp ~s" exp)]
		     )))
			  
			 
(define append-cps
  (lambda (L1 L2 K)
    (if (null? L1)
	(apply-k K L2)
	(append-cps (cdr L1) L2 (cons-cont (car L1) K)))))
			 
(define whileHelp
	(lambda (testExp bodies env cont)
		(eval-exp testExp (while-cont testExp bodies env cont) env)))

(define verify-in-order
  (lambda (x e cont)
    (cond [(null? x) (apply-k cont (void))]
	  [(equal? (caar x) '(var-exp else)) (eval-exp (cadar x) cont e)]
	  [else (eval-exp (caar x) (cond-cont (cadar x) cont e (cdr x)) e)])))

(define eval-case
	(lambda (id cases env cont)
		(cond [(null? cases) (eopl:error 'eval-case "No match in cases")]
			  [(equal? (caar cases) '(var-exp else)) (eval-exp (cadar cases) cont env)]
			  [else (eval-help id (caar cases) env (case-cont (cadar cases) env id (cdr cases) cont))])))
			  
(define eval-help
	(lambda (id cases env cont)
		(cond [(null? cases) (apply-k cont #f)]
		      [(eval-exp id (case-ref-cont (car cases) env id (cdr cases) cont) env)])))
			  
(define init-cont (halt-cont))
	  

(define eval-exp-env
  (lambda (e)
    (lambda (x)
      (eval-exp x e))))

	
(define apply-proc
  (lambda (proc-value args cont)
    (cases proc proc-value
	   [closure (vars body env)
		    (eval-exp body cont (extend-env-new vars args env '() '()))]
	   [prim-proc (id)
		      (apply-k cont (apply-prim-proc id args cont))]
		 [acontinuation (cont)
			  (apply-k cont (car args))]
     [else (error 'apply-proc
                     "Attempt to apply bad procedure: ~s" 
                      proc-value)]

        )))

(define length-cps
  (lambda (ls k)
     (cond [(null? ls) (apply-k k 0)]
	   [else (length-cps (cdr ls) (add-cont 1 k))])))
	   
(define-datatype ref-param ref-param?
  [ref-p
  (var symbol?)
  (env list?)])
	
(define apply-prim-proc ;;Check to make sure procedure? is legal and in cps
  (lambda (id args cont)
    (case id
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?)(zero? (1st args))]
      [(/)(apply / args)]
      [(not) (not (1st args))]
      [(list) args]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(>=)(>= (1st args)(2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(quotient) (quotient (1st args) (2nd args))]
      [(eqv?) (eqv? (1st args)(2nd args))]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length)(length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (if (continuation? args) #t (proc? args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (list->vector args)]
      [(make-vector) (if (null? (cdr args))
       (make-vector (1st args))
       (make-vector (1st args) (2nd args)))]
      [(list-tail) (list-tail (1st args) (2nd args))]
      [(member) (member (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (cadr args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(sqrt) (sqrt (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline)]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cddr) (cddr (1st args))]
      [(cdar) (cdar (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(caddr) (caddr (1st args))]
      [(cadar) (cadar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(append)(apply append args)]
      [(else) #t]
       [(map) (map (lambda (x) (apply-proc (car args) (list x) cont)) (cadr args))]
      [(apply) (apply-proc (car args) (cadr args) cont)]
      [(void) (void)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            args)])))


;debug mode	 
(define display eopl:pretty-print)
(define (sp exp) (syntax-expand (parse-exp exp)))
