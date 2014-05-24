; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define prim-proc-names '(+ - * / list-tail car cdr add1 sub1 zero? not = < > >= <= eqv? cons list null? eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr map apply assq assv append max display newline quotient))

(define prim
    (lambda (x)
          (cons x (prim-proc x))))


(define global-env
  (map prim prim-proc-names))

(define scheme-value? (lambda (v) #t))


(define empty-env
  (lambda ()
    '()))


(define extend-env
  (lambda (syms vals env)
    (cons (cons syms (list->vector vals)) env)))

(define extend-env-letrec
  (lambda (syms vals env cont)
    (let* ([len (length syms)]
	   [vec (list->vector vals)]
	   [new-env (cons (cons syms vec) env)])
      (for-each (lambda (item pos)
		  (if (proc? item)
		  (vector-set! vec
			       pos
				   (cases proc item
					  [closure (ids bodies toss-env)
						   (closure ids bodies new-env)]
					  [prim-proc (id)
						     item]
					  [acontinuation (cont) '()		 ]))))
		vals
		(indexes (- len 1) '()))
      (apply-k cont new-env))))

(define indexes
  (lambda (n accu)
    (if (= n 0)
	(cons 0 accu)
	(indexes (- n 1) (cons n accu)))))

(define extend-env-recur
	(lambda (vars vals env cont)
		(cond [(null? vars) env]
			  [else (extend-env-recur (cdr vars) (cdr vals) (extend-env (list (car vars)) (list (eval-exp (car vals) cont env)) env) cont)])))	  



(define apply-env
  (lambda (env sym)
    (cond [(null? env) (apply-global-env sym)]
	   [else (let ((pos (find-position sym (caar env) 0)))
				  (if (number? pos)
				      (let ([return (vector-ref (cdar env) pos)])
						(if (not (ref-param? return))
							return
						(cases ref-param return
							[ref-p (var env2) (apply-env env2 var)])))
				      (apply-env (cdr env) sym)))])))

(define extend-global-env
  (lambda (sym val)
    (set! global-env (cons (cons sym val) global-env))))


(define apply-global-env
  (lambda (sym)
    (let ((found (find-global sym global-env)))
      (if (null? found) (eopl:error 'apply-global-env "~s is not bound" sym)
	  found))))

(define find-global
  (lambda (sym env)
    (cond [(null? env) '()]
	  [(eqv? (caar env) sym) (cdar env)]
	  [else (find-global sym (cdr env))])))

(define change-env
  (lambda (env sym val)
    (if (null? env)
	(extend-global-env sym val)
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [next-env (cdr env)])
	  (let ((pos (find-position sym syms 0)))
	    (if (number? pos)
			(let ([return (vector-ref vals pos)])
					(if(not(ref-param? return))
						(vector-set! vals pos val)
						(cases ref-param return
							[ref-p (var env2)
								(change-env env2 var val)])))
		(change-env next-env sym val)))))))

; (define change-env-strong
;   (lambda (env sym val)
;     (if (null? env)
; 	'() ;;;error
; 	(let ([syms (caar env)]
; 	      [vals (cdar env)]
; 	      [next-env (cdr env)])
; 	  (let ((pos (find-position sym syms 0)))
; 	    (if (number? pos)
; 		(vector-set! vals pos val)
; 		(set! env (extend-env-letrec (list sym) (list val) env))))))))

(define find-position
  (lambda (sym ls pos)
    (cond [(null? ls) #f]
	  [(eq? sym (car ls)) pos]
	  [else (find-position sym (cdr ls) (+ pos 1))])))

(define make-init-env
  (lambda ()
    (map prim prim-proc-names)))

(define reset-global-env
  (lambda ()
    (set! global-env (make-init-env))))

(define extend-env-new
  (lambda (vars args env newvars newargs)
    (cond [(symbol? vars) (extend-env (append newvars (list vars)) (append newargs (list args)) env)]
	  [(null? vars) (extend-env newvars newargs env)]
	  [else (extend-env-new (cdr vars) (cdr args) env (append newvars (list (car vars))) (append newargs (list (car args))))])))


; reference helper
(define reference-help-cps
	(lambda (vars body env2 operands env cont)
		(find-ref-next-level vars operands env '() (refHelp-cont vars body env2 cont))))
			  
(define find-ref-next-level
	(lambda (vars ops env accu cont)
		(cond [(null? vars) (apply-k cont accu)]
			  [(list? (car vars)) (append-cps  accu (list (ref-p (cadar ops) env)) (ref-ref-ref-cont (cdr vars) (cdr ops) env cont))]
			  [else (eval-exp (car ops) (ref-ref-ref2-cont accu env (cdr ops) (cdr vars) cont) env)])))
			  
(define de-ref
	(lambda (vars accu cont)
		(cond [(null? vars) (apply-k cont accu)]
			  [(list? (car vars)) (append-cps accu (list (cadar vars)) (de-ref-cont (cdr vars) cont))] 	
			  [else (append-cps accu (list (car vars)) (de-ref-cont (cdr vars) cont))])))