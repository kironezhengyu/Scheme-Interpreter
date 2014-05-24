(define-datatype continuation continuation?
  (halt-cont)
  (cons-cont
   (v scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))  
  (eval-exps-cont
   (ops (list-of expression?))
   (cont continuation?)
   (env scheme-value?))
  (if-cont
   (if-else (list-of expression?))
   (cont continuation?)
   (env scheme-value?))
  (if-else-cont
   (value scheme-value?)
   (cont continuation?)
   (env scheme-value?))
  (rep-cont)
  (add-cont
   (value scheme-value?)
   (cont continuation?))
  (begin-cont
   (cont continuation?))
  (while-cont     
   (testExp expression?) 
   (bodies expression?)
   (env list?)
   (cont continuation?))
  (whileHelp-cont 
   (testExp expression?)
   (bodies expression?)
   (env list?) 
   (cont continuation?))
  (set-cont 
   (env list?)
   (var scheme-value?)
   (cont continuation?))
  (extend-global-env-cont 
   (sym scheme-value?)
   (cont continuation?))
  (letrec-cont 
   (vars list?)
   (bodies expression?)
   (env list?)
   (cont continuation?))
  (letrec-help-cont
   (bodies expression?)
   (cont continuation?))	 
  (end-cont
   (cont continuation?))
  (cond-cont
   (true-case expression?)
   (cont continuation?)
   (env list?)
   (false-case list?))
  (case-cont
   (true-case expression?)
   (env list?)
   (id expression?)
   (false-case list?)
   (cont continuation?))
  (case-ref-cont
   (arg expression?)
   (env list?)
   (id expression?)
   (false-part list?)
   (cont continuation?))
  (case-ref2-cont
   (arg scheme-value?)
   (env list?)
   (id expression?)
   (false-part list?)
   (cont continuation?))
  (app1-cont 
   (exps (list-of expression?))
   (env list?)
   (cont continuation?))
  (refHelp-cont
   (vars scheme-value?)
   (body expression?)
   (env2 list?)
   (cont continuation?))
  (ref-ref-cont
   (arg scheme-value?)
   (body expression?)
   (env list?)
   (cont continuation?))
  (ref-ref-ref-cont
   (vars scheme-value?)
   (ops list?)
   (env list?)
   (cont continuation?))
  (ref-ref-ref2-cont
   (accu list?)
   (env list?)
   (ops list?)
   (vars scheme-value?)
   (cont continuation?))
  (de-ref-cont
   (vars list?)
   (cont continuation?))
  (call-cont
   (cont continuation?))
  (rest-cont
   (c continuation?)
   (cont continuation?))
   (append-cont
  (arg list?)
  (cont continuation?))
	 )
   

(define apply-k
  (lambda (cont val)
    ; (display "apply-k") (newline)
    ; (pretty-print cont)
    (cases continuation cont
	   [halt-cont ()		      
		      val]
	   [cons-cont (v k)
		      (apply-k k (cons v val))]
	   [proc-cont (k)
		      (apply-proc (car val) (cdr val) k)]
	   [eval-exps-cont (ls k env)
		      (eval-expressions-cps ls (cons-cont val k) env)]
	   [if-cont (if-else k env)
		    (if val
			(eval-exp (car if-else) k env)
			(length-cps (cdr if-else) (if-else-cont (cdr if-else) k env)))]
		[if-else-cont (value k env)
			(if (= val 1)
				(eval-exp (car value) k env)
				(rep))]
		[rep-cont ()
			  (begin (pretty-print val) (rep))]
		[add-cont (value k)
			(apply-k k (+ val value))]
		[begin-cont (k)
			(if (null? val)
				(apply-k k val)
			(apply-k k (car (reverse val))))]
		[while-cont (testExp bodies env k)
			(if val 
				(eval-exp bodies (whileHelp-cont testExp bodies env k) env)
				(apply-k k '()))]
		[whileHelp-cont (testExp bodies env k)
			(whileHelp testExp bodies env k)]
		[set-cont (env var k)
			(begin
			(change-env env var val)
			(apply-k k (void)))]
		[extend-global-env-cont (sym k)
			(begin
			(extend-global-env sym val)
			(apply-k k (void)))]
		[letrec-cont (vars bodies env k)
			(extend-env-letrec vars val env (letrec-help-cont bodies k))]
		[letrec-help-cont (bodies k)
			(eval-exp bodies k val)]
		[end-cont (k)
				   (if (pair? val)
				       (if (eqv? (car val) 'closure)
					   (apply-k k '<interpreter-procedure>)
					   (apply-k k val))
				       (apply-k k val))]
		[cond-cont (true-case k env false-case)
			   (if val
			       (eval-exp true-case k env)
			       (verify-in-order false-case env k))]
		[case-cont (true-case env id false-case k)
			   (if val
			       (eval-exp true-case k env)
			       (eval-case id false-case env k))]
		[case-ref-cont (arg env id false-part k)
			       (eval-exp arg (case-ref2-cont val env id false-part k) env)]
		[case-ref2-cont (arg env id false-part k)
				(if (equal? val arg)
				    (apply-k k #t)
				    (eval-help id false-part env k))]
		[app1-cont (exps env k)
				(if (proc? val)
					(cases proc val
						[prim-proc (id) (eval-expressions-cps exps (proc-cont k) env)]
						[closure (vars body env2) (if (list? vars)
											  (reference-help-cps vars body env2 (cdr exps) env k)
											  (eval-expressions-cps exps (proc-cont k) env))]
						[acontinuation (cont) (eval-expressions-cps exps (proc-cont k) env)])
					(eval-expressions-cps (cdr exps) (rest-cont val k) env))]
		[refHelp-cont (vars body env2 k)
				(de-ref vars '() (ref-ref-cont val body env2 k))]
		[ref-ref-cont (arg body env k)
				(apply-proc (closure val body env) arg k)]
		[append-cont (v k)
			     (append-cps v val k)]
		[ref-ref-ref-cont (vars ops env k)
				(find-ref-next-level vars ops env val k)]
		[ref-ref-ref2-cont (accu env ops vars k)
				(append-cps accu (list val) (ref-ref-ref-cont vars ops env k))]
		[de-ref-cont (vars k)
				(de-ref vars val k)]
		[call-cont (k)
    ; (display "!!!!!!!!call-val")(newline)
    ; (pretty-print val)
    ; (display "****the k")(newline)
    ; (pretty-print k)
    ; (display "?????current cont")(newline)
    ; (pretty-print cont)
    (cond
				[(proc? val)
        (cases proc val
					[closure (vars body env2) (apply-proc val (list k) k)]
					[prim-proc (id) (apply-prim-proc id k cont)]
					[acontinuation (cont) 
    ; (display "!!!!!!!!call-val")(newline)
    ; (pretty-print val)
    ; (display "****the k")(newline)
    ; (pretty-print k)
    ; (display "?????current cont")(newline)
    ; (pretty-print cont)
					])]
        [(continuation? val)
    ; (display "!!!!!!!!call-val")(newline)
    ; (pretty-print val)
    ; (display "?????current cont")(newline)
    ; (pretty-print cont)
    ; (display "****the k")(newline)
    ; (pretty-print k)
          (apply-k  val k)
         ])]
		[rest-cont (c k)
				(apply-proc (acontinuation c) val k)]
		)))
