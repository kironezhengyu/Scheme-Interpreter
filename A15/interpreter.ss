; top-level-eval evaluates a form in the global environment
(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(/)(apply / args)]
      [(not) (not (1st args))]
      [(list)  (car (list args))]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(>=)(>= (1st args)(2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (cdr args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (vector (1st args))]
      [(make-vector) (if (null? (cdr args))
       (make-vector (1st args))
       (make-vector (1st args) (2nd args)))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
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
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            args)])))



      


(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter
(define parse-args
    (lambda (n ls)
        (if (= n 0)
            (cons #t ls)
            (let ([t (parse-args (- n 1) (cdr ls))])
                    (if (eq? (car t) #t)
                        (cons (list (car ls)) (list (cdr t)))
                        (cons (cons (car ls) (car t)) (cdr t)))))))



(define eval-begin
  (lambda (exp env )
    (let loop([exp exp])
        (if (null? (cdr exp))
        (eval-exp (car exp) env)
        (loop (cdr exp))))))
;add local enviroment
(define eval-exp
  (lambda (exp env)
    ; (display "i am top " )(display exp) (newline)
    (cases expression exp
      [lit-exp (datum) datum]
      [begin-exp (exps)
          (eval-begin exps env)]  
      [var-exp (id)
				(apply-env env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (apply-env init-env id
                          (lambda (x) x)
                          (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
              "variable not found in environment: ~s" id )))))]

      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]

      [let-exp (vars exps bodies)
          (let ([new-env (extend-env (map cadr vars)  
                            (map (lambda (x) (eval-exp x env)) exps) env)])
          (let loop ([bodies bodies])
                (if (null? (cdr bodies))
                  (eval-exp (car bodies) new-env)
                  (begin (eval-exp (car bodies) new-env )
                    (loop (cdr bodies))))))]
      [if-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env)
          (eval-exp then-exp env)
          (eval-exp else-exp env))]
      ;todos
      [no-else-if-exp (test-exp then-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env))]
      [lambda-exp (params body)
        (closure params body env)]

      [no-parens-lambda-exp (params body)
        (informal-closure params body env)
      ]
      [improper-lambda-exp (params rest body)
        (improper-closure params rest body env)]


      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands  putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (ran) (eval-exp ran env)) rands)))

;  Apply a procedure to its arguments.
;  At this point  we only have primitive procedures.  
;  User-defined procedures will be added later.



(define apply-proc
  (lambda (proc-value args)
    (cond 
      [(proc-val? proc-value)
        (cases proc-val proc-value
        [prim-proc (op) (apply-prim-proc op args)]
  			[closure (vars bodies env) (eval-exp   bodies (extend-env  vars  args env))]
        [informal-closure (vars bodies env)  (eval-exp bodies (extend-env  (list vars)  (list args) env)) ]
        [improper-closure (params rest body env) 
        (let* ([parsed-args (parse-args (length params) args)]
                             [defined (car parsed-args)]
                             [other (cadr parsed-args)])
                      (eval-exp  body (extend-env (list rest) (list other) (extend-env params defined env))))]
        )]
        
      [(list? proc-value)
        (letrec ([helper (lambda (ls)
            (if (null? (cdr ls))
                    (apply-proc (car ls) args)
                    (begin (apply-proc (car ls) args) (helper (cdr ls)))))]) (helper proc-value))]

      [else (error 'apply-proc
                     "Attempt to apply bad procedure: ~s" 
                      proc-value)]
    )))





(define *prim-proc-names* '(+ - *   /  add1  sub1  zero?  not  = 
			      <  >  >= cons  car  cdr  list  null?  assq  eq?  equal?  atom?  length 
			      list->vector  list?  pair?  procedure?  vector->list  vector  make-vector  vetor-ref  vector?  number?  symbol?  set-car!   set-cdr!  vector-set!   display   newline  
			      cadr  cddr  cdar  caar  caaar  caadr  caddr  cadar  cdddr  cddar  cdaar  cdadr))

(define init-env         ; for now  our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.



(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive  so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))




(define display pretty-print)




