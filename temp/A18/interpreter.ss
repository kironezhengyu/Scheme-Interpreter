; top-level-eval evaluates a form in the global environment



      

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

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
        (begin (eval-exp (car exp) env) (loop (cdr exp)))))))
;add local enviroment
(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [begin-exp (exps)
          (eval-begin exps env)]  
      [var-exp (id)
        (let [(result (apply-env env id; look up its value.
           (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (apply-env init-env id
                          (lambda (x) x)
                          (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
              "variable not found in environment: ~s" id ))))))]
        (if (box? result) (deref result) result))
        ]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]

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

      [while-exp (test bodies)
           (if (eval-exp test env)
           (begin (eval-exp (begin-exp bodies) env)
            (eval-exp (while-exp test bodies) env)))]
      [letrec-exp (vars vals bodies letrec-bodies)
        ;(display bodies) (newline) 
        (getlast (map (lambda (x) (eval-exp x
                          (extend-env-recursively
                            vars vals bodies env))) letrec-bodies))]
      [define-exp (id exp)
        (let [(result (apply-env-ref env id))]
          (if (equal? result 'nah)
              (set! init-env 
                    (append 
                    (append (list (car init-env) (cons id (2nd init-env))) 
                            (list (cons (box (eval-exp exp env)) (3rd init-env))))
                    (list (cadddr init-env))))  (display "nonono")))]
      [set!-exp (id exp) 
      (set-ref! (apply-env env id; look up its value.
           (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (apply-env init-env id
                          (lambda (x) x)
                          (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
              "variable not found in environment: ~s" id ))))) (eval-exp exp env))]

      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define getlast
  (lambda (exp)
    (if (null? (cdr exp))
      (car exp)
      (getlast (cdr exp)))))




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
        [closure (vars bodies env)(eval-exp   bodies (extend-env  vars  args env))]
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




(define *prim-proc-names* '(+ - *   /  add1  sub1  zero?  not  = sqrt member else quotient eqv? append
            <  >  >= cons  car  cdr  list  null?  assq  eq?  equal?  atom?  length union list-tail
            list->vector  list?  pair?  procedure?  vector->list  vector  make-vector  vector-ref  vector?  number?  symbol?  set-car!   set-cdr!  vector-set!   display   newline  
            cadr  cddr  cdar  caar  caaar  caadr  caddr  cadar  cdddr  cddar  cdaar  cdadr map apply))

(define init-env         ; for now  our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
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
      [(assq) (assq (1st args) (cdr args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length)(length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
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
      [(map) (map (lambda (x) (apply-proc (car args) (list x))) (cadr args))]
      [(apply) (apply-proc (car args) (cadr args))]
      [(void) (void)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            args)])))


(define syntax-expand
  (lambda (exp)
      (cases expression exp
      [define-exp (id exp)
        (define-exp id (syntax-expand exp))]
      [let-exp (vars exps bodies)
        (app-exp (lambda-exp (map cadr vars) (begin-exp (map syntax-expand bodies))) (map syntax-expand exps))]
      [cond-exp (body)
        (let loop([body  body])
          (if (null? (cdr body)) (cadar body)
            (if-exp (caar body) (cadar body) (loop (cdr body)))))]
      [and-exp (body)
        (let loop ([body body])
          (if (null?  (cdr body))
            (if-exp (car body) (car body) (lit-exp #f))
            (if-exp (car body) (loop (cdr body)) (lit-exp #f))))]
      [or-exp (body)
        (let loop ([body body])
          (if (null?  body)
            (lit-exp #f)
            (if-exp (car body) (car body) (loop (cdr body)))))]
      [let*-exp (ids values body) 
        ; (display "id")(display ids) (newline)
        ; (display "values")(display values)(newline)
        ;  (display "body")(display body)(newline)
       (let loop ([ids ids] [values values])
          (if (null? (cdr ids))
            (syntax-expand (let-exp (list (car ids)) (list (car values)) 
                            body))
            (syntax-expand (let-exp (list (car ids)) (list (car values)) 
                            (list (loop (cdr ids) (cdr values)))))))]

      [case-exp (test-value cases)
            (letrec ([helper (lambda (ls)
              (if (null? (cdr ls))
                  (if (and (not (null? (car ls))) (eq? (caar ls) 'else))
                    (no-else-if-exp (app-exp (var-exp 'else) '()) (cadar ls))
                      (no-else-if-exp (app-exp (var-exp 'member)   (list  test-value (lit-exp (caar ls))))
                      (cadar ls)))
                      
              (if-exp (app-exp (var-exp 'member) (list test-value (lit-exp (caar ls))))
              (cadar ls)
              (helper (cdr ls)))))])
            (helper cases))] 
      [while-exp (test-value bodies)
            (while-exp (syntax-expand test-value) (map syntax-expand bodies))]
      [named-let-exp (name ids values body)
        (app-exp (letrec-exp (list name) (list ids) (map syntax-expand (list body)) (list (var-exp name))) values)]
      [if-exp (test-exp then-exp else-exp)
      (if-exp test-exp then-exp (syntax-expand else-exp))]

      [letrec-exp (vars vals bodies letrec-bodies)
          (letrec-exp vars vals (map syntax-expand bodies) (map syntax-expand letrec-bodies))]      
    [else exp])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand(parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive  so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand(parse-exp x)))))




(define display pretty-print)



