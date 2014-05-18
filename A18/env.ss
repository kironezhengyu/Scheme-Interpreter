; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define (apply-env-ref env var)
  (cases environment env
    (empty-env-record () 'nah)
    (extended-env-record (syms vals env)
      (let ((pos (list-find-position var syms)))
        ; (display var)
        ; (display 'pos)
        ; (display pos)
        ; (display 'syms)
        ; (display syms)
        (if (number? pos) (list-ref vals pos) 'nah)))
    (recursively-extended-env-record (vars vals bodies old-env) 'nah)
    ))

(define (deref ref)
  (unbox ref))

(define (set-ref! ref value)
  (set-box! ref value))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))




(define reset-global-env
  (lambda ()
    (set! global-env   (map (lambda (name)
   (cons name (list (primitive name))))
       *prim-proc-names*))))



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


(define global-env
  (map (lambda (name)
   (cons name (list (primitive name))))
       *prim-proc-names*))


(define empty-env
    (lambda ()
          '()))


(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals ext-env)
        (let [(result (apply-env-ref env sym))]
          (if (and (not (equal? result 'nah)) (not(equal? result (void)))) 
            (succeed  result)
	          (apply-env ext-env sym succeed fail))))
      (recursively-extended-env-record (vars vals bodies old-env)
        (let ((pos (list-find-position sym vars)))
          (if (number? pos)
            (succeed (closure (list-ref vals pos)
              (list-ref bodies pos) 
              env))
            (apply-env old-env sym succeed fail)))))))

(define extend-env-recursively
  (lambda (vars vals bodies env)
    (recursively-extended-env-record
      vars vals bodies env)))
(define reset-global-env
  (lambda ()
    (set! init-env 
    (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))))


