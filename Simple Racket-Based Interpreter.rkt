#lang racket

(provide parse
         desugar
         eval
         load-defs
         repl)


;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (fn arg) #:transparent)

;; boolean value
(struct bool-exp (val) #:transparent)

;; if expression
(struct if-exp (bool true false) #:transparent)

;; relational expression
(struct rel-exp (rel lhs rhs) #:transparent)

;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; boolean literal
    [(? boolean?)
     (bool-exp sexp)]

    ;; arithmetic expression
    [(list (and op (or '+ '*)) lhs rhs)
     (arith-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; relational expression
    [(list (and op (or '= '<)) lhs rhs)
     (rel-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; identifier (variable)
    [(? symbol?)
     (var-exp sexp)]

    ;; if expressions
    [(list 'if bool true false)
     (if-exp (parse bool) (parse true) (parse false))]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]

    ;; lambda expression -- modified for > 1 params
    [(list 'lambda (list ids ...) body)
     (lambda-exp ids (parse body))]

    ;; function application -- modified for > 1 args
    [(list f args ...)
     (app-exp (parse f) (map parse args))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Desugar-er -- i.e., syntax transformer
(define (desugar exp)
  (match exp
    ((arith-exp op lhs rhs)
     (arith-exp op (desugar lhs) (desugar rhs)))

    ((rel-exp op lhs rhs)
     (rel-exp op (desugar lhs) (desugar rhs)))

    ((if-exp bool true false)
     (if-exp (desugar bool) (desugar true) (desugar false)))
    
    ((let-exp ids vals body)
     (let-exp ids (map desugar vals) (desugar body)))
    
    ((lambda-exp ids body)
     (foldr (lambda (id lexp) (lambda-exp id lexp))
            (desugar body)
            ids))

    ((app-exp (var-exp 'cond) (list (app-exp bools exps)...))
     (foldl (lambda (true bool false) (if-exp bool (first true) false))
            (desugar (first (last exps)))
            (map desugar (rest (reverse exps)))
            (map desugar (rest (reverse bools)))))
            

    ((app-exp (var-exp '-) (list lhs rhs))
     (arith-exp "+" (desugar lhs) (arith-exp "*" (int-exp -1) (desugar rhs))))

    ((app-exp (var-exp '>) (list lhs rhs))
     (rel-exp "<" (desugar rhs) (desugar lhs)))

    ((app-exp (var-exp '<=) (list lhs rhs))
     (desugar
      (app-exp (var-exp 'or) (list
                              (rel-exp "<" (desugar lhs) (desugar rhs))
                              (rel-exp "=" (desugar lhs) (desugar rhs))))))

    ((app-exp (var-exp '>=) (list lhs rhs))
     (desugar
      (app-exp (var-exp 'or) (list
                              (app-exp (var-exp '>) (map desugar (list lhs rhs)))
                              (rel-exp "=" (desugar lhs) (desugar rhs))))))

    ((app-exp (var-exp 'and) args)
     (foldl (lambda (bool true [false (bool-exp #f)]) (if-exp bool true false))
            (desugar (first args))
            (map desugar (rest args))))

    ((app-exp (var-exp 'or) args)
     (foldl (lambda (bool false [true (bool-exp #t)]) (if-exp bool true false))
            (desugar (first args))
            (map desugar (rest args))))

    ((app-exp f args)
     (foldl (lambda (id fexp) (app-exp fexp id))
            (desugar f)
            (map desugar args)))
    
    (_ exp)))


;; function value + closure
(struct fun-val (id body env) #:prefab)

;; Interpreter
(define (eval expr [env '()])
  (match expr
    ;; int literal
    [(int-exp val) val]

    ;; bool literal
    [(bool-exp val) val]

    ;; arithmetic expression
    [(arith-exp "+" lhs rhs)
      (+ (eval lhs env) (eval rhs env))]
    [(arith-exp "*" lhs rhs)
     (* (eval lhs env) (eval rhs env))]

    ;; relational expression
    [(rel-exp "=" lhs rhs)
     (= (eval lhs env) (eval rhs env))]
    [(rel-exp "<" lhs rhs)
     (< (eval lhs env) (eval rhs env))]
          
    ;; variable binding
    [(var-exp id)
     (let ([pair (assoc id env)])
       (if pair (cdr pair) (error (format "~a not bound!" id))))]

    ;; if expression
    [(if-exp bool true false)
     (if (eval bool env)
         (eval true env)
         (eval false env))]

    ;; let expression
    [(let-exp (list (var-exp id) ...) (list val ...) body)
     (let ([vars (map cons id
                      (map (lambda (v) (eval v env)) val))])
       (eval body (append vars env)))]

    ;; lambda expression
    [(lambda-exp id body)
     (fun-val id body env)]

    ;; function application
    [(app-exp f arg)
     (match-let ([(fun-val id body clenv) (eval f env)]
                 [arg-val (eval arg env)])
       (eval body (cons (cons id arg-val) clenv)))]

    ;; basic error handling
    [_ (error (format "Can't evaluate: ~a" expr))]))


;; load definitions (returning env)
(define (load-defs filename)
  (let* ([sexps (file->list filename)]
         [ph (make-placeholder '())]
         [env (let rec ([sexps sexps]
                        [acc '()])
                (if (empty? sexps)
                    acc
                    (rec (rest sexps)
                      (cons
                       (cons
                        ((compose first second) (first sexps)) ; fn-name
                        (fun-val ((compose second second) (first sexps)) ; id
                                 (desugar (parse (third (first sexps)))) ; body
                                 ph)) acc))))]) ; env
    (placeholder-set! ph env)
    (make-reader-graph env)))

;; REPL
(define (repl [filename #f])
  (let loop ([env (if filename (load-defs filename) '())])
    (let ([stx (desugar (parse (read)))])
      (when stx
        (println (eval stx env))
        (loop env)))))