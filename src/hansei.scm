
(module hansei *

  (import scheme 
   (chicken base) 
   (chicken continuation) 
   (chicken pretty-print) 
   (chicken fixnum) 
   (chicken sort) 
   srfi-69
   aux)

  (define-syntax probcc-pair-C (syntax-rules () ((_ p body ...) `((C ,(τ body ...)) ,p))))
  (define (probcc-pair-V p v) `((V ,v) ,p))

  (define-syntax letprobccpair
   (syntax-rules () 
    ((_ (((flag payload) p) probpair) body ...) (let* ((slot (car probpair))
                                                       (p (cadr probpair))
                                                       (flag (car slot))
                                                       (payload (cadr slot)))
                                                 body ...))
    ((_ ((slot p) probpair) body ...) (let* ((slot (car probpair))
                                             (p (cadr probpair)))
                                       body ...))))

  (define-syntax cond-probccslot
   (syntax-rules (V C)
    ((_ slot 
      ((V v) vbody ...)
      ((C t) cbody ...))
     (let ((flag (car slot))
           (payload (cadr slot)))
      (cond
       ((equal? flag 'V) (let1 (v payload) vbody ...))
       ((equal? flag 'C) (let1 (t payload) cbody ...))
       (else (error `(not a probability slot ,slot))))))))

  (define (probcc-explore maxdepth choices)
   (letrec ((ans (make-hash-table))
            (loop (λ (p depth down choices susp)
                   (cond
                    ((null? choices) susp)
                    (else (letprobccpair ((slot pt) (car choices)) 
                           (let* ((p*pt (* p pt))
                                  (rest (cdr choices)))
                            (cond-probccslot slot
                             ((V v) (hash-table-update!/default ans v (λ (w) (+ w p*pt)) 0)
                                    (loop p depth down rest susp))
                             ((C t) (cond 
                                     (down (loop p depth down rest
                                            (loop p*pt (add1 depth) (< depth maxdepth) (t) susp)))
                                     (else (let1 (s (cons (probcc-pair-C p*pt (t)) susp)) 
                                            (loop p depth down rest s)))))))))))))
    (let* ((susp (loop 1 0 #t choices '()))
           (f (λ (v p l) (cons (probcc-pair-V p v) l)))
           (folded (hash-table-fold ans f susp)))
     (sort folded (λ (a b) (> (cadr a) (cadr b)))))))

  (define (probcc-normalize choices)
    (let* ((tot (foldr (λ (each t) (+ t (cadr each))) 0 choices))
           (N (λ (each) (list (car each) (exact->inexact (/ (cadr each) tot))))))
      (map N choices)))

  (define ((probcc-distribution/k distribution) k)
    (map (λ (pair)
          (letcar&cdr (((v p) pair))
           (probcc-pair-C (car p) (k v))))
         distribution))
  (define probcc-distribution (o callshiftcc probcc-distribution/k))

  (define ((probcc-reflect/k choices) k)
    (letrec ((make-choices (λ (pv) (map f pv)))
             (f (λ (probpair)
                 (letprobccpair ((slot p) probpair)
                  (cond-probccslot slot
                   ((V v) (probcc-pair-C p (k v)))
                   ((C t) (probcc-pair-C p (make-choices (t)))))))))
     (make-choices choices)))
  (define probcc-reflect (o callshiftcc probcc-reflect/k))

  (define (probcc-unit v) (list (probcc-pair-V 1 v)))
  (define (probcc-reify0 m) (resetcc (probcc-unit (m))))
  (define (probcc-bernoulli t f p) (probcc-distribution `((,t ,p) (,f ,(- 1 p)))))
  (define (probcc-coin p) (probcc-bernoulli #t #f p))
  (define (probcc-impossible) (probcc-distribution '()))
  (define-syntax probcc-when
   (syntax-rules ()
    ((_ test body ...) (cond (test body ...) (else (probcc-impossible))))))
  (define-syntax probcc-model
   (syntax-rules ()
    ((_ body ...) (probcc-reify0 (τ body ...)))))
  (define-syntax probcc-inference-exact
   (syntax-rules ()
    ((_ body ...) (probcc-explore +inf.0 (probcc-model body ...)))))
  (define-syntax λ-probcc-bucket
   (syntax-rules ()
    ((_ args body ...) (letrec ((f (λ args body ...))
                                (bucket (λ-memo bargs (probcc-inference-exact (apply f bargs)))))
                        (o probcc-reflect bucket)))))

  (define (probcc-leaves choices)
    (letrec ((L (λ (choices count)
                 (let1 (F (λ (probpair acc) 
                           (letprobccpair ((slot p) probpair)
                            (cond-probccslot slot
                            ((V v) (add1 acc))
                            ((C t) (L (t) acc))))))
                  (foldr F count choices)))))
     (L choices 0)))

)