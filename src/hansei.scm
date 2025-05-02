
; use parameters to generalize constants and arithmetic operations

(module hansei *

  (import scheme 
   (chicken base) 
   (chicken continuation) 
   (chicken pretty-print) 
   (chicken fixnum) 
   (chicken sort) 
   srfi-69
   aux)

  (define-syntax probcc-τ (syntax-rules () ((_ p body ...) `((C ,(τ body ...)) ,p))))
  (define (probcc-value p v) `((V ,v) ,p))

  (define-syntax let1/probccpair
    (syntax-rules () 
      ((_ ((slot p) probpairexpr) body ...) (let* ((probpair probpairexpr)
						   (slot (car probpair))
						   (p (cadr probpair)))
					      body ...))))

  (define-syntax cond/probccslot
   (syntax-rules (V C)
    ((_ slotexpr 
      ((V v) vbody ...)
      ((C t) cbody ...))
     (let* ((slot slotexpr)
	    (flag (car slot))
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
                    (else (let1/probccpair ((slot pt) (car choices)) 
                           (let ((p*pt (* p pt))
                                 (rest (cdr choices)))
                            (cond/probccslot slot
                             ((V v) (hash-table-update!/default ans v (λ (w) (+ w p*pt)) 0)
                                    (loop p depth down rest susp))
                             ((C t) (cond 
                                     (down (loop p depth down rest
                                            (loop p*pt (add1 depth) (< depth maxdepth) (t) susp)))
                                     (else (let1 (s (cons (probcc-τ p*pt (t)) susp))
                                            (loop p depth down rest s)))))))))))))
    (let* ((susp (loop 1 0 #t choices '()))
           (f (λ (v p l) (cons (probcc-value p v) l)))
           (folded (hash-table-fold ans f susp)))
     (sort folded (λ (a b) (> (cadr a) (cadr b)))))))

  (define (probcc-next-value choices)
    (cond
      ((null? choices) '())
      (else (let1/probccpair ((slot pt) (car choices)) 
			     (cond/probccslot slot
					      ((V v) (probcc-value pt v))
					      ((C t) (probcc-next-value
						       (append (cdr choices)
							       (map (λ (pair)
								       (let1/probccpair ((slot p) pair)
											`(,slot ,(* p pt))))
								    (t))))))))))

  (define (probcc-normalize choices)
    (let* ((tot (foldr (λ (each t) (+ t (cadr each))) 0 choices))
           (N (λ (each) (list (car each) (exact->inexact (/ (cadr each) tot))))))
      (map N choices)))

  (define (probcc-distribution distribution)
    (letcc/shift k
		 (map (λ (pair)
			 (letcar&cdr (((v p) pair))
				     (probcc-τ (car p) (k v))))
		      distribution)))

  (define (probcc-reflect choices)
    (letcc/shift k
		 (letrec ((make-choices (λ (pv) (map f pv)))
			  (f (λ (probpair)
				(let1/probccpair ((slot p) probpair)
						 (cond/probccslot slot
								  ((V v) (probcc-τ p (k v)))
								  ((C t) (probcc-τ p (make-choices (t)))))))))
		   (make-choices choices))))

  ; Events and random variables.
  (define (probcc-impossible) (probcc-distribution '()))
  (define (probcc-unit v) (list (probcc-value 1 v)))
  (define (probcc-bernoulli t f p) (probcc-distribution `((,t ,p) (,f ,(- 1 p)))))
  (define (probcc-coin p) (probcc-bernoulli #t #f p))

  (define-syntax probcc-when
   (syntax-rules ()
    ((_ test body ...) (cond 
			 (test body ...) 
			 (else (probcc-impossible))))))

  (define (probcc-reify/0 model) (resetcc (probcc-unit (model))))
  (define ((probcc-reify model) depth) (probcc-explore depth (probcc-reify/0 model)))
  (define (probcc-reify/exact model) ((probcc-reify model) +inf.0))

  (define-syntax λ-probcc-bucket
   (syntax-rules ()
    ((_ args body ...) (letrec ((f (λ args body ...))
                                (bucket (λ-memo bargs 
						(probcc-reify/exact 
						  (τ (apply f bargs))))))
                        (o probcc-reflect bucket)))))

  (define (probcc-leaves choices)
    (letrec ((L (λ (choices count)
                 (let1 (F (λ (probpair acc) 
                           (let1/probccpair ((slot p) probpair)
                            (cond/probccslot slot
                            ((V v) (add1 acc))
                            ((C t) (L (t) acc))))))
                  (foldr F count choices)))))
     (L choices 0)))

)
