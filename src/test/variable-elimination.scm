
(import
  (chicken sort)
  (chicken time)
  unittest aux hansei wolfram srfi-1)

(define-wolfram W (->MathML 'MathML))

; setting some parameters
(op/plus (λ args (W `(Simplify (Plus ,@args)))))
(op/subtract (λ args (W `(Simplify (Subtract ,@args)))))
(op/times (λ args (W `(Simplify (Times ,@args)))))
(op/divide (λ args (W `(Simplify (Divide ,@args)))))
(op/greater (λ (a b) (> (W `(LeafCount ,a)) (W `(LeafCount ,b)))))

(define expected '(((V #f)
		    (Plus (Power (Plus -1 p) 10)
			  (Times (Power p 2)
				 (Plus 45
				       (Times -360 p)
				       (Times 1470 (Power p 2))
				       (Times -3780 (Power p 3))
				       (Times 6510 (Power p 4))
				       (Times -7560 (Power p 5))
				       (Times 5715 (Power p 6))
				       (Times -2550 (Power p 7))
				       (Times 511 (Power p 8))))))
		   ((V #t)
		    (Times 2
			   p
			   (Plus 5
				 (Times -45 p)
				 (Times 240 (Power p 2))
				 (Times -840 (Power p 3))
				 (Times 2016 (Power p 4))
				 (Times -3360 (Power p 5))
				 (Times 3840 (Power p 6))
				 (Times -2880 (Power p 7))
				 (Times 1280 (Power p 8))
				 (Times -256 (Power p 9)))))))

(define expanded-expected '(((V #f)
			     (Plus 1
				   (Times -10 p)
				   (Times 90 (Power p 2))
				   (Times -480 (Power p 3))
				   (Times 1680 (Power p 4))
				   (Times -4032 (Power p 5))
				   (Times 6720 (Power p 6))
				   (Times -7680 (Power p 7))
				   (Times 5760 (Power p 8))
				   (Times -2560 (Power p 9))
				   (Times 512 (Power p 10))))
			    ((V #t)
			     (Times 2
				    p
				    (Plus 5
					  (Times -45 p)
					  (Times 240 (Power p 2))
					  (Times -840 (Power p 3))
					  (Times 2016 (Power p 4))
					  (Times -3360 (Power p 5))
					  (Times 3840 (Power p 6))
					  (Times -2880 (Power p 7))
					  (Times 1280 (Power p 8))
					  (Times -256 (Power p 9)))))))

(define-suite hansei-ve-suite

  ((doc r) `((structure/section "Variable elimination optimization")
	     (p "This trick transform a stochastic function " (code/inline "a -> b") " to a generally faster function:")
	     (code/lang ocaml "let variable_elim f arg = reflect (exact_reify (fun () -> f arg))")
	     (p "The probability of " (i "tail") " is:")
	     (container (escape ,(->MathML `(Expand ,(second (first expected))) rule/MathML/display/block)))))

  ((test/exponential _)

   (define result (probcc-reify/exact 
		    (let loop ((p 'p) (n 10))
		      (cond
			((equal? 1 n) (probcc-coin p))
			(else (let1 (r (loop p (sub1 n)))
				    (not (equal? (probcc-coin p) r))))))))

   (⊦= expected result))

  ((test/var-elimination _)

   (define (flipxor-model p)
     (letrec ((loop (λ (n)
		       (cond
			 ((equal? 1 n) (probcc-coin p))
			 (else (let1 (r ((probcc-variable-elimination loop) (sub1 n)))
				     (not (equal? (probcc-coin p) r))))))))
       loop))

   (define res (probcc-reify/exact ((flipxor-model 'p) 10)))
   (⊦= expanded-expected res))

  ((test/bucket _)

   (define (flipxor-model p)
     (letrec ((loop (λ-probcc-bucket (n)
                                      (cond
                                        ((equal? 1 n) (probcc-coin p))
                                        (else (let1 (r (loop (sub1 n)))
						    (not (equal? (probcc-coin p) r))))))))
       loop))

   (define res (probcc-reify/exact ((flipxor-model 'p) 10)))
   (⊦= expanded-expected res))

  )

(unittest/✓ hansei-ve-suite)








