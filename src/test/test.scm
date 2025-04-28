
(import unittest aux hansei)

(define-suite hanseitest

  ((doc r) `((structure/section "Introduction")
	     (p "Quoting from " 
                (cite/a "https://okmij.org/ftp/kakuritu/kakuritu.html" 
                        "Embedded domain-specific languages for probabilistic programming")
		":"
                (cite/quote
                  "Oleg Kiselyov"

                  "Broadly speaking, probabilistic programming languages are to express
		  computations with degrees of uncertainty, which comes from the
		  imprecision in input data, lack of the complete knowledge or is
		  inherent in the domain. More precisely, the goal of probabilistic
		  programming languages is to represent and automate reasoning about
		  probabilistic models, which describe uncertain quantities -- random
		  variables -- and relationships among them. The canonical example is the
		  grass model, with three random variables representing the events of
		  rain, of a switched-on sprinkler and wet grass. The (a priori)
		  probabilities of the first two events are judged to be 30% and 50%
		  correspondingly. Probabilities are non-negative real numbers that may
		  be regarded as weights on non-deterministic choices. Rain almost
		  certainly (90%) wets the grass. The sprinkler also makes the grass wet,
		  in 80% of the cases. The grass may also be wet for some other reason.
		  The modeler gives such an unaccounted event 10% of a chance. This model
		  is often depicted as a directed acyclic graph (DAG) -- so-called
		  Bayesian, or belief network -- with nodes representing random variables
		  and edges conditional dependencies. Associated with each node is a
		  distribution (such as Bernoulli distribution: the flip of a biased
				     coin), or a function that computes a distribution
		  from the node's inputs (such as the noisy disjunction nor).
		  The sort of reasoning we wish to perform on the model is finding out
		  the probability distribution of some of its random variables. For
		  example, we can work out from the model that the probability of the
		  grass being wet is 60.6%. Such reasoning is called probabilistic
		  inference. Often we are interested in the distribution conditioned on
		  the fact that some random variables have been observed to hold a
		  particular value. In our example, having observed that the grass is
		  wet, we want to find out the chance it was raining on that day. "))
             (p
	       "Here we focus on the dsl " (i "Hansei") " " 
	       (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html" 
		       "Embedded probabilistic domain-specific language HANSEI")
	       " and the corresponding published paper "
	       (cite/a "https://okmij.org/ftp/kakuritu/dsl-paper.pdf" 
		       (i "Embedded Probabilistic Programming")
		       ", In proceedings of the IFIP working conference on
		       domain-specific languages, ed. Walid Taha. LNCS 5658,
		       Springer, 2009, pp. 360-384.")
	       ", by Oleg Kiselyov and Chung-chieh Shan; moreover, an application to logic can be found in "
               (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html"
                       "HANSEI as a Declarative Logic Programming Language")
	       ".")

	       (structure/section "Implementation")
	       (code/scheme/file "../hansei.scm")))

  ((test/procc/simple-model _)
   (⊦= '(((V (#t #t)) 0.36) ((V (#t #f)) 0.24) ((V (#f #t)) 0.24))
         (probcc-inference-exact
           (let* ((p 0.6)
                  (x (probcc-coin p))
                  (y (probcc-coin p)))
             (probcc-when (or x y) (list x y))))))


  ((test/procc/grass-model _)
   (define grass-model
     (probcc-model
       (let* ((rain (probcc-coin 0.3))
              (sprinkler (probcc-coin 0.5))
              (grass-is-wet
                (or (and (probcc-coin 0.9) rain)
                    (and (probcc-coin 0.8) sprinkler)
                    (probcc-coin 0.1))))
         (probcc-when grass-is-wet rain))))
   (⊦= (list (probcc-value 0.322 #f) (probcc-value 0.2838 #t)) 
         (probcc-explore +inf.0 grass-model)))

  ((test/procc/flip-xor-model _)
   (define flipxor-model
     (probcc-model
       (let loop ((p 0.6) (n 10))
         (cond
           ((equal? 1 n) (probcc-coin p))
           (else (not (equal? (probcc-coin (- 1 p)) (loop p (sub1 n)))))))))

   (let1 (res (probcc-explore +inf.0 flipxor-model))
         (⊦= '(((V #t) 0.500000051200001) ((V #f) 0.4999999488)) res)
         (⊦= 1024 (probcc-leaves flipxor-model))))

  ((test/procc/flip-xor-model/middle _)

   (define (flipxor-model c p)
     (probcc-model
       (letrec ((loop (λ (n)
                          (cond
                            ((equal? 1 n) (probcc-coin p))
                            (else (not (equal? (probcc-coin (- 1 p)) 
                                               (probcc-reflect (probcc-inference-exact (loop (sub1 n)))))))))))
         (loop c))))

   (let* ((tree (flipxor-model 10 0.6))
          (res (probcc-explore +inf.0 tree)))
     (⊦= '(((V #t) 0.5000000512) ((V #f) 0.4999999488)) res)
     (⊦= 4 (probcc-leaves tree))))

  ((test/procc/flip-xor-model/bucket _)

   (define (flipxor-model c p)
     (probcc-model
       (letrec ((loop (λ-probcc-bucket (n)
                                        (cond
                                          ((equal? 1 n) (probcc-coin p))
                                          (else (not (equal? (probcc-coin (- 1 p)) 
                                                             (loop (sub1 n)))))))))
         (loop c))))

   (let* ((tree (flipxor-model 10 0.6))
          (res (probcc-explore +inf.0 tree)))
     (⊦= '(((V #t) 0.5000000512) ((V #f) 0.4999999488)) res)
     (⊦= 2 (probcc-leaves tree))))

  )

(unittest/✓ hanseitest)


