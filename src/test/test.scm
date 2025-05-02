
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
		  variables -- and relationships among them."))
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

	     (p "The first implementation of our language uses the probability
		monad that represents a stochastic computation as a lazy search
		tree. That is, our implementation uses the type constructor " (code/inline "pV") 
		" defined below.  "
		(code/lang "" 
			"type 'a vc = V of 'a | C of (unit -> 'a pV)\n"
			"and 'a pV = (prob * 'a vc) list")
		"Each node in a tree is a weighted list of branches. The empty
		list denotes failure, and a singleton list " (code/inline "[(p, V v)]") 
		" denotes a deterministic successful outcome" (code/inline v) "with the
		probability mass " (code/inline p) ". A branch of the form "
		(code/inline "V v") " is a leaf node
		that describes a possible successful outcome, whereas a branch
		of the form " (code/inline "C thunk") " is not yet explored.
		The intended meaning of a search tree of type " (code/inline
		"'a pV") " is a discrete probability distribution over values of type "
	     (code/inline "'a") ". ")

             (code/scheme/file "../hansei.scm")))

  ((test/procc/coin-model _)
   (⊦= '(((V ((x #t) (y #t))) 0.36)
	     ((V ((x #t) (y #f))) 0.24)
	     ((V ((x #f) (y #t))) 0.24)
	     ((V ((x #f) (y #f))) 0.16))
       (probcc-reify/exact
	 (τ
	   (let* ((p 0.6)
		  (x (probcc-coin p))
		  (y (probcc-coin p)))
	     `((x ,x) (y ,y)))))))

  ((test/procc/coin-model/when _)
   (⊦= '(((V (#t #t)) 0.36) ((V (#t #f)) 0.24) ((V (#f #t)) 0.24))
       (probcc-reify/exact
	 (τ
	   (let* ((p 0.6)
		  (x (probcc-coin p))
		  (y (probcc-coin p)))
	     (probcc-when (or x y) (list x y)))))))

  ((test/procc/grass-model _)
   (define-τ grass-model
       (let* ((rain (probcc-coin 0.3))
              (sprinkler (probcc-coin 0.5))
              (grass-is-wet
                (or (and (probcc-coin 0.9) rain)
                    (and (probcc-coin 0.8) sprinkler)
                    (probcc-coin 0.1))))
         (probcc-when grass-is-wet `(rain ,rain))))
   (⊦= (list (probcc-value 0.322 '(rain #f)) (probcc-value 0.2838 '(rain #t)))
         (probcc-reify/exact grass-model))
   `(doc
      (cite/quote
        "Oleg Kiselyov"
	"The canonical example is the grass model, with three random variables
	representing the events of rain, of a switched-on sprinkler and wet
	grass. The (a priori) probabilities of the first two events are judged
	to be 30% and 50% correspondingly. Probabilities are non-negative real
	numbers that may be regarded as weights on non-deterministic choices.
	Rain almost certainly (90%) wets the grass. The sprinkler also makes
	the grass wet, in 80% of the cases. The grass may also be wet for some
	other reason.  The modeler gives such an unaccounted event 10% of a
	chance. This model is often depicted as a directed acyclic graph (DAG)
	-- so-called Bayesian, or belief network -- with nodes representing
	random variables and edges conditional dependencies. Associated with
	each node is a distribution (such as Bernoulli distribution: the flip
					  of a biased coin), or a function that
	computes a distribution from the node's inputs (such as the noisy
							     disjunction nor).
	The sort of reasoning we wish to perform on the model is finding out
	the probability distribution of some of its random variables. For
	example, we can work out from the model that the probability of the
	grass being wet is 60.6%. Such reasoning is called probabilistic
	inference. Often we are interested in the distribution conditioned on
	the fact that some random variables have been observed to hold a
	particular value. In our example, having observed that the grass is
	wet, we want to find out the chance it was raining on that day. ")))


  ((test/procc/grass-model/complete _)
   (define-τ grass-model
       (let* ((rain (probcc-coin 0.3))
              (sprinkler (probcc-coin 0.5))
              (grass-is-wet
                (or (and (probcc-coin 0.9) rain)
                    (and (probcc-coin 0.8) sprinkler)
                    (probcc-coin 0.1))))
         `((rain ,rain) 
	   (sprinkler ,sprinkler)
	   (grass-is-wet ,grass-is-wet))))
   (⊦= '(((V ((rain #f) (sprinkler #f) (grass-is-wet #f))) 0.315)
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #t))) 0.287)
           ((V ((rain #t) (sprinkler #t) (grass-is-wet #t))) 0.1473)
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #t))) 0.1365)
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #f))) 0.063)
           ((V ((rain #f) (sprinkler #f) (grass-is-wet #t))) 0.035)
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #f))) 0.0135)
           ((V ((rain #t) (sprinkler #t) (grass-is-wet #f))) 0.0027)) 
         (probcc-reify/exact grass-model)))

  ((test/procc/flip-xor-model _)
   (define-τ flipxor-model
       (let loop ((p 0.6) (n 10))
         (cond
           ((equal? 1 n) (probcc-coin p))
           (else (not (equal? (probcc-coin (- 1 p)) (loop p (sub1 n))))))))

   (let1 (res (probcc-reify/exact flipxor-model))
         (⊦= '(((V #t) 0.500000051200001) ((V #f) 0.4999999488)) res)
         (⊦= 1024 (probcc-leaves (probcc-reify/0 flipxor-model)))))

  ((test/procc/flip-xor-model/middle _)

   (define (flipxor-model c p)
       (letrec ((loop (λ (n)
			 (τ
                          (cond
                            ((equal? 1 n) (probcc-coin p))
                            (else (not (equal? (probcc-coin (- 1 p)) 
                                               (probcc-reflect 
						 (probcc-reify/exact (loop (sub1 n))))))))))))
         (loop c)))

   (let* ((tree (flipxor-model 10 0.6))
          (res (probcc-reify/exact tree)))
     (⊦= '(((V #t) 0.5000000512) ((V #f) 0.4999999488)) res)
     (⊦= 4 (probcc-leaves (probcc-reify/0 tree)))))

  ((test/procc/flip-xor-model/bucket _)

   (define (flipxor-model c p)
     (τ
       (letrec ((loop (λ-probcc-bucket (n)
                                        (cond
                                          ((equal? 1 n) (probcc-coin p))
                                          (else (not (equal? (probcc-coin (- 1 p)) 
                                                             (loop (sub1 n)))))))))
         (loop c))))

   (let* ((tree (flipxor-model 10 0.6))
          (res (probcc-reify/exact tree)))
     (⊦= '(((V #t) 0.5000000512) ((V #f) 0.4999999488)) res)
     (⊦= 2 (probcc-leaves (probcc-reify/0 tree)))))

  )

(unittest/✓ hanseitest)



