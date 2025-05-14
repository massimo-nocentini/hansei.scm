
(import
  (chicken sort)
  unittest aux hansei wolfram srfi-1)

(define-wolfram W (->MathML 'MathML))

; setting some parameters
(op/plus (λ args (W `(Simplify (Plus ,@args)))))
(op/subtract (λ args (W `(Simplify (Subtract ,@args)))))
(op/times (λ args (W `(Simplify (Times ,@args)))))
(op/divide (λ args (W `(Simplify (Divide ,@args)))))
(op/greater (λ (a b) (> (W `(LeafCount ,a)) (W `(LeafCount ,b)))))

(define (wolfram/joint-form result)
  `(MatrixForm
     (List ,@(letmap ((p result)) 
		     `(Rule (Prob ,@(map (λ (each)
					    (cond
					      ((procedure? each) 'proc)
					      (else (second each)))) 
					 (cadr (car p)))) ,(cadr p))))))

(define-suite hansei-symbolic-suite

  ((doc r) `((structure/section "Symbolic")))

  ((test/coin-model _)
   (define-τ model (let* ((p 'p)
                          (x (probcc-coin p))
                          (y (probcc-coin p)))
                      `((x ,x) (y ,y))))

   (define result (probcc-reify/exact model))

   (⊦= '(((V ((x #t) (y #f))) (Times -1 (Plus -1 p) p))
           ((V ((x #f) (y #t))) (Times -1 (Plus -1 p) p))
           ((V ((x #f) (y #f))) (Power (Plus -1 p) 2))
           ((V ((x #t) (y #t))) (Power p 2)))
         result)
   (⊦= result (probcc-normalize result))
   `(doc (p "Joint distribution of tossing two coins, where head has probability " (math (m p)) " to appear:")
         (container (escape ,(->MathML (wolfram/joint-form result) rule/MathML/display/block)))
	 (p "observe that both proportional and normalized probabilities " (b "are the same")
	    " because there is no observation that rules out some branches, therefore the normalization constant equals " 
	    (math 1) ".")))

  ((test/coin-model/observed _)
   (define result (probcc-reify/exact
		    (τ
		      (let* ((p 'p)
			     (x (probcc-coin p))
			     (y (probcc-coin p)))
			(probcc-when (or x y) `((x ,x) (y ,y)))))))
   (define normalized (probcc-normalize result))
   (⊦= '(((V ((x #t) (y #f))) (Times -1 (Plus -1 p) p))
           ((V ((x #f) (y #t))) (Times -1 (Plus -1 p) p))
           ((V ((x #t) (y #t))) (Power p 2)))
         result)

   `(doc (p "Slightly variation of the previous test, here it has been " 
            (b "observed") " that " (i "at least one head") " appeared. Now the probabilities are " 
	    (i "proportional") " according to the following rules: " )
	 (container (escape ,(->MathML (wolfram/joint-form result) rule/MathML/display/block)))
	 (p " and they can be normalized, ")
	 (container (escape ,(->MathML (wolfram/joint-form normalized) rule/MathML/display/block)))
	 (p " to obtain a valid probability distribution, provided that the assumed observation occurred.")))


  ((test/grass-model _)
   (define-τ grass-model
     (let* ((rain (probcc-coin 'r))
            (sprinkler (probcc-coin 's))
            (grass-is-wet
              (or (and (probcc-coin 'w) rain)
                  (and (probcc-coin 'v) sprinkler)
                  (probcc-coin 'e))))
       (probcc-when grass-is-wet `(rain ,rain))))

   (define result (probcc-reify/exact grass-model))
   (define normalized (probcc-normalize result))
   (define p/rain (second (first normalized)))
   (define p/not-rain (second (second normalized)))
   (define rules '(List (Rule r 0.3) (Rule s 0.5) (Rule w 0.9) (Rule v 0.8) (Rule e 0.1)))

   (⊦= '(((V (rain #t))
            (Times r
                   (Plus (Times e (Plus -1 (Times s v)) (Plus -1 w))
                         w
                         (Times s (Plus v (Times -1 v w))))))
           ((V (rain #f))
            (Times (Plus -1 r)
                   (Plus (Times -1 s v) (Times e (Plus -1 (Times s v)))))))
         result)
   (⊦= '(((V (rain #t)) 0.2838) ((V (rain #f)) 0.322)) 
       (letmap ((p result)) `(, (car p) ,(W `(ReplaceAll ,(cadr p) ,rules)))))
   `(doc (p "The conditional probability that " (i "rained") " given that a " (i "wet grass") " has been observed is ")
         (container (escape ,(->MathML p/rain rule/MathML/display/block)))
         (p "fully symbolic. On the other hand, the rules")
         (container (escape ,(->MathML `(MatrixForm ,rules) rule/MathML/display/block)))
         (p "allow us to get numerical values according to the expected results already shown.")))

  ((test/grass-model/joint _)
   (define-τ grass-model
     (let* ((rain (probcc-coin 'r))
            (sprinkler (probcc-coin 's))
            (grass-is-wet
              (or (and (probcc-coin 'w) rain)
                  (and (probcc-coin 'v) sprinkler)
                  (probcc-coin 'e))))
       `((rain ,rain) (sprinkler ,sprinkler) (grass-is-wet ,grass-is-wet))))

   (define result (probcc-reify/exact grass-model))
   (define normalized (probcc-normalize result))

   (⊦= '(((V ((rain #t) (sprinkler #t) (grass-is-wet #t)))
            (Times r
                   s
                   (Plus v
                         (Times e (Plus -1 v) (Plus -1 w))
                         w
                         (Times -1 v w))))
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #t)))
            (Times r (Plus -1 s) (Plus (Times e (Plus -1 w)) (Times -1 w))))
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #t)))
            (Times (Plus -1 r) s (Plus (Times e (Plus -1 v)) (Times -1 v))))
           ((V ((rain #t) (sprinkler #t) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) r s (Plus -1 v) (Plus -1 w)))
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) r (Plus -1 s) (Plus -1 w)))
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) (Plus -1 r) s (Plus -1 v)))
           ((V ((rain #f) (sprinkler #f) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) (Plus -1 r) (Plus -1 s)))
           ((V ((rain #f) (sprinkler #f) (grass-is-wet #t)))
            (Times e (Plus -1 r) (Plus -1 s))))
         result)

   `(doc (p "If we remove the " (i "observation") (code/scheme (probcc-when grass-is-wet `(rain ,rain)))
            "from the previous test, then we can show the " (i "joint distribution"))
         (container (escape ,(->MathML `(MatrixForm
                                          (List ,@(letmap ((p normalized)) 
                                                          `(Rule (p ,@(map second (cadr (car p)))) ,(cadr p)))))
                                       rule/MathML/display/block)))
         (p "where " (math (m (p rain sprinkler grass-is-wet))) " is the " (i "non-normalized") " probability density function.")))


  ((test/geometric _)
   (define result ((probcc-reify (τ (probcc-geometric 'p 's 'f))) 5))
   (define t6 (cadr (car (sixth result))))
   (define t7 (cadr (car (seventh result))))
   (define t8 (cadr (car (eighth result))))
   #;(⊦= `(((V 3) (Times -1 (Power (Plus -1 p) 3) p))
           ((C ,t6) (Times -1 (Power (Plus -1 p) 5) p))
           ((V 2) (Times (Power (Plus -1 p) 2) p))
           ((V 4) (Times (Power (Plus -1 p) 4) p))
           ((C ,t8) (Times -1 (Power (Plus -1 p) 7)))
           ((C ,t7) (Times (Power (Plus -1 p) 6) p))
           ((V 1) (Times -1 (Plus -1 p) p))
           ((V 0) p))
       result)
   `(doc (container (escape ,(->MathML `(MatrixForm
                                          (List ,@(letmap ((p result)) 
                                                          `(Rule (p ,(let1 (v (cadr (car p))) 
									   (if (procedure? v) v `(List ,@v)))) ,(cadr p)))))
                                       rule/MathML/display/block)))))

  ((test/flip _)
   (define-τ model
     (let loop ((p 'p) (n 10))
       (cond
         ((equal? 1 n) (probcc-coin p))
         (else (and (probcc-coin p) (loop p (sub1 n)))))))

   (define result (probcc-reify/exact model))

   (⊦= '(((V #f) (Plus 1 (Times -1 (Power p 10)))) ((V #t) (Power p 10))) result)
   (⊦= 11 (probcc-leaves (probcc-reify/0 model)))
   `(doc (container (escape ,(->MathML (second (first result)) rule/MathML/display/block)))))

  ((test/flip-xor-model _)
   (define-τ flipxor-model
     (let loop ((p 'p) (n 10))
       (cond
         ((equal? 1 n) (probcc-coin p))
         (else (not (equal? (probcc-coin p) (loop p (sub1 n))))))))

   (define res (probcc-reify/exact flipxor-model))
   (⊦= '(((V #f)
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
                         (Times -256 (Power p 9)))))) res)
   (⊦= 1024 (probcc-leaves (probcc-reify/0 flipxor-model)))
   `(doc (container (escape ,(->MathML `(Expand ,(second (first res))) rule/MathML/display/block)))
	 (container (escape ,(->MathML `(Expand ,(second (second res))) rule/MathML/display/block)))))

  ((test/flip-xor-model/middle _)

   (define (flipxor-model c p)
     (letrec ((loop (λ (n)
                        (τ
                          (cond
                            ((equal? 1 n) (probcc-coin p))
                            (else (not (equal? (probcc-coin p) 
                                               (probcc-reflect
                                                 (probcc-reify/exact 
						   (loop (sub1 n))))))))))))
       (loop c)))

   (define tree (flipxor-model 10 'p))
   (define res (probcc-reify/exact tree))
   (⊦= '(((V #f)
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
                         (Times -256 (Power p 9)))))) res)
   (⊦= 4 (probcc-leaves (probcc-reify/0 tree)))
   `(doc (container (escape ,(->MathML `(Expand ,(second (first res))) rule/MathML/display/block)))
	 (container (escape ,(->MathML `(Expand ,(second (second res))) rule/MathML/display/block)))))

  ((test/flip-xor-model/bucket _)

   (define (flipxor-model c p)
     (τ
       (letrec ((loop (λ-probcc-bucket (n)
                                        (cond
                                          ((equal? 1 n) (probcc-coin p))
                                          (else (not (equal? (probcc-coin ((op/subtract) 1 p))
                                                             (loop (sub1 n)))))))))
         (loop c))))

   (define tree (flipxor-model 10 'p))
   (define res (probcc-reify/exact tree))
   (⊦= '(((V #t)
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
           ((V #f)
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
                         (Times -256 (Power p 9)))))) res)
   (⊦= 2 (probcc-leaves (probcc-reify/0 tree)))
   `(doc (container (escape ,(->MathML `(Expand ,(second (first res))) rule/MathML/display/block)))
	 (container (escape ,(->MathML `(Expand ,(second (second res))) rule/MathML/display/block)))))

  )

(unittest/✓ hansei-symbolic-suite)






