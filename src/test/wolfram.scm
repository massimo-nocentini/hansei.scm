
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

(define-suite hansei-symbolic-suite

  ((doc r) `((structure/section "Symbolic")))

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
   (⊦= '(((V (rain #t)) 0.2838) ((V (rain #f)) 0.322)) (letmap ((p result))
			       `(,(car p) ,(W `(ReplaceAll ,(cadr p) ,rules)))))
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
	 (p "where " (math/inline "p(rain, sprinkler, grass-is-wet)") " is the " (i "non-normalized") " probability density function.")))

  )

(unittest/✓ hansei-symbolic-suite)





