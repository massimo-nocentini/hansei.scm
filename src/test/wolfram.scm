
(import
  (chicken sort)
  unittest aux hansei wolfram srfi-1)

(define-wolfram W (->MathML 'MathML))

(op/plus (λ args (W `(Simplify (Plus ,@args)))))
(op/subtract (λ args (W `(Simplify (Subtract ,@args)))))
(op/times (λ args (W `(Simplify (Times ,@args)))))
(op/divide (λ args (W `(Simplify (Divide ,@args)))))
(op/greater (λ (x y) #f))

(define-suite hansei-symbolic-suite

  ((doc r) `((structure/section "Symbolic")))

  ((test/procc/grass-model _)
   (define-τ grass-model
     (let* ((rain (probcc-coin 'r))
            (sprinkler (probcc-coin 's))
            (grass-is-wet
              (or (and (probcc-coin 'w) rain)
                  (and (probcc-coin 'v) sprinkler)
                  (probcc-coin 'e))))
       (probcc-when grass-is-wet `(rain ,rain))))
   (define result (probcc-reify/exact grass-model))
   (⊦= '(((V (rain #t))
            (Times r
                   (Plus (Times e (Plus -1 (Times s v)) (Plus -1 w))
                         w
                         (Times s (Plus v (Times -1 v w))))))
           ((V (rain #f))
            (Times (Plus -1 r)
                   (Plus (Times -1 s v) (Times e (Plus -1 (Times s v)))))))
         result)
   `(doc (p (escape ,(->MathML (second (second (probcc-normalize result)))
                               '(Rule "MathAttributes" (List (Rule "display" "block"))))))))

  )

(unittest/✓ hansei-symbolic-suite)





