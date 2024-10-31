
(import unittest aux hansei)

(define-suite hanseitest

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