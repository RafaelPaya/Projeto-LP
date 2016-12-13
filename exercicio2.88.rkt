; Esse exercício foi facilitado quando percebemos que não era necessário refazer tudo do zero. Era possível
;utilizar a função add se fosse mudado os sinais de todos os coeficientes do polinômio que era subtraido, o que tornou
;rápido e simples.

(put 'neg '(polynomial)
     (lambda (p) (tag (neg-poly p))))

(define (neg-poly p)
  (make-poly (variable p)
             (neg-list (term-list p))))

(define (neg-list terms-list)
  (if (null? terms-list)
      terms-list
      (adjoin-term
       (neg (coeff (first-term terms-list)))
       (neg-list (rest-list terms-list)))))

(define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list (neg-poly p2))))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 (neg-poly p2)))))
