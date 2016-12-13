; Essa funções é interessante pois ela testa se o polinômio é nulo verificando se é uma lista vazia
;ou se todos os coeficientes são nulos. Neste momento que a tabela e os processos genéricos se fazem
;importantes, independente do tipo do coeficiente(racional, complexo, etc...) nós somos capazes de verificar se é nulo.

(put '=zero? '(polynomial)
     (lambda (p) (zero-aux p)))

(define (zero-aux p)
  (or (null? (term-list p))
      (and (=zero? (coeff (first-term (term-list p))))
           (zero-aux2 (rest-term (term-list p))))))

(define (zero-aux2 terms-list)
  (and (=zero? (coeff (first-term terms-list)))
       (zero-aux2 (rest-term terms-list))))
  
