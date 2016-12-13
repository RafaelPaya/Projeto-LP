; Neste momento foi apresentado uma outra forma de representar polinômios chamada de forma densa, so foi
;necessário mudar algumas coisas no código conforme pedido no exercício.

(define (adjoin-term term terms-list)
  (if (=zero? term)
      terms-list
      (cons term terms-list)))

(define (the-empty-termlist) '())

(define (first-term terms-list)
  (list (- (length terms-list) 1) (car terms-list)))

(define (rest-terms terms-list) (cdr terms-list))

(define (empty-termlist? terms-list) (null? terms-list))

(define (make-term coeff) (list coeff))
