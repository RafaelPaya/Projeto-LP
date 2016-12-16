 Essa parte foi considerada a mais importante e difícil de todo o projeto, o maior desafio desse
capítulo foi: 
 1. Aprender e implementar a noção de tabelas 
 2. Refazer todo o esquema de processos genéricos. 
Tivemos que nos aprofundar no capítulo =3.3.3= para o item 1, 2.4 e 2.5 para solucionar o item 2.

**1.

 Durante a pesquisa sobre representação de tabelas (3.3.3) conseguimos implementar as funções abaixo, e
ainda fomos capazes de otimizar os processos =lookup= e =insert!= usando apenas =hash-ref= e =hash=set!=:

#+BEGIN_EXAMPLE

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type)))
  
(define table (make-hash))

#+END_EXAMPLE

 As quatro funções abaixo foram desenvolvidas no capítulo 2.4:

#+BEGIN_EXAMPLE

(define (attach-tag type-tag contents)
  (if (equal? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum - TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum - CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))))))

#+END_EXAMPLE

 Lembrando que o papel das funções apresentadas acima é fazer uma procura na tabela, a partir de informações
dadas a elas, assim escolhendo o processo genérico correto a ser aplicado numa operação,essa escolha vai ser
definida pelas informações dadas de type e processo.
 
 Como exemplo: =(add '(polynomial x (1 3) (0 1)))= nesse casso temos o processo =add= e o tipo =polynomial=
que serão a chave para indentificar qual código correta para a operação.

 Agora definimos alguns desses processo aritméticos genéricos: 
#+BEGIN_EXAMPLE

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg x) (apply-generic 'neg x))
(define (=zero? x) (apply-generic '=zero? x))

(define (square a) (* a a))
#+END_EXAMPLE

**2.

 Agora vamos começar a desenvolver os pacotes do capítulo 2.5 para poder expandir nossas operações
do =polynimial-package=:
 
Scheme number package:

#+BEGIN_EXAMPLE

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (* x -1))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

#+END_EXAMPLE

Rational numbers:

#+BEGIN_EXAMPLE

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (neg-rat x)
    (make-rat (* (numer x) -1)
              (denom x)))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'neg '(rational)
       (lambda (x) (tag (neg-rat x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

#+END_EXAMPLE

 Definições para trabalhar tanto com a forma retangular quanto polar, também temos os 
=rectangular-package= e o =polar-package= abaixo:

#+BEGIN_EXAMPLE

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))



(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'angle '(rectangular) angle)
  (put 'magnitude '(rectangular) magnitude)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

#+END_EXAMPLE
Agora, finalmente, o complex package:

#+BEGIN_EXAMPLE

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (neg-complex z1)
    (make-from-real-imag (* (real-part z1) -1)
                         (* (imag-part z1) -1)))

  (define (tag x) (attach-tag 'complex x))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (z1) (tag (neg-complex z1))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

#+END_EXAMPLE

 Com tudo isso feito já podemos chamar os pacotes e começar a executar, Lembrando que ainda
temos o =polynomial-package= para unir a isso ao final do trabalho:

#+BEGIN_EXAMPLE

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

#+END_EXAMPLE
