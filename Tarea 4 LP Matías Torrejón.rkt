#lang scheme

#|inicio -> (encode bits)|#

;; Crea variables globales de acumulación
;;
;;
;;

(define acum 0)
(define lista-retornar-encode '())

;; Recorre la lista entregada y agrega al retorno la cantidad de veces que aparece 'digito'
;;
;; lista: Lista a recorrer
;; digito: Digito que se busca (0 o 1)

(define (recorrer-lista lista digito)
  (if (null? lista)
      '()
      (begin
        (if (= (first lista) digito)     
            (set! acum (+ acum 1))      
            '())         
        (if (and (>= (length lista) 2)
                 (not (= (second lista) digito)))
            (begin
              (if (= digito 1)
                  (set! digito 0)                  
                  (set! digito 1))
              (set! lista-retornar-encode (cons acum lista-retornar-encode))  
              (set! acum 0))
            
            (if (= (length lista) 1)
                (set! lista-retornar-encode (cons acum lista-retornar-encode))
                '()))
        
        (recorrer-lista (rest lista) digito)))
  (set! acum 0)
)

;; Implementa el recorrido de lista a partir del primer digito que aparece en lista
;;
;; bits: Lista de bits a procesar
;;

(define (encode bits)
  (set! lista-retornar-encode '())
  (if (= (first bits) 1)
      (begin (recorrer-lista bits 1)
             (set! lista-retornar-encode (reverse lista-retornar-encode))
             (set! lista-retornar-encode (cons 0 lista-retornar-encode)))      
      (begin (recorrer-lista bits 0)
             (set! lista-retornar-encode (reverse lista-retornar-encode))))
  lista-retornar-encode
)
#|fin -> (encode bits)|#


#|inicio -> (decode_simple lista)|#

;; Crea variable de acumulación
;;
;;
;;

(define lista-retornar-decode '())

;; Añade 0's a la lista que va a retornar
;;
;; cantidad: Cantidad de 0's que se van a añadir
;;

(define (anadir-0 cantidad)
  (if (= cantidad 0)
      '()
      (begin
        (set! lista-retornar-decode (cons 0 lista-retornar-decode))
        (anadir-0 (- cantidad 1))))
)

;; Añade 1's a la lista que va a retornar
;;
;; cantidad: Cantidad de 1's que se van a añadir
;;

(define (anadir-1 cantidad)
  (if (= cantidad 0)
      '()
      (begin
        (set! lista-retornar-decode (cons 1 lista-retornar-decode))
        (anadir-1 (- cantidad 1))))
)

;; Implementa los anadir-0/-1 para crear el decode pero que acumula los retornos al no haber reseteo 
;;
;; lista: Lista a procesar
;;

(define (decode-simple-sin-reset lista)
  (if (not (null? lista))
      (begin
        (anadir-0 (first lista))
        (set! lista (rest lista))
        (if (not (null? lista))
            (begin
              (anadir-1 (first lista))
              (set! lista (rest lista))
              (decode-simple-sin-reset lista))
            (reverse lista-retornar-decode)))
      (reverse lista-retornar-decode))
)

;; Copia una lista en otra sin generar que los cambios de una afecten en otra
;;
;; lista-a-copiar: Lista que se va a replicar
;;

(define (cpy lista-a-copiar)
  (apply list lista-a-copiar)
)

;; Decode final que resetea los retornos después de entregarlos
;;
;; lista: Lista a procesar
;;

(define (decode_simple lista)
  (define lista-auxiliar (cpy (decode-simple-sin-reset lista)))
  (set! lista-retornar-decode '())
  lista-auxiliar
)
#|fin -> (decode_simple lista)|#


#|inicio -> (decode_cola lista)|#

;; Añade 0's a una lista resultado que va a retornar
;;
;; cantidad: Cantidad de 0's que se van a añadir
;; resultado: Lista que acumula los 0's

(define (anadir-0-cola cantidad resultado)
  (if (= cantidad 0)
      resultado
      (anadir-0-cola (- cantidad 1) (cons 0 resultado)))
)

;; Añade 1's a una lista resultado que va a retornar
;;
;; cantidad: Cantidad de 1's que se van a añadir
;; resultado: Lista que acumula los 1's

(define (anadir-1-cola cantidad resultado)
  (if (= cantidad 0)
      resultado
      (anadir-1-cola (- cantidad 1) (cons 1 resultado)))
)

;; Funcion auxiliar para generar el decode con recursión de cola
;;
;; lista: Lista a procesar
;; resultado: Lista que acumula los 0's para luego retornar

(define (decode-cola-auxiliar lista resultado)
  (cond
    ((null? lista)
     (reverse resultado))
    ((null? (rest lista))
     (decode-cola-auxiliar '() (anadir-0-cola (first lista) resultado)))
    (else
     (decode-cola-auxiliar (cddr lista) (anadir-1-cola (second lista) (anadir-0-cola (first lista) resultado)))))
)

;; Funcion decode que utiliza el auxiliar
;;
;; lista: Lista a procesar
;;

(define (decode_cola lista)
  (decode-cola-auxiliar lista '())
)
#|fin -> (decode_cola lista)|#


#|inicio -> (integrar_simple a b n f)|#

;; Retorna una ecuación simple de división y resta
;;
;; a: Valor a entregado
;; b: Valor b entregado
;; n: Valor n entregado

(define (resta-intervalo a b n)
  (/ (- b a) n)
)

;; Calcula la sumatoria de k a n de la ecuación entregada
;;
;; a: Valor a entregado
;; b: Valor b entregado
;; n: Valor n entregado
;; f: Función lambda entregada
;; k: Valor k entregado para inicializar sumatoria
;; resultado: Parámetro que acumula el resultado para posteriormente retornarlo

(define (sumatoria-simple a b n f k resultado)
  (if (= k n)
      resultado
      (sumatoria-simple a b n f (+ k 1) (+ resultado (f (+ a (* k (resta-intervalo a b n)))))))
)

;; Integra la función entregada entre los valores entregados
;;
;; a: Valor a entregado
;; b: Valor b entregado
;; n: Valor n entregado
;; f: Función lambda entregada

(define (integrar_simple a b n f)
  (exact->inexact (* (resta-intervalo a b n)
                     (+ (/ (f a) 2) (sumatoria-simple a b n f 1 0) (/ (f b) 2))))
)
#|fin -> (integrar_simple a b n f)|#


#|inicio -> (integrar_cola a b n f)|#

;; Calcula la sumatoria de k a n de la ecuación entregada utilizando recursión de cola gracias a la función auxiliar interna
;;
;; a: Valor a entregado
;; b: Valor b entregado
;; n: Valor n entregado
;; f: Función lambda entregada

(define (sumatoria-cola a b n f)

  ;; k: Valor k entregado para inicializar sumatoria
  ;; acumulador: Parámetro que acumula el resultado para posteriormente retornarlo
  
  (define (sumatoria-auxiliar k acumulador)
    (if (= k n)
        acumulador
        (sumatoria-auxiliar (+ k 1) (+ acumulador (f (+ a (* k (resta-intervalo a b n))))))))
  (sumatoria-auxiliar 1 0)
)

;; Integra la función entregada con recursión de cola
;;
;; a: Valor a entregado
;; b: Valor b entregado
;; n: Valor n entregado
;; f: Función lambda entregada

(define (integrar_cola a b n f)
  (exact->inexact (* (resta-intervalo a b n)
                     (+ (/ (f a) 2) (sumatoria-cola a b n f) (/ (f b) 2))))
)
#|fin -> (integrar_cola a b n f)|#


#|inicio -> (map_arbol arbol camino f)|#

;; Directamente aplica la función lambda entregada en el valor de cada nodo encontrado en el camino dado
;;
;; arbol: Árbol a recorrer
;; camino: Direcciones para bajar por el camino que se desea
;; f: Función lambda entregada
;; lista-retornar-map: Crea la lista de los valores modificados por la función lambda

(define (map_arbol-auxiliar arbol camino f lista-retornar-map)
  (cond ((or (null? arbol) (null? camino))
         lista-retornar-map)
        ((= (first camino) 0)
         (map_arbol-auxiliar (second arbol) (rest camino) f (cons (f (first arbol)) lista-retornar-map)))
        ((= (first camino) 1)
         (map_arbol-auxiliar (third arbol) (rest camino) f (cons (f (first arbol)) lista-retornar-map)))
        (else '())))
#|fin -> (map_arbol arbol camino f)|#
