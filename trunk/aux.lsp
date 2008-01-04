;El juego del conecta 4
;
;    * Consideremos un tablero con forma de retícula de 7 columnas con 6 celdas en cada una.
;    * Hay dos jugadores que juegan por turnos. Las piezas se colocan dejándolas caer desde lo alto de una columna que no está completa, y la pieza se coloca en la posición libre más baja de dicha columna.
;    * El juego acaba cuando algún jugador logra tener en el tablero 4 fichas adyacentes alineadas en horizontal, vertical o en diagonal, y ese jugador gana la partida. En caso de que el tablero se llene y ningún jugador haya logrado ganar, se acaba la partida con resultado de empate. 
;
;Se pide:
;
;   1. Implementar el juego propuesto en el entorno de juegos visto en clase:
;          * Describir la representación elegida para los estados y los movimientos.
;          * Definir varias funciones de evaluación estática y comprobar su bondad experimentalmente.
;          * Se valorará la claridad de la interfaz. 
;   2. Modificar el algoritmo del juego de manera que en el turno del jugador humano (MIN) permita solicitar consejo al ordenador sobre la mejor jugada.
;   3. Definir una función compara_heurs que reciba los nombres de dos funciones heurísticas y genere un fichero de texto con la partida que resulta si MIN utiliza la primera heurística y MAX la segunda.
;   4. Como aliciente para estimular la búsqueda de implementaciones eficientes (sobre todo buenas funciones de evaluación estática) se realizará un torneo entre los trabajos entregados, premiándose a los mejores. 
;
;Forma de entrega.
;Se deberá entregar un solo fichero, conteniendo la implementación del juego (representación de estados, movimientos, funciones de evaluación, etc.) así como los procedimientos generales de control de juego necesarios (puede utilizarse material visto en clase). El código deberá estar convenientemente comentado.
;
;Agustín Riscos Núñez
;ariscosn@us.es

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *T-prueba* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(x nil nil nil nil nil nil)
		(x nil x nil nil nil nil)
		(x o x o o x o)
		(o x o x o x o)
		(x x x o o o x))))

;Estructura que representa el tablero con las fichas
(defstruct (juego (:constructor crea-tablero)
                   (:conc-name nil)
                   (:print-function muestra-tablero))
  fichas
  posiciones)

; Constructor del tablero
;------------------------------
(defun crea-tablero-en-blanco ()
  (setf *tablero* (make-array '(6 7))))

;(crea-tablero-en-blanco)

;Función que imprime el tablero
;------------------------------

(defun imprime-tablero (a)
  (let* ((dim (array-dimensions a))
	 (f (first dim))
	 (c (second dim)))
    (format t "~%")
    (escribe-linea-aux c)
    (loop for i from 0 to (- f 1)
	  do (loop for j from 0 to (- c 1)
		   do (if (equal (aref a i j) NIL)
			  (format t "| ")
			(format t "|~a" (aref a i j))))
	  (format t "|~%")
	  (escribe-linea-aux c))))

(defun escribe-linea-aux (col)
       (loop for i from 0 to (- col 1)
               do (format t "+-"))
       (format t "+~%"))

;(imprime-tablero *tablero*)

;Funcion de inserción de las fichas
;------------------------------------
;inserta una ficha correctamente en la columna que tu quieras
;notas: Si la columna esta llena no peta
(defun inserta-ficha-en-columna (tablero columna color)
  (setf (aref tablero 
	      (primera-posicion-vacia tablero columna)
	      columna)
	color))

;(inserta-ficha-en-fila *tablero* 1 'R)
;(imprime-tablero *tablero*)

(defun posicion-vacia (tablero f c)
  (if (eq nil (aref tablero f c))
      t ;vacia
    nil))
;(posicion-vacia *tablero* 1 1)      

(defun primera-posicion-vacia (tablero columna)
    (loop for i from *fila* downto 0
	  minimize i
	  until(posicion-vacia tablero i columna)))
;(primera-posicion-vacia *tablero* 1)

;Funciones de contar
;-------------------------------
;; (defun mismo-color (tablero i j color)
;;   (if (eq (aref tablero i j) color)
;;       t
;;     nil))
;(mismo-color *tablero* 1 1 'r)


(defun fichas-consecutivas (tablero lista color)
(maximo (loop for x in lista collect 
	(cuenta-fichas-consecutivas 
		(recorre-posiciones tablero x) color))))	

;; Devuelve en una lista los valores contenidos en el tablero
;; que esten en las posciones definidas por lista
(defun recorre-posiciones (tablero lista)
	(loop for x in lista collect 
		(aref tablero (first x) (second x))))

;; Cuenta el numero de fichas consecutivas del mismo colo y devuelve la longitud de la secuencia mas larga
(defun cuenta-fichas-consecutivas (secuencia color)
(let ((cont 0))	
	(loop for x in secuencia
		maximize
		(if (eq x color)
		(setf cont (+ 1 cont))
		(setf cont 0)))))
;; devuelve el maximo entero de la lista

(defun maximo (lista)
	(apply #'max lista))