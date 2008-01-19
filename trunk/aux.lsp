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
(setf *T-prueba* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(h nil nil nil nil nil nil)
		(h nil h nil nil nil nil)
		(h h m m m h h)
		(m m h m m h m)
		(m m m h h h m))))

(setf *prueba* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil))))

(setf *prueba2* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil h nil nil nil))))

(setf *prueba3* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil h nil nil nil)
		(nil nil nil h nil nil nil))))

(setf *prueba4* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil m nil nil nil)
		(nil nil nil h nil nil nil)
		(nil nil nil h nil nil nil))))

(setf *prueba5* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil h nil nil nil)
		(nil nil nil h nil nil nil)
		(nil nil nil h nil nil nil))))

;Estructura que representa el tablero con las fichas
(defstruct (partida (:constructor crea-tablero)
                    (:conc-name nil)
                    (:print-function muestra-tablero))
  fichas
  posiciones)

;; Devuelve la primera posicion de la lista de posiciones con una heuristica mayor
(defun mejor-eleccion (tablero heuristica color)
(let ((posicion-valor (list nil *minimo-valor*))
	(aux nil))
	(loop for x in (posiciones-pos tablero) do
		(if (< (second posicion-valor) (setf aux (funcall (symbol-function heuristica) tablero x color)))
			(setf posicion-valor (list x aux))))
	(first posicion-valor)))

;; (mejor-eleccion *t-prueba* 'heuristica-3 'x)

(defun posiciones-pos (tablero)
(let ((fila nil))
(loop for x in (movimientos-legales tablero) 
	when (not (null (setf fila (primera-posicion-vacia tablero x))))
	collect (list fila x))))
;; (posiciones-pos *t-prueba*)
;; Cuenta el numero de fichas consecutivas del mismo color y devuelve la longitud
;; de la secuencia más larga
(defun cuenta-fichas-consecutivas-en-secuencia (secuencia color)
(let ((cont 0))	
	(loop for x in secuencia
		maximize
		(if (eq x color)
			(setf cont (+ 1 cont))
			(setf cont 0)))))

; Constructor del tablero
;------------------------------
(defun crea-tablero-en-blanco ()
  (setf *tablero* (make-array '(6 7))))

;(crea-tablero-en-blanco)

;(inserta-ficha-en-fila *tablero* 1 'R)
;(imprime-tablero *tablero*)

(defun posicion-vacia (tablero f c)
  (if (eq nil (aref tablero f c))
      t ;vacia
    nil))
;(posicion-vacia *tablero* 1 1)