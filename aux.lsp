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
(defun recarga()
(load "conecta4.lsp"))

(defun jr()
(juego :procedimiento (list 'minimax-a-b '3)))

(setf *prueba0* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(X nil nil nil nil nil nil)
		(X nil X nil nil nil nil)
		(X X O O O X X)
		(O O X O O X O)
		(O O O X X X O))))

(setf *prueba1* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(o o x nil nil x x)
		(x x x o nil o o))))

(setf *prueba2* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil x o nil nil))))

(setf *prueba3* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil X nil nil nil)
		(nil nil nil X nil nil nil))))

(setf *prueba4* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil x nil nil nil)
		(nil nil nil o nil nil nil)
		(x x x o x x x))))

(setf *prueba5* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil o nil nil nil)
		(nil nil nil o nil nil nil)
		(nil nil x o x nil nil))))

(setf *prueba6* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil x nil nil nil nil)
		(nil x o o nil nil nil)
		(x o o o x nil nil))))

(setf *prueba7* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil x o nil nil nil)
		(nil x o o o nil nil)
		(x o o o x o nil))))

(setf *unaficha1* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(x nil o o nil nil x))))

(setf *unaficha2* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil o nil nil nil nil)
		(x x nil o nil nil nil))))

(setf *unaficha3* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(x nil nil o nil nil nil)
		(x nil nil o nil nil nil))))

(setf *unaficha4* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil x nil nil))))

(setf *unaficha5* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil x nil))))

(setf *unaficha6* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil x))))

(setf tableros (list *prueba0* *prueba1* *prueba2* *prueba3* *prueba4* *prueba5* *prueba6* *prueba7*))

;; [35]> (heuristica-4 *unaficha0* '(4 0) 'X)
;; 3
;; [36]> (heuristica-4 *unaficha3* '(5 0) 'X)
;; 3
;; [37]> (heuristica-4 *unaficha0* '(5 1) 'X)
;; 3
;; [38]> (heuristica-4 *unaficha3* '(5 1) 'X)
;; 3
;; [39]> (heuristica-4 *unaficha0* '(5 2) 'X)
;; 3
;; [40]> (heuristica-4 *unaficha3* '(5 2) 'X)
;; 3
;; [42]> (heuristica-4 *unaficha0* '(5 3) 'X)
;; 4
;; [43]> (heuristica-4 *unaficha3* '(4 3) 'X)
;; 4
;; [44]> (heuristica-4 *unaficha0* '(5 4) 'X)
;; 0
;; [45]> (heuristica-4 *unaficha3* '(5 4) 'X)
;; 3
;; [46]> (heuristica-4 *unaficha0* '(5 5) 'X)
;; 0
;; [47]> (heuristica-4 *unaficha3* '(5 5) 'X)
;; 3
;; [48]> (heuristica-4 *unaficha0* '(5 6) 'X)
;; 0
;; [49]> (heuristica-4 *unaficha3* '(5 6) 'X)
;; 3

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

(defun heurjoke (tablero posicion color)
	(random 10))

(defun p-f-e (lista color)
(loop for x in lista do
(format t "valor ~a " (f-e x color))))

(defun f-e (tablero color)
	(imprime-tablero tablero)
	(format t "color ~a  " color)
	(loop for mov in (movimientos-legales tablero) summing
		(heuristica-4 tablero (list (primera-posicion-vacia tablero mov) mov) color)))

;; ;; Devuelve todas las posiciones no ocupadas del tablero
;; (defun posiciones-posibles (tablero)
;; 	(loop for i from 0 to *columnas*
;; 		append 
;; 		(loop for j from 0 to *filas* 
;; 			until (not (null (aref tablero j i)))
;; 			collect (list j i))))
;; 
;; ;; Cuenta el numero de piezas del mismo color que hay en un rango de 3 posiciones
;; (defun heuristica-1 (tablero lista-valores jugador)
;; 	(loop for pos in lista-valores count (igual-color tablero pos color)))
;; 
;; ;; Cuenta el numero de fichas consecutivas que habría sin colocar la nuestra y le resta
;; ;; el numero de turnos que tardaríamos en poner la ficha allí, 3 es lo máximo :D
;; (defun heuristica-2 (tablero posicion color)
;; 	(-
;; 		(fichas-consecutivas tablero posicion color)
;; 		(minimo-turnos-ocupar-posicion tablero posicion)))