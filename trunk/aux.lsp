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
		(x nil nil nil nil nil nil)
		(x nil o o o nil x))))

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
		(nil nil o x x nil x))))

(setf *unaficha5* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil x nil nil nil)
		(nil nil nil x nil nil nil)
		(nil o nil o nil nil nil))))

(setf *unaficha6* (make-array '(6 7) :initial-contents
             '((nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil)
		(nil nil nil x x o x))))

(setf *tableros* (list *prueba0* *prueba1* *prueba2* *prueba3* *prueba4* *prueba5* *prueba6* *prueba7* *unaficha1* *unaficha2* *unaficha3* *unaficha4* *unaficha5* *unaficha6*))

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


;(posicion-vacia *tablero* 1 1)

(defun heurjoke (tablero posicion color)
	(random 10))

(defun pFED (color)
(loop for x in *tableros* do
(format t "valor ~a " (feD x color))))

(defun pFEDd (color)
(loop for x in *tableros* do
(format t "valor ~a " (feDD x color))))

(defun feD (tablero color)
	(imprime-tablero tablero)
	(format t "color ~a  " color)
	(loop for mov in (movimientos-legales tablero) summing
		(heuristica-4 tablero (list (primera-posicion-vacia tablero mov) mov) color)))
;; 		(heuristicaDC tablero (list (primera-posicion-vacia tablero mov) mov) color)))
(defun feDD (tablero color)
	(imprime-tablero tablero)
	(format t "color ~a  " color)
	(loop for mov in (movimientos-legales tablero) collect
		(heuristica-4 tablero (list (primera-posicion-vacia tablero mov) mov) color)))
;; 		(heuristicaDC tablero (list (primera-posicion-vacia tablero mov) mov) color)))

(defun heuristicaD (tablero posicion color)
(let 
  ((heuristica-favor (heuristica-3D tablero posicion color))
    (heuristica-contra (heuristica-3D tablero posicion (contrincante color))))
  (if (<= heuristica-favor heuristica-contra)
    (* -1 heuristica-contra)
    heuristica-favor)))

(defun heurísticaDC (tablero posicion color)
(heuristica-3D tablero posicion color))


(defun heuristica-3D  (tablero posicion color)
(let* ((listas (cuatro-en-linea-posible tablero posicion color))
	(valor-heuristico-consecutivas (heuristica-3-auxD listas tablero posicion color))
	(valor-heuristico-multiplicador (heuristica-3-aux-max4D listas)))
	(format t "~% posicion: ~a ,color: ~a ,listas posibles ~a " posicion color listas) ;;debug
	(format t "~%Nº listas o valor: ~a " valor-heuristico-consecutivas) ;;debug
	(format t "consecutivas: ~a " valor-heuristico-multiplicador) ;;debug
		(cond ((eq valor-heuristico-consecutivas *maximo-valor*)
;; significa que si ponemos aqui una ficha ganamos
			*maximo-valor*)
			((eq valor-heuristico-consecutivas  *medio-valor*)
			(+ *medio-valor* valor-heuristico-multiplicador))
;; significa que tiene dos en linea y que deberiamos cortarsela por si acaso
			(* valor-heuristico-consecutivas valor-heuristico-multiplicador))))
;; significa que multiplicaremos el numero de posibles lineas completas que podemos tener y lo dividiremos por el numero de turnos que tardaríamos en llegar nosotros a esa posicion


;; detecta cuando solo hay que insertar una ficha para ganar y devuelve el maximo valor heuristico para ese nodo
;; si no devuelve un numero que será mayor mientras menos fichas tengamos que insertar para conseguir 4 en linea
;; devuelve un numero
(defun heuristica-3-auxD (listas tablero posicion color)
	(cond
		((= 3 (fichas-consecutivas tablero posicion color))
		;; Si hay tres del mismo color en linea desde esa posicion hemos ganado
			*maximo-valor*)
		((= 2 (fichas-consecutivas tablero posicion color))
			*medio-valor*)
;; 		da mucha prioridad a cuando tienes dos consecutivas
		((null listas) 
		;; Si no hay donde poner no pongas
			*minimo-valor*)
		(t
		(length listas))))
			
(defun heuristica-3-aux-max4D (listas)
(maximo 
			(loop for x in listas collect
				(mas-posibilidades-conecta-4 x))))

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

;; Fila con un rango + - 3
;; (defun rango-posiciones (f c)
;; 	(list (seccion-fila f c) (seccion-columna f c) (seccion-diagonal-izq f c) (seccion-diagonal-der f c)))
;; 
;; (defun seccion-fila (f c)
;;   (let ((inicio (max (- c 3) 0))
;; 	(fin (min (+ c 3) *columnas*)))
;; 	(list
;;     	(reverse ;; tiene que estar al revés
;; 		(loop for i from inicio to (- c 1) collect (list f i)))
;; 	(loop for i from (+ 1 c) to fin collect (list f i)))))
;; 
;; ;; Columna con un rango de + - 3
;; (defun seccion-columna (f c)
;;   (let ((inicio (max (- f 3) 0))
;; 	(fin (min (+ f 3) *filas*)))
;; 	(list 
;;     	(reverse ;; tiene que estar al revés
;; 		(loop for i from inicio to (- f 1) collect (list i c)))
;; 	(loop for i from (+ 1 f) to fin collect (list i c)))))
;; 
;; ;; Diagonal izquierda con un rango de + - 3 
;; (defun seccion-diagonal-izq (f c) 
;;     (list
;; 	(reverse ;; tiene que estar al revés
;; 	(loop for i from 3 downto 1 when (and (>= (- f i) 0) (>= (- c i) 0)) collect
;; 	(list (- f i) (- c i))))
;; 	(loop for i from 1 to 3 when (and (<= (+ f i) *filas*) (<= (+ c i) *columnas*)) collect
;; 	(list (+ f i) (+ c i)))))
;; 
;; ;; diagonal derecha con un rango de + - 3
;; (defun seccion-diagonal-der (f c) 
;;     (list
;; 	(reverse ;; tiene que estar al revés
;; 	(loop for i from 3 downto 1 when (and (>= (- f i) 0) (<= (+ c i) *columnas*)) collect
;; 	(list (- f i) (+ c i))))
;; 	(loop for i from 1 to 3 when (and (<= (+ f i) *filas*) (>= (- c i) 0)) collect
;; 	(list (+ f i) (- c i)))))
;; 
;; ;; Devuelve una lista de posiciones posibles en las que insertar una ficha de
;; ;; nuestro color incrementaria el numero de fichas consecutivas
;; (defun todas-posiciones-posibles (tablero posicion color)
;; 	(loop for x in (rango-color (first posicion) (second posicion) color)
;; 		collect
;; 			(posiciones-cuatro-en-linea tablero x color)))
;; 
;; ;; Devuelve todas las posiciones no ocupadas del tablero
;; (defun posiciones-posibles (tablero)
;; 	(loop for i from 0 to *columnas*
;; 		append 
;; 		(loop for j from 0 to *filas* 
;; 			until (not (null (aref tablero j i)))
;; 			collect (list j i))))
;; 
;; ;; Devuelve sólo las posiciones consecutivas accesibles que conectan con nuestro color
;; ;; accesibles significa libres y no cortadas por otro color
;; (defun posiciones-cuatro-en-linea (tablero rango color)
;; 	(loop for posiciones in rango
;; 	append
;; 	(loop for x in posiciones
;; 		until (not (or
;; 			(eq (aref tablero (first x) (second x)) color)
;; 			(eq (aref tablero (first x) (second x)) nil)))
;; 		collect
;; 		(if (eq (aref tablero (first x)(second x)) nil)
;; 		x
;; 		nil))))
;; 
;; (defun cuenta-fichas-consecutivas (secuencias color)
;; (+
;; 		(loop for x in (first secuencias) count (eq x color) until (not(eq x color)))
;; 		(loop for x in (second secuencias) count (eq x color) until (not(eq x color))))))
;; 
;; (defun cuenta-fichas-consecutivas-con-centro (secuencias tablero posicion color)
;; 	(+
;; 		(loop for x in (first secuencias) count (eq x color) until (not(eq x color)))
;; 		(if (eq color (aref tablero (first posicion) (second posicion)))
;; 			1
;; 			0)
;; 		(loop for x in (second secuencias) count (eq x color) until (not(eq x color)))))
;; 
;; (defun fichas-ocupadas (lista)
;; (- (length lista)
;; (loop for x in lista count (not (null x)))))
;; 
;; (defun maximo-consecutivos (lista)
;; (let ((max 0)
;; 	(aux 0))
;; 	(loop for x in lista do
;; 		(if (null x)
;; 			(if (<= max aux)
;; 			(setf max (setf aux (+ 1 aux)))
;; 			(setf aux (+ 1 aux)))
;; 		(setf aux 0)))
;; 	max))
;; 
;; ;; Dada una posicion (x y) devuelve el numero maximo de veces consecutivas que se repite el color
;; (defun fichas-consecutivas (tablero posicion color)
;; (maximo 
;; 	(loop for x in 
;; 		(rango-color tablero (first posicion) (second posicion) color) 
;; 		collect 
;; 			(cuenta-fichas-consecutivas 
;; 				 color))))
;; 
;; (defun fichas-consecutivas-con-centro (tablero posicion color)
;; (maximo 
;; 	(loop for x in 
;; 		(rango-color (first posicion) (second posicion) color) 
;; 		collect 
;; 			(cuenta-fichas-consecutivas-con-centro
;; 				(recorre-posiciones tablero x) tablero posicion color))))
;; 
;; ;; Lista tiene una doble lista con los las posiciones del array ordenadas de
;; ;; tal modo que el primer elemento es la parte derecha y el segundo es la parte
;; ;; izquierda sin incluir la posicion inicial
;; ;; Devuelve en una doble lista los valores contenidos en el tablero
;; ;; que esten en las posciones definidas por lista
;; (defun recorre-posiciones (tablero lista)
;; 	(list 
;; 		(loop for x in (first lista) collect 
;; 			(aref tablero (first x) (second x)))
;; 		(loop for x in (second lista) collect 
;; 			(aref tablero (first x) (second x)))))
;; 
;; ;; Mínimo de turnos que necesitaremos para ocupar esa posición
;; (defun minimo-turnos-ocupar-posicion (tablero posicion)
;; 	(loop for i from *filas* downto (first posicion) count
;; 		(eq (aref tablero i (second posicion)) nil)))
;; 
;; ;; Devuelve una lista de listas de posiciones posibles en las que colocando
;; ;; una ficha del color apropiado se podria hacer cuatro en linea
;; (defun conecta-4-posible (tablero posicion color)
;; 	(loop for x in (todas-posiciones-posibles tablero posicion color)
;; 		when (< 2 (length x)) collect x))
;; ;; 			mayor que tres son 4


;; (defun rango-color (tablero pos color)
;; (loop for x in
;; (list (seccion-fila-color tablero (first pos) (second pos) color) (seccion-columna-color tablero (first pos) (second pos) color) (seccion-diagonal-izq-color tablero (first pos) (second pos) color) (seccion-diagonal-der-color tablero (first pos) (second pos) color))
;; when (< 1 (length x)) collect x))
;; 
;; 
;; 
;; (defun seccion-fila-color (tablero f c color)
;;   (append 
;; 	(reverse (loop for i from (- c 1) downto 0 until (corte (aref tablero f i) color)
;;     	collect
;; 	(if (null (aref tablero f i))
;; 		nil
;; 		color)))
;; 	(loop for i from (+ 1 c) to *columnas* until (corte (aref tablero f i) color)
;;     	collect
;; 	(if (null (aref tablero f i))
;; 		nil
;; 		color))))
;; 
;; (defun seccion-columna-color (tablero f c color)
;;   (append 
;; 	(reverse (loop for i from (- f 1) downto 1 until (corte (aref tablero i c) color)
;;     	collect
;; 	(if (null (aref tablero i c))
;; 		nil
;; 		color)))
;; 	(loop for i from (+ 1 f) to *filas* until (corte (aref tablero i c) color)
;;     	collect
;; 	(if (null (aref tablero i c))
;; 		nil
;; 		color))))
;; 
;; ;;devuelve T solo si la posicion es invalida
;; 
;; 
;; (defun seccion-diagonal-izq-color (tablero f c color) 
;;     (append
;; 	(reverse ;; tiene que estar al revés
;; 	(loop for i from 1 to *maximo-valor* 
;; 		until (or (pos-invalida (- f i) (- c i)) (corte (aref tablero (- f i) (- c i)) color)) 
;; 	collect
;; 		(if (null (aref tablero  (- f i) (- c i)))
;; 		nil
;; 		color)))
;; 	(loop for i from 1 to *maximo-valor* 
;; 		until (or (pos-invalida (+ f i) (+ c i)) (corte (aref tablero (+ f i) (+ c i)) color)) 
;; 	collect
;; 		(if (null (aref tablero  (+ f i) (+ c i)))
;; 		nil
;; 		color))))
;; 
;; (defun seccion-diagonal-der-color (tablero f c color) 
;;     (append
;; 	(reverse ;; tiene que estar al revés
;; 	(loop for i from 1 to *maximo-valor* until (or (pos-invalida (+ f i) (- c i)) (corte (aref tablero (+ f i) (- c i)) color)) 
;; 	collect
;; 		(if (null (aref tablero  (+ f i) (- c i)))
;; 		nil
;; 		color)))
;; 	(loop for i from 1 to *maximo-valor* until (or (pos-invalida (- f i) (+ c i)) (corte (aref tablero (- f i) (+ c i)) color)) 
;; 	collect
;; 		(if (null (aref tablero  (- f i) (+ c i)))
;; 		nil
;; 		color))))