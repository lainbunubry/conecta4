(load "funciones-auxiliares.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPRESENTACIÓN DE ESTADOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Se ha elegido una representación a partir de una matriz, en donde las fichas
;; de cada bando se representan por X u O respectivamente, y las casillas vacías
;; mediante NIL

;; Definicion de variables
(defvar *filas* 5)		;; Los arrays comienzan en cero
(defvar *columnas* 6)
(defvar *nodo-j-inicial*)
(defvar *estado-inicial* (make-array '(6 7)))
(defvar *jugador-humano* 'humano)
(defvar *jugador-maquina* 'maquina)
(defvar *color-maquina* 'M)
(defvar *color-humano* 'H)

;; Estructura que representa un nodo del árbol de búsqueda
(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nil)
                   (:print-function escribe-nodo-j))
  estado 		;; Tablero modificado
  jugador
  valor) 		;; Valor heurístico de la nueva jugada

;; Funcion que muestra por pantalla el nodo por el canal t (pantalla) y profundidad
(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
  (format canal "~%Estado : ~a~%Jugador : ~a"		;; TODO - Hay que cambiar esto para que use imprime-tablero
          (estado nodo-j)
          (jugador nodo-j)))

;; Función que inicializa *nodo-j-inicial*
(defun crea-nodo-j-inicial (jugador)
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
                 :jugador jugador)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES HEURÍSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Front-End para las heurísticas, sirve para que f-e-estatica no tenga que preocuparse
;; sobre qué heurística es la más avanzada, siempre llama a esta función y es aquí donde
;; se escoje la mejor heurística
(defun funcion-heuristica (tablero jugador)
	(cond
		((equal jugador *jugador-maquina)
			(loop for mov in (movimientos-legales tablero) maximize
				(heuristica-2 tablero mov *color-maquina*)))
		(t
			(loop for mov in (movimientos-legales tablero) maximize
				(heuristica-2 tablero mov *color-humano*)))))
				
;; Recibe los nombres de dos funciones heurísticas y genere un fichero de texto con la partida que
;; resulta si MIN utiliza la primera heurística y MAX la segunda
(defun compara_heurs (heuristica1 heuristica2)		;; TODO - Bufff XD
	)

;; Cuenta el numero de piezas del mismo color que hay en un rango de 3 posiciones
(defun heuristica-1 (tablero lista-valores jugador)
	(loop for pos in lista-valores count (igual-color tablero pos color)))

;; (defun igual-color (tablero pos color)
;; 	(eq (aref tablero (first pos) (second pos)) color))

;; Cuenta el numero de fichas consecutivas que habría sin colocar la nuestra y le resta
;; el numero de turnos que tardaríamos en poner la ficha allí, 3 es lo máximo :D
(defun heuristica-2 (tablero posicion color)
	(-
		(fichas-consecutivas tablero posicion color)
		(minimo-turnos-ocupar-posicion tablero posicion)))

;; (defun mejor-eleccion (tablero heuristica posiciones color)
;; (loop for x in 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCIONES AUXILIARES DE LA HEURÍSTICA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mínimo de turnos que necesitaremos para ocupar esa posición
(defun minimo-turnos-ocupar-posicion (tablero posicion)
	(loop for i from *filas* downto (first posicion) count
		(eq (aref tablero i (second posicion)) nil)))

;; Devuelve una lista de listas de posiciones posibles en las que colocando
;; una ficha del color apropiado se podria hacer cuatro en linea
(defun cuatro-en-linea-posible (tablero posicion color)
	(loop for x in (todas-posiciones-posibles tablero posicion color)
		when (< 2 (length x)) collect x))

;; Devuelve una lista de posiciones posibles en las que insertar una ficha de
;; nuestro color incrementaria el numero de fichas consecutivas
(defun todas-posiciones-posibles (tablero posicion color)
	(loop for x in (rango-posiciones (first posicion) (second posicion))
		collect
			(posiciones-posibles tablero x color)))

;; TODO - Esta función falla
;; Devuelve sólo las posiciones consecutivas accesibles que conectan con nuestro color
;; (defun posiciones-posibles (tablero rango color)
;; 	(loop for posiciones in rango
;; 		append
;; 			(loop for x in posiciones 
;; 				until (not (or 
;; 							(eq (aref tablero (first x) (second x)) color) 
;; 							(eq (aref tablero (first x) (second x)) nil)))
;; 				collect
;; 					(if (eq (aref tablero (first x) (second x)) nil)
;; 						x
;; 						nil)))))

;; Dada una posicion (x y) devuelve el numero maximo de veces consecutivas que se repite el color
(defun fichas-consecutivas (tablero posicion color)
(maximo 
	(loop for x in 
		(rango-posiciones (first posicion) (second posicion)) 
		collect 
			(cuenta-fichas-consecutivas 
				(recorre-posiciones tablero x) color))))

;; Lista tiene una doble lista con los las posiciones del array ordenadas de
;; tal modo que el primer elemento es la parte derecha y el segundo es la parte
;; izquierda sin incluir la posicion inicial
;; Devuelve en una doble lista los valores contenidos en el tablero
;; que esten en las posciones definidas por lista
(defun recorre-posiciones (tablero lista)
	(list 
		(loop for x in (first lista) collect 
			(aref tablero (first x) (second x)))
		(loop for x in (second lista) collect 
			(aref tablero (first x) (second x)))))

;; Secuencias es una doble lista de valores, el primer miembro es la primera
;; parte de la lista de valores y el segundo miembro es la segunda parte de la lista
;; Se han ordenado de esta manera para facilitar contar las fichas consecutivas
;; partiendo de la posicion inicial que se presupone nil
;; Devuelve el numero de fichas del mismo color consecutivas
(defun cuenta-fichas-consecutivas (secuencias color)
	(+
		(loop for x in (first secuencias) count (eq x color) until (not(eq x color)))
		(loop for x in (second secuencias) count (eq x color) until (not(eq x color)))))

;; Devuelve el maximo entero de la lista
(defun maximo (lista)
	(apply #'max lista))

;; Cuenta el numero de fichas consecutivas del mismo color y devuelve la longitud
;; de la secuencia más larga
(defun cuenta-fichas-consecutivas-en-secuencia (secuencia color)
(let ((cont 0))	
	(loop for x in secuencia
		maximize
		(if (eq x color)
			(setf cont (+ 1 cont))
			(setf cont 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES DE RANGOS DE VALORES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Genera el rango de valores de la matriz donde vamos a contar
;; las fichas que esten consecutivas
;; f y c representan la el numero de fila y de columna (f,c)

;; Devuelve una lista de listas de listas(iz der) de posiciones filas,columnas
;; y diagonales ordenas desde el centro y sin incluir la posicion
(defun rango-posiciones (f c)
	(list (seccion-fila f c) (seccion-columna f c) (seccion-diagonal-izq f c) (seccion-diagonal-der f c)))

;; Fila con un rango + - 3
(defun seccion-fila (f c)
  (let ((inicio (max (- c 3) 0))
	(fin (min (+ c 3) *columnas*)))
	(list
    	(reverse ;; tiene que estar al revés
		(loop for i from inicio to (- c 1) collect (list f i)))
	(loop for i from (+ 1 c) to fin collect (list f i)))))

;; Columna con un rango de + - 3
(defun seccion-columna (f c)
  (let ((inicio (max (- f 3) 0))
	(fin (min (+ f 3) *filas*)))
	(list 
    	(reverse ;; tiene que estar al revés
		(loop for i from inicio to (- f 1) collect (list i c)))
	(loop for i from (+ 1 f) to fin collect (list i c)))))

;; Diagonal izquierda con un rango de + - 3 
(defun seccion-diagonal-izq (f c) 
    (list
	(reverse ;; tiene que estar al revés
	(loop for i from 3 downto 1 when (and (>= (- f i) 0) (>= (- c i) 0)) collect
	(list (- f i) (- c i))))
	(loop for i from 1 to 3 when (and (<= (+ f i) *filas*) (<= (+ c i) *columnas*)) collect
	(list (+ f i) (+ c i)))))

;; diagonal derecha con un rango de + - 3
(defun seccion-diagonal-der (f c) 
    (list
	(reverse ;; tiene que estar al revés
	(loop for i from 3 downto 1 when (and (>= (- f i) 0) (<= (+ c i) *columnas*)) collect
	(list (- f i) (+ c i))))
	(loop for i from 1 to 3 when (and (<= (+ f i) *filas*) (>= (- c i) 0)) collect
	(list (+ f i) (- c i)))))
