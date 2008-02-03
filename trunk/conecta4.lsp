(load "aux.lsp")
(load "compheurs.lsp")

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
(defvar *estado-inicial*)
(defvar *jugador-humano* 'min)
(defvar *jugador-maquina* 'max)
(defvar *color-maquina* 'X)
(defvar *color-humano* 'O)
(defvar *profundidad* '3)
(defvar *ultimo-movimiento* '(0 0)) ;; Última posición donde se ha echado una ficha

;; Estructura que representa un nodo del árbol de búsqueda
(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nil)
                   (:print-function escribe-nodo-j))
  estado 		;; Tablero modificado
  jugador
  valor) 		;; Valor heurístico de la nueva jugada

;; Funcion que muestra por pantalla el nodo por el canal t (pantalla) y profundidad
(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
	(format canal "~%Estado :~%")
	(imprime-tablero (estado nodo-j))
	(format canal "~%Último movimiento : ~a" *ultimo-movimiento*)
	(format canal "~%Jugador : ~a" (jugador nodo-j)))

;; Función que inicializa *nodo-j-inicial*
(defun crea-nodo-j-inicial (jugador)
  (setf *estado-inicial* (make-array '(6 7)))
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
                 :jugador jugador)))

;; Muestra por pantalla el contenido de un tablero
(defun imprime-tablero (a)
  (let* ((dim (array-dimensions a))
	 (f (first dim))
	 (c (second dim)))
    (format t "~%  0   1   2   3   4   5   6~%")
    (escribe-linea-aux c)
    (loop for i from 0 to (- f 1)
	  do (loop for j from 0 to (- c 1)
		   do (if (equal (aref a i j) NIL)
			  (format t "|   ")
			(format t "| ~a " (aref a i j))))
	  (format t "| ~a~%" i)
	  (escribe-linea-aux c))))

;; Genera una línea del tablero a mostrar
(defun escribe-linea-aux (col)
       (loop for i from 0 to (- col 1)
               do (format t "+---"))
       (format t "+~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARBITRACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variable con la información del algoritmo a usar
(defvar *procedimiento*)
(defvar *movimientos* '(0 1 2 3 4 5 6))		;; Lista con las columnas en las que echar una ficha

;; Da comienzo a la partida y establece el primer turno de juego
(defun juego (&key (empieza-la-maquina? nil)
                   (procedimiento (list 'minimax '5)))
  (setf *procedimiento* procedimiento)
  (cond (empieza-la-maquina? (crea-nodo-j-inicial 'max)
                             (if (es-estado-final *estado-inicial*)
                                 (analiza-final *nodo-j-inicial*)
                                 (jugada-maquina *nodo-j-inicial*)))
        (t (crea-nodo-j-inicial 'min)
           (if (es-estado-final *estado-inicial*)
               (analiza-final *nodo-j-inicial*)
               (jugada-humana *nodo-j-inicial*)))))

;; Comprueba el resultado de la partida
;; Hay que tener en cuenta que se analiza un nodo para un jugador que ya ha echado su ficha, por eso todo
;; parece pensado para su contrincante
(defun analiza-final (nodo-j-final &optional (canal t))
  (escribe-nodo-j nodo-j-final)
  (cond ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'min)
         		(format canal "~&La maquina ha ganado"))
        ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'max)
         		(format canal "~&El humano ha ganado"))
        (t (format canal "~&Empate"))))

;; Función llamada cuando es el turno de la máquina
(defun jugada-maquina (nodo-j)
  (escribe-nodo-j nodo-j)
  (format t "~%Mi turno.~&")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
    (setf *ultimo-movimiento* (compara-tableros (estado nodo-j) (estado siguiente)))
    (if (es-estado-final (estado siguiente))
        (analiza-final siguiente)
        (jugada-humana siguiente))))

;; Devuelve para un determinado estado qué movimientos son posibles
(defun movimientos-legales (estado)
  (loop for m in *movimientos*
        when (primera-posicion-vacia estado m)
        collect m))

(defun fila-superior (tablero)
(loop for x in (loop for x in *movimientos* collect (primera-posicion-ocupada tablero x))
when (not (null x)) collect x))

;; Muestra por pantalla los movimientos permitidos obtenidos con movimientos-legales
(defun escribe-movimientos (movimientos)
  (format t "~%Los movimientos permitidos son:")
  (let ((numero 0))
    (loop for m in movimientos
          do
          (if (= (mod numero 3) 0)
              (format t "~%      ~a (~a)" m numero)
              (format t "      ~a (~a)" m numero))
          (setf numero (+ numero 1)))))

;; Función llamada cuando es el turno del humano
;; Modificado para permitir al humano solicitar consejo
(defun jugada-humana (nodo-j)
  (escribe-nodo-j nodo-j)
  (let ((movimientos (movimientos-legales (estado nodo-j))))
    (escribe-movimientos movimientos)
    (format t "~%Tu turno (escribe <<consejo>> si quieres una sugerencia): ")
    (let ((m (read)))
	 (cond ((equal m 'consejo)				;; En el caso de que quiera pedir consejo
			(solicitar-consejo nodo-j)
			(format t "~%Tu turno : ")		;; Hay que volver a leer la m una vez dado el consejo
			(setf m (read))))
      	(cond ((and (integerp m) (< -1 m (length movimientos)))
		(let ((nuevo-estado
			(aplica-movimiento (nth m movimientos) (estado nodo-j) *color-humano*)))
			(cond (nuevo-estado
				(let ((siguiente (crea-nodo-j
					:estado nuevo-estado
					:jugador 'max))) 
				(setf *ultimo-movimiento* (compara-tableros (estado nodo-j) (estado siguiente))) ;;Elección del humano
	                        (if (es-estado-final nuevo-estado)
     	                       		(analiza-final siguiente)
          	                	(jugada-maquina siguiente))))
               	      	(t (format t "~&   El movimiento ~a no se puede usar. " m)
			(jugada-humana nodo-j)))))
		(t (format t "~&   ~a es ilegal. " m)
               		(jugada-humana nodo-j))))))

;; TODO - No funciona bien
;; Función que se llama cuando se pide consejo a la máquina
(defun solicitar-consejo (nodo-j)
  (format t "Pensando")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))	;; TODO - Devuelve dnd echaría la máquina, así q no tiene demasiado sentido XD
	(format t " - Mi recomendación: ~a" (second (compara-tableros
							(estado nodo-j)
							(estado siguiente))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES AUXILIARES DE ARBITRACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compara dos tableros, tales que el segundo es el mismo que el primero pero con una jugada más,
;; y devuelve el movimiento que lleva del primer tablero al segundo
(defun compara-tableros (viejo nuevo)
	(let ((resultado nil))
		(loop for i from 0 to *filas* do
			(loop for j from 0 to *columnas* do
				(if (not (equal (aref viejo i j) (aref nuevo i j)))
					(setf resultado (list i j))
					nil)))
		resultado))

;; Determina si ha ganado algún jugador la partida
;; Hay que tener en cuenta que se analiza un nodo para un jugador que ya ha echado su ficha, por eso todo
;; parece pensado para su contrincante
(defun es-estado-ganador (tablero jugador turno)
	(if (es-estado-final tablero)
		(cond
			((not (movimientos-legales tablero))
				nil) ;; Empate
			((and (equal jugador *jugador-humano*)
				 (equal turno 'min))
				t) ;; Gana máquina
			((and (equal jugador *jugador-maquina*)
				 (equal turno 'max))
				t) ;; Gana humano
			(t nil))
		nil))

;; Comprueba si la ficha de la posición dada es del color dado
(defun mismo-color (tablero posicion color)
	(if (eq (aref tablero (first posicion) (second posicion)) color)
		t 
		nil))

;; Devuelve el nodo siguiente según una jugada de la IA
(defun aplica-decision (procedimiento nodo-j)
	(funcall (symbol-function (first procedimiento)) nodo-j (first(rest procedimiento))))

;; Devuelve el estado siguiente según el movimiento dado por el jugador, sin alterar el tablero original
(defun aplica-movimiento (columna tablero color)
	(let ((posicion (primera-posicion-vacia tablero columna))
		 (nuevo-tablero (duplica-tablero tablero)))
		(cond ((null posicion)
			nil)
			(t
				(setf (aref nuevo-tablero (first posicion) columna) color)
				nuevo-tablero))))

;; Devuelve una copia de un tablero
(defun duplica-tablero (tablero)
	(let ((nuevo-tablero (make-array '(6 7))))
		(loop for i from 0 to *filas* do
			(loop for j from 0 to *columnas* do
				(setf (aref nuevo-tablero i j) (aref tablero i j))))
		nuevo-tablero))

;; Determina si el juego ha llegado a su final
(defun es-estado-final (tablero)
(cond ((<= (length (movimientos-legales tablero)) 0) t)
	(t 
	(< 0
	(loop for x in (fila-superior tablero) 
 	count (or
		(> (maximo-conecta-4 (rango-accesible tablero x *color-humano*)) 3)	;; Se tiene en cuenta el centro del tablero
		(> (maximo-conecta-4 (rango-accesible tablero x *color-maquina*)) 3)))))))

(defun maximo-conecta-4 (listas)
(if (listp listas)
(maximo
	(loop for x in listas when (> (length x) 3) collect
	(cuenta-fichas-consecutivas x)))
0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITMO MINIMAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Valores máximos y mínimos para las variables alfa y beta
(defvar *minimo-valor* -10080)
(defvar *maximo-valor* 10080)
(defvar *medio-valor* 720) ;;porque son valores facilmente divisible 2*3*4*5*6

;; Para un posible nodo del árbol devuelve sus hijos
(defun sucesores (nodo-j)
  (let ((resultado ()))
    (loop for movimiento in *movimientos* do
      (let ((siguiente
             (aplica-movimiento movimiento
              (estado nodo-j) (if (equal (jugador nodo-j) 'max)
		*color-maquina*
		*color-humano*))))
        (when siguiente
          (push
            (crea-nodo-j
		:estado siguiente
		:jugador (contrario (jugador nodo-j)))
            resultado))))
    (nreverse resultado)))

;; Devuelve el jugador contrario al dado
(defun contrario (jugador)
  (if (eq jugador 'max) 'min 'max))

;; Algoritmo MINIMAX
(defun minimax (nodo-j profundidad)
  (if (or (es-estado-final (estado nodo-j))
          (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                        (jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                              (jugador nodo-j)))
            (if (eq (jugador nodo-j) 'max)
                (maximizador sucesores profundidad)
                (minimizador sucesores profundidad))))))

;; Función que busca maximizar (MAX) la puntuación
(defun maximizador (sucesores profundidad)
  (let ((mejor-sucesor (first sucesores))
        (mejor-valor *minimo-valor*))
    (loop for sucesor in sucesores do
          (setf valor (valor (minimax sucesor (1- profundidad))))
          (when (> valor mejor-valor)
                (setf mejor-valor valor)
                (setf mejor-sucesor sucesor)))
    (setf (valor mejor-sucesor) mejor-valor)
    mejor-sucesor))

;; Función que busca minimizar (MIN) la puntuación
(defun minimizador (sucesores profundidad)
  (let ((mejor-sucesor (first sucesores))
        (mejor-valor *maximo-valor*))
    (loop for sucesor in sucesores do
          (setf valor (valor (minimax sucesor (1- profundidad))))
          (when (< valor mejor-valor)
                (setf mejor-valor valor)
                (setf mejor-sucesor sucesor)))
    (setf (valor mejor-sucesor) mejor-valor)
    mejor-sucesor))

;; Algoritmo MINIMAX con poda ALFA-BETA
(defun minimax-a-b (nodo-j profundidad
                           &optional (alfa *minimo-valor*)
                           (beta *maximo-valor*))
  (if (or (es-estado-final (estado nodo-j)) (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                        (jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
	   (format t "~%DEBUG - Tablero original: ")	;; DEBUG
	   (imprime-tablero (estado nodo-j))	;; DEBUG
	   (format t "~%DEBUG - Tableros sucesores: ")	;; DEBUG
	   (loop for x in sucesores do	;; DEBUG
		(imprime-tablero (estado x))	;; DEBUG
		(format t "~%DEBUG - Jugador ~a, Valor heurístico sucesor: ~a~&" (jugador x) (f-e-estatica (estado x) (jugador x))))	;; DEBUG
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                              (jugador nodo-j)))
          (if (eq (jugador nodo-j) 'max)
              (maximizador-a-b
               (sort sucesores #'> :key (lambda (nodo) (f-e-estatica (estado nodo) 'min)))
               profundidad alfa beta)
              (minimizador-a-b
               (sort sucesores #'< :key (lambda (nodo) (f-e-estatica (estado nodo) 'max)))
               profundidad alfa beta))))))

;; Función que busca maximizar (MAX) la puntuación con ALFA-BETA
(defun maximizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
          (setf valor
                (valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
          (when (> valor alfa)
                (setf alfa valor)
                (setf mejor-sucesor sucesor))
          (when (>= alfa beta)
                (return)))
    (setf (valor mejor-sucesor) alfa)
	(format t "~%DEBUG - Mejor sucesor maximizador, jugador ~a: "(jugador mejor-sucesor)) ;; DEBUG
	(imprime-tablero (estado mejor-sucesor)) ;; DEBUG
    mejor-sucesor))

;; Función que busca minimizar (MIN) la puntuación con ALFA-BETA
(defun minimizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
          (setf valor
                (valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
          (when (< valor beta)
                (setf beta valor)
                (setf mejor-sucesor sucesor))
          (when (>= alfa beta)
                (return)))
    (setf (valor mejor-sucesor) beta)
	(format t "~%DEBUG - Mejor sucesor minimizador, jugador ~a: "(jugador mejor-sucesor)) ;; DEBUG
	(imprime-tablero (estado mejor-sucesor)) ;; DEBUG
    mejor-sucesor))

;; Devuelve una valoración heurística para un nodo (jugada)
;; Parece que no tenga sentido comprobar las posiciones para el color del jugador contrario, pero al igual
;; que es-estado-ganador o analiza-final resulta que el jugador que recibimos como parámetro no es otro que
;; el del último nodo creado, un nodo sucesor del cual queremos conocer su heurística pero para el jugador
;; que echó último, es decir, el jugador anterior
(defun f-e-estatica (tablero jugador)
  (cond
    ((es-estado-ganador tablero jugador 'min) (* *columnas* *maximo-valor*))
    ((es-estado-ganador tablero jugador 'max) *minimo-valor*)
    ((equal jugador *jugador-maquina*)
    		(loop for posicion in (posiciones-heuristicas tablero) summing
			(heuristica-4 tablero posicion *color-humano*)))
    ((equal jugador *jugador-humano*)
		(loop for posicion in (posiciones-heuristicas tablero ) summing
	  		(heuristica-4 tablero posicion *color-maquina*)))))

;; Devuelve la lista de posiciones adecuadas por la cual se va a valorar el tablero
(defun posiciones-heuristicas (tablero)
  (loop for i from 0 to *columnas* collect
    (if (null (primera-posicion-vacia tablero i))
      (primera-posicion-ocupada tablero i)
      (primera-posicion-vacia tablero i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES HEURÍSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cuenta el numero de piezas del mismo color que hay en un rango de 3 posiciones
(defun heuristica-1 (tablero lista-valores jugador)
	(loop for pos in lista-valores count (igual-color tablero pos color)))

;; Cuenta el numero de fichas consecutivas que habría sin colocar la nuestra y le resta
;; el numero de turnos que tardaríamos en poner la ficha allí, 3 es lo máximo :D
(defun heuristica-2 (tablero posicion color)
	(-
		(fichas-consecutivas tablero posicion color)
		(minimo-turnos-ocupar-posicion tablero posicion)))

;; Esta es la heuristica definitiva :D (TODO - Cambiar comentario XD )
(defun heuristica-3  (tablero posicion color)
  (loop for x in (rango-accesible tablero posicion color) 
  when (> (length x) 3) 
  summing
    (heuristica-3-aux 
    (distancia-minima x posicion) 
    (cuenta-fichas-consecutivas x) 
    (cuenta-fichas x))))

(defun heuristica-3-aux (distancia consecutivas fichas)
  (cond
    ((or (null distancia) (null consecutivas))
	0)
    ((< 2 consecutivas)
	;; Si hay tres del mismo color en linea desde esa posicion hemos ganado
	(/ *maximo-valor* (max 1 distancia)))
    ((= 2 consecutivas)
;; 	(* (/ *medio-valor* (max 1 distancia)) fichas))
	(/ *medio-valor* (max 1 distancia)))
;; 		da mucha prioridad a cuando tienes dos consecutivas
    (t
	(* (- *columnas* distancia ) fichas))))

;; Heuristica 4 corta la jugada del contrario
(defun heuristica-4 (tablero posicion color)
(let 
  ((heuristica-favor (heuristica-3 tablero posicion color))
    (heuristica-contra (heuristica-3 tablero posicion (contrincante color)))) ;; Le da menos prioridad a ganar él
  (cond 
	((< heuristica-contra heuristica-favor)
	heuristica-favor)
	(t
    	(* -1 heuristica-contra))))) ;; El siguiente paso es para el contrario, favorecemos al contrario

(defun contrincante (color)
(if (eq color *color-humano*)
	*color-maquina*
	*color-humano*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCIONES AUXILIARES DE LA HEURÍSTICA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Secuencias es una doble lista de valores, el primer miembro es la primera
;; parte de la lista de valores y el segundo miembro es la segunda parte de la lista
;; Se han ordenado de esta manera para facilitar contar las fichas consecutivas
;; partiendo de la posicion inicial que se presupone nil
;; Devuelve el numero de fichas del mismo color consecutivas
(defun cuenta-fichas-consecutivas (secuencia)
(let ((maximo 0)
	(aux 0))
	(loop for x in secuencia do
		(cond ((not (null x))
			(setf aux (+ aux 1))
			(if (< maximo aux)
				(setf maximo aux) 
				nil))
		(t
			(setf aux 0))))
	maximo))

(defun cuenta-fichas (secuencia)
(loop for x in secuencia count (not (null x))))

;; Devuelve el maximo entero de la lista, y si la lista es vacía devuelve 0
(defun maximo (lista)
	(if  (null lista)
		0
		(apply #'max
			(loop for x in lista when (not (null x)) collect x)))) ;; Hay que filtrar los nil ya que max no los reconoce

;; Devuelve la posición de la primera casillla ocupada de la columna
(defun primera-posicion-ocupada (tablero columna)
  (let ((fila 
    (loop for i from 0 to *filas* until (aref tablero i columna) count t)))
      (if (> fila *filas*)
	nil
	(list fila columna))))

;; Devuelve la posición de la primera casillla vacía de la columna
(defun primera-posicion-vacia (tablero columna)
  (let (( fila 
	(- *filas* (loop for i from *filas* downto 0 
		until (null (aref tablero i columna)) count t))))
	(if (> 0 fila)
	nil (list fila columna))))

(defun posicion-ocupada (tablero f c)
  (if (null (aref tablero f c))
      t ;vacia
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES DE RANGOS DE VALORES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Genera una secuencia con el rango de valores accesibles desde una posición
;; en el tablero y que conecten con un color

(defun distancia-minima (lista pos)
  (loop for x in lista when (not (null x)) minimize (distancia x pos)))

(defun distancia (posx posy)
(+ (abs (- (second posx) (second posy)))))

(defun rango-accesible (tablero pos color)
(loop for x in
	(list 
		(seccion-fila-accesible tablero (first pos) (second pos) color) 
		(seccion-columna-accesible tablero (first pos) (second pos) color) 
		(seccion-diagonal-izq-accesible tablero (first pos) (second pos) color) 
		(seccion-diagonal-der-accesible tablero (first pos) (second pos) color))
	when (< 3 (length x)) collect x)) ;; filtro que tenga un tamaño minimo de 4

;; Función que dice si la posicion inferior esta ocupada o no
(defun inacesible (tablero f c)
(if (pos-invalida (+ f 1) c)
	 nil ;;tamos en el fondo del tablero
	(null (aref tablero (+ f 1) c))))

;; Función de corte,devuelve T sólo si es distinto color
(defun corte (x y)
(not (or (eq x y) (null x))))

;; Devuelve t para una posicion invalida en la matriz
(defun pos-invalida (f c)
(or
	(> 0 f)
	(> 0 c)
	(> f *filas*)
	(> c *columnas*)))

(defun seccion-fila-accesible (tablero f c color)
  (append 
	(reverse 
	(loop for i from c downto 0  ;;tiene en cuenta el centro
		until 
		(or 
			(corte (aref tablero f i) color) 
			(inacesible tablero f i))
    	collect
		(if (null (aref tablero f i))
			nil
			(list f i))))
	(loop for i from (+ 1 c) to *columnas* 
		until 
		(or 
			(corte (aref tablero f i) color) 
			(inacesible tablero f i))
    		collect
		(if (null (aref tablero f i))
			nil
			(list f i)))))

(defun seccion-columna-accesible (tablero f c color)
  (append 
	(loop for i from 0 to f until (corte (aref tablero i c) color)
	  collect
	  (if (null (aref tablero i c))
	    nil
	    (list i c)))
 ;; las posiciones arriba no estan ocupadas
	(loop for i from (+ 1 f) to *filas* until (corte (aref tablero i c) color)
    		collect
		(if (null (aref tablero i c))
			nil
			(list i c)))))


(defun seccion-diagonal-izq-accesible (tablero f c color) 
    (append
	(reverse ;; tiene que estar al revés
	(loop for i from 0 to *maximo-valor*  ;;tiene en cuenta el centro
		until (or 
			(pos-invalida (- f i) (- c i)) 
			(corte (aref tablero (- f i) (- c i)) color) 
			(inacesible tablero (- f i) (- c i))) 
	collect
		(if (null (aref tablero  (- f i) (- c i)))
		nil
		(list(- f i) (- c i)))))
	(loop for i from 1 to *maximo-valor* 
		until (or 
			(pos-invalida (+ f i) (+ c i)) 
			(corte (aref tablero (+ f i) (+ c i)) color) 
			(inacesible tablero (+ f i) (+ c i))) 
	collect
		(if (null (aref tablero  (+ f i) (+ c i)))
		nil
		(list (+ f i) (+ c i))))))

(defun seccion-diagonal-der-accesible (tablero f c color) 
    (append
	(reverse ;; tiene que estar al revés
	(loop for i from 0 to *maximo-valor* ;; tiene en cuenta el centro
		until (or 
			(pos-invalida (+ f i) (- c i)) 
			(corte (aref tablero (+ f i) (- c i)) color) 
			(inacesible tablero (+ f i) (- c i))) 
	collect
		(if (null (aref tablero  (+ f i) (- c i)))
		nil
		(list (+ f i) (- c i)))))
	(loop for i from 1 to *maximo-valor* 
		until (or 
			(pos-invalida (- f i) (+ c i)) 
			(corte (aref tablero (- f i) (+ c i)) color) 
			(inacesible tablero (- f i) (+ c i))) 
	collect
		(if (null (aref tablero  (- f i) (+ c i)))
		nil
		(list (- f i) (+ c i))))))
