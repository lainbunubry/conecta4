(load "aux.lsp")

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
(defvar *profundidad* '5)

;; Estructura que representa un nodo del árbol de búsqueda
(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nil)
                   (:print-function escribe-nodo-j))
  estado 		;; Tablero modificado
  jugador
  valor) 		;; Valor heurístico de la nueva jugada

;; Funcion que muestra por pantalla el nodo por el canal t (pantalla) y profundidad
(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
	(format canal "~%Estado :")
	(imprime-tablero (estado nodo-j))
	(format canal "~%Jugador : ~a" (jugador nodo-j)))

;; Función que inicializa *nodo-j-inicial*
(defun crea-nodo-j-inicial (jugador)
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
                 :jugador jugador)))

;; Muestra por pantalla el contenido de un tablero
(defun imprime-tablero (a)
  (let* ((dim (array-dimensions a))
	 (f (first dim))
	 (c (second dim)))
    (format t "~% 0 1 2 3 4 5 6 ~%")
    (escribe-linea-aux c)
    (loop for i from 0 to (- f 1)
	  do (loop for j from 0 to (- c 1)
		   do (if (equal (aref a i j) NIL)
			  (format t "| ")
			(format t "|~a" (aref a i j))))
	  (format t "|~%")
	  (escribe-linea-aux c))))

;; Genera una línea del tablero a mostrar
(defun escribe-linea-aux (col)
       (loop for i from 0 to (- col 1)
               do (format t "+-"))
       (format t "+~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARBITRACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variable con la información del algoritmo a usar
(defvar *procedimiento*)
(defvar *movimientos* '(0 1 2 3 4 5 6))		;; Lista con las columnas en las que echar una ficha

;; Da comienzo a la partida y establece el primer turno de juego
(defun juego (&key (empieza-la-maquina? nil)
                   (procedimiento (list 'minimax-a-b '5)))
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
(defun analiza-final (nodo-j-final &optional (canal t))
  (escribe-nodo-j nodo-j-final)
  (cond ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'max)
         (format canal "~&La maquina ha ganado"))
        ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'min)
         (format canal "~&El humano ha ganado"))
        (t (format canal "~&Empate"))))

;; Función llamada cuando es el turno de la máquina
(defun jugada-maquina (nodo-j)
  (escribe-nodo-j nodo-j)
  (format t "~%Mi turno.~&")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
    (if (es-estado-final (estado siguiente))
        (analiza-final siguiente)
        (jugada-humana siguiente))))

;; Devuelve para un determinado estado qué movimientos son posibles
(defun movimientos-legales (estado)
  (loop for m in *movimientos*
        when (aplica-movimiento m estado)
        collect m))

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
     	               (aplica-movimiento (nth m movimientos) (estado nodo-j))))
          	     (cond (nuevo-estado
               	       (let ((siguiente (crea-nodo-j
	:estado nuevo-estado
	:jugador 'max)))
	                        (if (es-estado-final nuevo-estado)
     	                       (analiza-final siguiente)
          	                (jugada-maquina siguiente))))
               	      (t (format t "~&   El movimiento ~a no se puede usar. " m)
                    	    (jugada-humana nodo-j)))))
            (t (format t "~&   ~a es ilegal. " m)
               (jugada-humana nodo-j))))))

;; Función que se llama cuando se pide consejo a la máquina
(defun solicitar-consejo (nodo-j)
  (format t "~%Mi recomendación: ")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
	(format t "~a" (compara-tableros
				(estado nodo-j)
				(estado siguiente)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES AUXILIARES DE ARBITRACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compara dos tableros, tales que el segundo es el mismo que el primero pero con una jugada más,
;; y devuelve el movimiento que lleva del primer tablero al segundo
(defun compara-tableros (viejo nuevo)
	(let ((resultado nil))
		(loop for i from 0 to *filas* do
			(loop for j from 0 to *columnas* do
				(if (not (equal (aref i j viejo) (aref i j nuevo)))
					(setf resultado (aref i j nuevo))
					nil)))
		resultado))

;; Determina si ha ganado algún jugador la partida
(defun es-estado-ganador (tablero jugador turno)
	(cond
		((not (movimientos-legales tablero))
			nil) ;; Empate
		((and (equal jugador *jugador-maquina*) (equal turno 'max))
			t)
		((and (equal jugador *jugador-humano*) (equal turno 'min))
			t)
		(t nil))) ;; En principio inalcanzable

;; Devuelve el nodo siguiente según una jugada de la IA
(defun aplica-decision (procedimiento nodo-j)
	(apply #'(lambda (x y) (first procedimiento) (list nodo-j (rest procedimiento)))))

;; Devuelve el estado siguiente según el movimiento dado por el jugador
(defun aplica-movimiento (movimiento tablero)
	(inserta-ficha-en-columna tablero movimiento *color-humano*))

;; Inserta una ficha correctamente en la columna dicha
;; Nota: Si la columna esta llena no falla
(defun inserta-ficha-en-columna (tablero columna color)
  (setf (aref tablero 
	      (primera-posicion-vacia tablero columna)
	      columna)
	color))

;; Determina si el juego ha llegado a su final
(defun es-estado-final (tablero)
	(or
		(movimientos-legales tablero)
		(cuenta-4-en-horizontal tablero *color-maquina*)
		(cuenta-4-en-horizontal tablero *color-humano*)
		(cuenta-4-en-vertical tablero *color-maquina*)
		(cuenta-4-en-vertical tablero *color-humano*)
		(cuenta-4-en-diagonal tablero *color-maquina*)
		(cuenta-4-en-diagonal tablero *color-humano*)))

;; Busca alguna secuencia horizontal del color dado de longitud mayor o igual a 4 en el tablero
(defun cuenta-4-en-horizontal (tablero color)
	(apply #'or
		(loop for x in
			(loop for i from 0 to *filas* collect
				(cuenta-fichas-consecutivas-en-secuencia (loop for j from 0 to *columnas* collect (aref tablero i j)) color))
			collect (> x 3))))

;; Busca alguna secuencia vertical del color dado de longitud mayor o igual a 4 en el tablero
(defun cuenta-4-en-vertical (tablero color)
	(apply #'or
		(loop for x in
			(loop for j from 0 to *columnas* collect
				(cuenta-fichas-consecutivas-en-secuencia (loop for i from 0 to *filas* collect (aref tablero i j)) color))
			collect (> x 3))))

;; Busca alguna secuencia diagonal del color dado de longitud mayor o igual a 4 en el tablero
(defun cuenta-4-en-diagonal (tablero color)
	(or
		(cuenta-4-en-diagonales-izquierdas tablero color)
		(cuenta-4-en-diagonales-derechas tablero color)))

;; TODO
(defun cuenta-4-en-diagonales-izquierdas (tablero color)
	(let ((i 0) (j 0))))

;; TODO
(defun cuenta-4-en-diagonales-derechas (tablero color)
	)

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
;;; ALGORITMO MINIMAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Valores máximos y mínimos para las variables alfa y beta
(defvar *minimo-valor* -9999)
(defvar *maximo-valor* 9999)

;; Para un posible nodo del árbol devuelve sus hijos
(defun sucesores (nodo-j)
  (let ((resultado ()))
    (loop for movimiento in *movimientos* do
      (let ((siguiente
             (aplica-movimiento movimiento
                                (estado nodo-j))))
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

;; Algoritmo MINIMAX con poda ALFA-BETA
(defun minimax-a-b (nodo-j profundidad
                           &optional (alfa *minimo-valor*)
                           (beta *maximo-valor*))
  (if (or (es-estado-final (estado nodo-j))
          (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                        (jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                              (jugador nodo-j)))
          (if (eq (jugador nodo-j) 'max)
              (maximizador-a-b
               (sort sucesores #'>
	:key (lambda (nodo) (f-e-estatica (estado nodo) 'min)))
               profundidad alfa beta)
              (minimizador-a-b
               (sort sucesores #'<
	:key (lambda (nodo) (f-e-estatica (estado nodo) 'max)))
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
    mejor-sucesor))

;; Devuelve una valoración heurística para un nodo (jugada)
(defun f-e-estatica (tablero jugador)
	(funcion-heuristica tablero jugador))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES HEURÍSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *heuristica1*)
(defvar *heuristica2*)

;; Front-End para las heurísticas, sirve para que f-e-estatica no tenga que preocuparse
;; sobre qué heurística es la más avanzada, siempre llama a esta función y es aquí donde
;; se escoje la mejor heurística
;; NOTA: Actualizado a heuristica-3
(defun funcion-heuristica (tablero jugador)
	(cond
		((equal jugador *jugador-maquina)
			(loop for mov in (movimientos-legales tablero) maximize
				(heuristica-3 tablero mov *color-maquina*)))
		(t
			(loop for mov in (movimientos-legales tablero) maximize
				(heuristica-3 tablero mov *color-humano*)))))
				
;; Recibe los nombres de dos funciones heurísticas y genere un fichero de texto con la partida que
;; resulta si MIN utiliza la primera heurística y MAX la segunda
(defun compara_heurs (heuristica1 heuristica2)
	(setf *procedimiento* (list 'heuristica1 *profundidad*))
	(setf *procedimiento2* (list 'heuristica2 *profundidad*))
	(setq *fichero-compara_heurs* (open "compara_heurs.txt" :direction :input))
	(crea-nodo-j-inicial 'max)
	(es-estado-final *estado-inicial*)
	(analiza-final *nodo-j-inicial*)
	(jugada-maquina-compara_heurs-2 *nodo-j-inicial*)
	(close *fichero-compara_heurs*)) ;; MAX usa la segunda heurística

;; Cuenta el numero de piezas del mismo color que hay en un rango de 3 posiciones
(defun heuristica-1 (tablero lista-valores jugador)
	(loop for pos in lista-valores count (igual-color tablero pos color)))

;; Cuenta el numero de fichas consecutivas que habría sin colocar la nuestra y le resta
;; el numero de turnos que tardaríamos en poner la ficha allí, 3 es lo máximo :D
(defun heuristica-2 (tablero posicion color)
	(-
		(fichas-consecutivas tablero posicion color)
		(minimo-turnos-ocupar-posicion tablero posicion)))

;; Devuelve la primera posicion de la lista de posiciones con una heuristica mayor
(defun mejor-eleccion (tablero heuristica posiciones color)
(let ((posicion-valor (list nil *valor-minimo*))
	(aux nil))
	(loop for x in posiciones do
		(if (< (second posicion-valor) (setf aux (apply #' heuristica tablero x color)))
			(setf posicion-valor (list x aux))))
	(first posicion-valor)))

;; Esta es la heuristica definitiva :D
;; NOTA: No se puede aplicar sobre posiciones ya ocupadas, eso aparte de ser inutil provocaría una division por 0
(defun heuristica-3  (tablero posicion color)
(let* ((listas (cuatro-en-linea-posible tablero posicion color))
	(valor-heuristico-consecutivas (heuristica-3-aux-consecutivas listas tablero posicion color))
	(valor-heuristico-multiplicador (length listas))
	(valor-heuristico-divisor (minimo-turnos-ocupar-posicion tablero posicion)))
		(if (eq valor-heuristico-consecutivas *maximo-valor*)
;; significa que si ponemos aqui una ficha ganamos
			*maximo-valor*
;; significa que multiplicaremos el numero de posibles lineas completas que podemos tener y lo dividiremos por el numero de turnos que tardaríamos en llegar nosotros a esa posicion
			(/ (* valor-heuristico-consecutivas valor-heuristico-multiplicador) 
				valor-heuristico-divisor))))

;; detecta cuando solo hay que insertar una ficha para ganar y devuelve el maximo valor heuristico para ese nodo
;; si no devuelve un numero que será mayor mientras menos fichas tengamos que insertar para conseguir 4 en linea
;; devuelve un numero
(defun heuristica-3-aux-consecutivas (listas tablero posicion color)
(if(= 3 (fichas-consecutivas tablero posicion color))
;; Si hay tres del mismo color en linea desde esa posicion hemos ganado
*maximo-valor*
(maximo 
	(loop for x in listas collect
		(mas-posibilidades-conecta-4 x)))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCIONES AUXILIARES DE LA HEURÍSTICA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *fichero-compara_heurs*)
(defvar *procedimiento2*)

;; Función llamada cuando es el turno de la máquina de la heurística 1 en compara_heurs
(defun jugada-maquina-compara_heurs-1 (nodo-j)
	(escribe-nodo-j *fichero-compara_heurs* nodo-j)
	(format *fichero-compara_heurs* "~%Turno heurística 1.~&")
	(let ((siguiente (aplica-decision *procedimiento* nodo-j)))
		(if (es-estado-final (estado siguiente))
			(analiza-final siguiente *fichero-compara_heurs*)
			(jugada-maquina-compara_heurs-2 siguiente))))

;; Función llamada cuando es el turno de la máquina de la heurística 2 en compara_heurs
(defun jugada-maquina-compara_heurs-2 (nodo-j)
	(escribe-nodo-j *fichero-compara_heurs* nodo-j)
	(format *fichero-compara_heurs* "~%Turno heurística 2.~&")
	(let ((siguiente (aplica-decision *procedimiento2* nodo-j)))
		(if (es-estado-final (estado siguiente))
			(analiza-final siguiente *fichero-compara_heurs*)
			(jugada-maquina-compara_heurs-1 siguiente))))

;; Mínimo de turnos que necesitaremos para ocupar esa posición
(defun minimo-turnos-ocupar-posicion (tablero posicion)
	(loop for i from *filas* downto (first posicion) count
		(eq (aref tablero i (second posicion)) nil)))

;; Devuelve una lista de listas de posiciones posibles en las que colocando
;; una ficha del color apropiado se podria hacer cuatro en linea
(defun cuatro-en-linea-posible (tablero posicion color)
	(loop for x in (todas-posiciones-posibles tablero posicion color)
		when (< 2 (length x)) collect x))

;; Nos indica el numero de fichas del mismo color que ya hay en una lista, lista es una de las listas resultantes de cuantro-en-linea-posible
;; mientras mayor sea el numero menos fichas habra que insertar para ganar
(defun mas-posibilidades-conecta-4 (lista)
(- (length lista)
(loop for x in lista count (not (null x)))))

;; Devuelve una lista de posiciones posibles en las que insertar una ficha de
;; nuestro color incrementaria el numero de fichas consecutivas
(defun todas-posiciones-posibles (tablero posicion color)
	(loop for x in (rango-posiciones (first posicion) (second posicion))
		collect
			(posiciones-cuatro-en-linea tablero x color)))

;; Devuelve todas las posiciones no ocupadas del tablero
(defun posiciones-posibles (tablero)
	(loop for i from 0 to *columnas*
		append 
		(loop for j from 0 to *filas* 
			until (not (null (aref tablero j i)))
			collect (list j i))))

;; Devuelve sólo las posiciones consecutivas accesibles que conectan con nuestro color
;; accesibles significa libres y no cortadas por otro color
(defun posiciones-cuatro-en-linea (tablero rango color)
	(loop for posiciones in rango
	append
	(loop for x in posiciones
		until (not (or
			(eq (aref tablero (first x) (second x)) color)
			(eq (aref tablero (first x) (second x)) nil)))
		collect
		(if (eq (aref tablero (first x)(second x)) nil)
		x
		nil))))

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
	(apply #'max 
;; tenemos que filtrar los nil ya que max no los reconoce
	(loop for x in lista when (not (null x)) collect x)))

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
