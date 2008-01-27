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
	(format canal "~%Estado :")
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
;; (format t "tablero a pelo ~a "a) ;; DEBUG
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
	  (format t "|~a ~%" i)
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
    (setf *ultimo-movimiento* (compara-tableros (estado nodo-j) (estado siguiente))) ;; Elección de la máquina
    (if (es-estado-final (estado siguiente))
        (analiza-final siguiente)
        (jugada-humana siguiente))))

;; Devuelve para un determinado estado qué movimientos son posibles
(defun movimientos-legales (estado)
  (loop for m in *movimientos*
        when (primera-posicion-vacia estado m)
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
(defun es-estado-ganador (tablero jugador turno)
	(cond
		((not (movimientos-legales tablero))
			nil) ;; Empate
		((and (equal jugador *jugador-maquina*) 
			(equal turno 'max)) 
			t) ;; Gana máquina
		((and (equal jugador *jugador-humano*) 
			(equal turno 'min))
			t) ;; Gana humano
		(t nil))) ;; En principio inalcanzable

;; Devuelve el nodo siguiente según una jugada de la IA
(defun aplica-decision (procedimiento nodo-j)
	(funcall (symbol-function (first procedimiento)) nodo-j (first(rest procedimiento))))

;; Devuelve el estado siguiente según el movimiento dado por el jugador, sin alterar el tablero original
(defun aplica-movimiento (columna tablero color)
	(let ((fila (primera-posicion-vacia tablero columna))
		 (nuevo-tablero (duplica-tablero tablero)))
		(cond ((null fila)
			nil)
			(t
				(setf (aref nuevo-tablero fila columna) color)
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
(if (null *ultimo-movimiento*)
nil
	(or
		(<= (length (movimientos-legales tablero)) 0)
		(> (fichas-consecutivas-con-centro tablero *ultimo-movimiento* *color-humano*) 3)
		(> (fichas-consecutivas-con-centro tablero *ultimo-movimiento* *color-maquina*) 3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITMO MINIMAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Valores máximos y mínimos para las variables alfa y beta
(defvar *minimo-valor* -9999)
(defvar *maximo-valor* 9999)

;; Para un posible nodo del árbol devuelve sus hijos
(defun sucesores (nodo-j)
  (format t "~%DEBUG - Calculando sucesores")	;; DEBUG
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
  (format t "~%DEBUG - Entrando en minimax-a-b")	;; DEBUG
  (if (or (es-estado-final (estado nodo-j)) (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                        (jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
	   (format t "~%DEBUG - Calculados los sucesores ;)")	;; DEBUG
	   (format t "~%DEBUG - Tablero original: ")	;; DEBUG
	   (imprime-tablero (estado nodo-j))	;; DEBUG
	   (format t "~%DEBUG - Tableros sucesores: ")	;; DEBUG
	   (loop for x in sucesores do	;; DEBUG
		(imprime-tablero (estado x))	;; DEBUG
		(format t "~%DEBUG - Valor heurístico sucesor: ~a~&" (f-e-estatica (estado x) (jugador x))))	;; DEBUG
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
	(format t "~%DEBUG - Mejor sucesor maximizador: ") ;; DEBUG
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
	(format t "~%DEBUG - Mejor sucesor minimizador: ") ;; DEBUG
	(imprime-tablero (estado mejor-sucesor)) ;; DEBUG
    mejor-sucesor))

;; Devuelve una valoración heurística para un nodo (jugada)
(defun f-e-estatica (tablero jugador)
	(cond
		((equal jugador *jugador-maquina*)
			(loop for mov in (movimientos-legales tablero) summing
				(heuristica-4 tablero (list (primera-posicion-vacia tablero mov) mov) *color-maquina*)))
		((equal jugador *jugador-humano*)
			(loop for mov in (movimientos-legales tablero) summing
				(heuristica-4 tablero (list (primera-posicion-vacia tablero mov) mov) *color-humano*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES HEURÍSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables para compara_heurs
(defvar *heuristica1*)
(defvar *heuristica2*)

;; TODO - Peta infinito xD
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

;; Esta es la heuristica definitiva :D
;; NOTA: No se puede aplicar sobre posiciones ya ocupadas, eso aparte de ser inutil provocaría una division por 0
(defun heuristica-3  (tablero posicion color)
(let* ((listas (cuatro-en-linea-posible tablero posicion color))
	(valor-heuristico-consecutivas (heuristica-3-aux-consecutivas listas tablero posicion color))
	(valor-heuristico-multiplicador (length listas)))
;; 	(format t "~% listas posibles ~a ~%" listas) ;;debug
;; 	(format t "consecutivas ~a " valor-heuristico-consecutivas) ;;debug
;; 	(format t "numero de posibles conecta-4 ~a ~%" valor-heuristico-multiplicador) ;;debug
		(if (eq valor-heuristico-consecutivas *maximo-valor*)
;; significa que si ponemos aqui una ficha ganamos
			*maximo-valor*
;; significa que multiplicaremos el numero de posibles lineas completas que podemos tener y lo dividiremos por el numero de turnos que tardaríamos en llegar nosotros a esa posicion
			(* valor-heuristico-consecutivas valor-heuristico-multiplicador))))

;; detecta cuando solo hay que insertar una ficha para ganar y devuelve el maximo valor heuristico para ese nodo
;; si no devuelve un numero que será mayor mientras menos fichas tengamos que insertar para conseguir 4 en linea
;; devuelve un numero
(defun heuristica-3-aux-consecutivas (listas tablero posicion color)
	(cond
		((= 3 (fichas-consecutivas tablero posicion color))
		;; Si hay tres del mismo color en linea desde esa posicion hemos ganado
			*maximo-valor*)
		((null listas) 
		;; Si no hay donde poner no pongas
			*minimo-valor*)
		(t
		;; Te quedas con el de la heuristica mejor
			(maximo 
			(loop for x in listas collect
				(mas-posibilidades-conecta-4 x))))))

;; TODO - No funciona como debiera
;; Heuristica 4 corta la jugada del contrario
(defun heuristica-4 (tablero posicion color)
;; (max 
	(heuristica-3 tablero posicion color)
	)
;; 	(heuristica-4-aux tablero posicion (contrincante color))))

(defun heuristica-4-aux (tablero posicion color)
(let ((valor (heuristica-3 tablero posicion color)))
	(cond 
		((eq valor *maximo-valor*) 
			(- 1 *maximo-valor*)) ;; Da prioridad a ganar Yo a que gane él
		(t
			valor))))

(defun contrincante (color)
(if (eq color *color-humano*)
	*color-maquina*
	*color-humano*))

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
;; (format t "~%maximo fichas consecutivas ~a" (maximo-consecutivos lista)) ;;debug
;; (format t "fichas ocupadas de ~a son ~a ~%" lista (fichas-ocupadas lista)) ;;debug
(* (maximo-consecutivos lista) (fichas-ocupadas lista)))

(defun fichas-ocupadas (lista)
(- (length lista)
(loop for x in lista count (not (null x)))))

(defun maximo-consecutivos (lista)
(let ((max 0)
	(aux 0))
	(loop for x in lista do
		(if (null x)
			(if (<= max aux)
			(setf max (setf aux (+ 1 aux)))
			(setf aux (+ 1 aux)))
		(setf aux 0)))
	max))

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

(defun fichas-consecutivas-con-centro (tablero posicion color)
(maximo 
	(loop for x in 
		(rango-posiciones (first posicion) (second posicion)) 
		collect 
			(cuenta-fichas-consecutivas-con-centro
				(recorre-posiciones tablero x) tablero posicion color))))

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

(defun cuenta-fichas-consecutivas-con-centro (secuencias tablero posicion color)
	(+
		(loop for x in (first secuencias) count (eq x color) until (not(eq x color)))
		(if (eq color (aref tablero (first posicion) (second posicion)))
			1
			0)
		(loop for x in (second secuencias) count (eq x color) until (not(eq x color)))))

;; Devuelve el maximo entero de la lista, y si la lista es vacía devuelve 0
(defun maximo (lista)
	(if  (null lista)
		0
		(apply #'max
			(loop for x in lista when (not (null x)) collect x)))) ;; tenemos que filtrar los nil ya que max no los reconoce

;; Devuelve la fila de la primera casilla vacía de la columna
(defun primera-posicion-vacia (tablero columna)
(if (and (<= columna *columnas*)(posicion-vacia tablero 0 columna))
    	(loop for i from *filas* downto 0
	  minimize i
	  until (posicion-vacia tablero i columna))
	nil))

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
