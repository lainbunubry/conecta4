;;     A four in line implementation in LISP using the minimax algorithm
;;     Copyright (C) 2008 Alejandro Blanco Escudero, Manuel Gomar Acosta
;; 
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;; 
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;; 
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRABAJO IA1 - CONECTA 4   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alejandro Blanco Escudero ;;
;; Manuel Gomar Acosta       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Se ha evitado escribir tildes a proposito, para evitar los posibles conflictos que pueden
;; provocar al ser caracteres especiales

;; Interfaz de la aplicacion, permite escoger de una forma comoda todas las posibilidades del juego
;;Didactico
(defun menu ()
  (let ((salir nil) (opcion 0) (heur1 nil) (heur2 nil) (prof 3) (ab 0) (emp 0))
    (loop until salir do
          (format t "~&~%MENU - CONECTA 4~%+-+-+-+-+-+-+-+-+~%~%")
          (format t "Elija la opcion que desee:~%")
          (format t "1.- Jugar contra la maquina~%")
          (format t "2.- Comparar dos heuristicas~%")
          (format t "3.- Salir~%~%Su eleccion: ")
          (setf opcion (read))
          (cond 
           ((= opcion 1)
            (format t "~&~%Introduzca la profundidad deseada para el algoritmo minimax: ")
            (setf prof (read))
            (format t "~&~%Escoja la version del algoritmo que desee:~%")
            (format t "1.- Minimax normal empezando el jugador~%")
            (format t "2.- Minimax con poda alfa-beta empezando el jugador~%")
            (format t "3.- Minimax normal empezando la maquina~%")
            (format t "4.- Minimax con poda alfa-beta empezando la maquina~%~%Su eleccion: ")
            (setf ab (read))
            (cond
             ((= ab 1)
              (juego :procedimiento (list 'minimax prof)))
             ((= ab 2)
              (juego :procedimiento (list 'minimax-a-b prof)))
             ((= ab 3)
              (juego :procedimiento (list 'minimax prof) :empieza-la-maquina? t))
             ((= ab 4)
              (juego :procedimiento (list 'minimax-a-b prof) :empieza-la-maquina? t))
             (t
              (format t "~&~%Opciones erroneas, por favor escoja de nuevo"))))
           ((= opcion 2)
            (format t "~&~%Las heuristicas disponibles son:~%~%")
            (format t "heuristica-1~%heuristica-2~%heuristica-3~%heuristica-4~%~%")
            (format t "Nombre de la primera heuristica: ")
            (setf heur1 (read))
            (format t "~&~%Nombre de la segunda heuristica: ")
            (setf heur2 (read))
            (format t "~&~%Introduzca la profundidad deseada para el algoritmo minimax: ")
            (setf prof (read))
            (format t "~&~%Elija que heuristica desea que empiece la partida:~%")
            (format t "1.- ~a~%2.- ~a~%~%Su eleccion: " heur1 heur2)
            (setf emp (read))
            (cond
             ((= emp 1)
              (format t "~&~%Comienza la partida:~%")
              (compara_heurs heur2 heur1 prof)) ;; Empieza la segunda que es MAX
             ((= emp 2)
              (format t "~&~%Comienza la partida:~%")
              (compara_heurs heur1 heur2 prof)) ;; Empieza la primera que es MAX
             (t
              (format t "~&~%Opciones erroneas, por favor escoja de nuevo"))))
           ((= opcion 3)
            (setf salir t))
           (t
            (format t "~&~%Opcion invalida, por favor escoja de nuevo"))))))

;; Interfaz del juego
(defun competicion ()
  (let ((salir nil) (opcion 0))
    (loop until salir do
          (format t "~&~%MENU - CONECTA 4~%+-+-+-+-+-+-+-+-+~%~%")
          (format t "Elija la opcion que desee:~%")
          (format t "1.- Empieza la maquina~%")
          (format t "2.- Empieza contrincante~%")
          (format t "3.- Salir~%~%Su eleccion: ")
          (setf opcion (read))
          (cond 
           ((= opcion 1)
		 (juego :procedimiento (list 'minimax-a-b 3) :empieza-la-maquina? t))
   	   ((= opcion 2)
		 (juego :procedimiento (list 'minimax-a-b 3)))
	   ((= opcion 3)
            (setf salir t))
           (t
            (format t "~&~%Opcion invalida, por favor escoja de nuevo"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPRESENTACIoN DE ESTADOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Se ha elegido una representacion a partir de una matriz, en donde las fichas
;; de cada bando se representan por X u O respectivamente, y las casillas vacias
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
(defvar *ultimo-movimiento* '(0 0)) ;; ultima posicion donde se ha echado una ficha

;; Estructura que representa un nodo del arbol de busqueda
(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nil)
                   (:print-function escribe-nodo-j))
  estado 		;; Tablero modificado
  jugador
  valor) 		;; Valor heuristico de la nueva jugada

;; Funcion que muestra por pantalla (u otro canal) el nodo dado
(defun escribe-nodo-j (nodo-j &optional (canal t))
  (format canal "~%Estado :~%")
  (imprime-tablero (estado nodo-j) canal)
  (format canal "~%ultimo movimiento : ~a" *ultimo-movimiento*))
;; 	(format canal "~%Jugador : ~a" (jugador nodo-j)))

;; Funcion que inicializa *nodo-j-inicial*
(defun crea-nodo-j-inicial (jugador)
  (setf *estado-inicial* (make-array '(6 7)))
  (setf *nodo-j-inicial*
        (crea-nodo-j :estado *estado-inicial*
                     :jugador jugador)))

;; Muestra por pantalla el contenido de un tablero
(defun imprime-tablero (a &optional (canal t))
  (let* ((dim (array-dimensions a))
	 (f (first dim))
	 (c (second dim)))
    (format canal "~%  0   1   2   3   4   5   6~%")
    (escribe-linea-aux c canal)
    (loop for i from 0 to (- f 1)
	  do (loop for j from 0 to (- c 1)
		   do (if (equal (aref a i j) NIL)
			  (format canal "|   ")
                          (format canal "| ~a " (aref a i j))))
	  (format canal "| ~a~%" i)
	  (escribe-linea-aux c canal))))

;; Genera una linea del tablero a mostrar
(defun escribe-linea-aux (col canal)
  (loop for i from 0 to (- col 1)
        do (format canal "+---"))
       (format canal "+~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARBITRACIoN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variable con la informacion del algoritmo a usar
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
  (escribe-nodo-j nodo-j-final canal)
  (cond ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'min)
         		(format canal "~&La maquina ha ganado"))
        ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'max)
         (format canal "~&El humano ha ganado"))
        (t (format canal "~&Empate"))))

;; Funcion llamada cuando es el turno de la maquina
(defun jugada-maquina (nodo-j)
  (escribe-nodo-j nodo-j)
  (format t "~%Mi turno.~&")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
    (setf *ultimo-movimiento* (compara-tableros (estado nodo-j) (estado siguiente)))
    (if (es-estado-final (estado siguiente))
        (analiza-final siguiente)
        (jugada-humana siguiente))))

;; Devuelve para un determinado estado que movimientos son posibles
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
              (format t "~%   Col ~a (Tecla ~a)" m m)
              (format t "   Col ~a (Tecla ~a)" m m))
          (setf numero (+ numero 1)))))

;; Funcion llamada cuando es el turno del humano
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
      (cond ((and (integerp m) (member m movimientos))
             (let ((nuevo-estado
                    (aplica-movimiento (nth m *movimientos*) (estado nodo-j) *color-humano*)))
               (cond (nuevo-estado
                      (let ((siguiente (crea-nodo-j
					:estado nuevo-estado
					:jugador 'max))) 
                        (setf *ultimo-movimiento* (compara-tableros (estado nodo-j) (estado siguiente))) ;;Eleccion del humano
                        (if (es-estado-final nuevo-estado)
                            (analiza-final siguiente)
                            (jugada-maquina siguiente))))
                     (t (format t "~&   El movimiento ~a no se puede usar. " m)
			(jugada-humana nodo-j)))))
            (t (format t "~&   ~a es ilegal. " m)
               (jugada-humana nodo-j))))))

;; Funcion que se llama cuando se pide consejo a la maquina
(defun solicitar-consejo (nodo-j)
  (format t "Pensando")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
    (format t " - Mi recomendacion: ~a" (second (compara-tableros
							(estado nodo-j)
							(estado siguiente))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES AUXILIARES DE ARBITRACIoN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compara dos tableros, tales que el segundo es el mismo que el primero pero con una jugada mas,
;; y devuelve el movimiento que lleva del primer tablero al segundo
(defun compara-tableros (viejo nuevo)
  (let ((resultado nil))
    (loop for i from 0 to *filas* do
          (loop for j from 0 to *columnas* do
                (if (not (equal (aref viejo i j) (aref nuevo i j)))
                    (setf resultado (list i j))
                    nil)))
    resultado))

;; Determina si ha ganado algun jugador la partida
;; Hay que tener en cuenta que se analiza un nodo para un jugador que ya ha echado su ficha, por eso todo
;; parece pensado para su contrincante
(defun es-estado-ganador (tablero jugador turno)
	(if (es-estado-final tablero)
		(cond			
		 ((and (equal jugador *jugador-humano*)
                              (equal turno 'min))
                         t) ;; Gana maquina
		 ((and (equal jugador *jugador-maquina*)
                              (equal turno 'max))
                         t) ;; Gana humano
		((not (movimientos-legales tablero))
                  nil) ;; Empate
		 (t nil))
		nil))

;; Comprueba si la ficha de la posicion dada es del color dado
(defun mismo-color (tablero posicion color)
  (if (eq (aref tablero (first posicion) (second posicion)) color)
      t 
      nil))

;; Devuelve el nodo siguiente segun una jugada de la IA
(defun aplica-decision (procedimiento nodo-j)
  (funcall (symbol-function (first procedimiento)) nodo-j (first (rest procedimiento))))

;; Devuelve el estado siguiente segun el movimiento dado por el jugador, sin alterar el tablero original
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

;; De una lista de listas de fichas devuelve el numero maximo de fichas consecutivas encontrados
(defun maximo-conecta-4 (listas)
  (if (listp listas)
      (maximo
       (loop for x in listas when (> (length x) 3) collect
             (cuenta-fichas-consecutivas x)))
      0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITMO MINIMAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Valores maximos y minimos para las variables alfa y beta
(defvar *minimo-valor* -10080)
(defvar *maximo-valor* 10080)
(defvar *medio-valor* 720) ;;porque son valores facilmente divisible 2*3*4*5*6

;; Para un posible nodo del arbol devuelve sus hijos
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

;; Funcion que busca maximizar (MAX) la puntuacion
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

;; Funcion que busca minimizar (MIN) la puntuacion
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

;; Funcion que busca maximizar (MAX) la puntuacion con ALFA-BETA
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

;; Funcion que busca minimizar (MIN) la puntuacion con ALFA-BETA
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

;; Devuelve una valoracion heuristica para un nodo (jugada)
;; Parece que no tenga sentido comprobar las posiciones para el color del jugador contrario, pero al igual
;; que es-estado-ganador o analiza-final resulta que el jugador que recibimos como parametro no es otro que
;; el del ultimo nodo creado, un nodo sucesor del cual queremos conocer su heuristica pero para el jugador
;; que echo ultimo, es decir, el jugador anterior
(defun f-e-estatica (tablero jugador)
  (cond
   ((es-estado-ganador tablero jugador 'max) *minimo-valor*) ;; tenemos que ver si gana nuestro oponente
   ((es-estado-ganador tablero jugador 'min) (* *columnas* *maximo-valor*)) ;; ganamos!!!!
   ((equal jugador *jugador-maquina*)
    (loop for posicion in (posiciones-heuristicas tablero) summing
          (heuristica-4 tablero posicion *color-humano*)))
   ((equal jugador *jugador-humano*)
    (loop for posicion in (posiciones-heuristicas tablero) summing
          (heuristica-4 tablero posicion *color-maquina*)))))

;; Nota: Por algun motivo, cuando se trata de profundidades pares en el algoritmo minimax,
;; la IA se comporta de una forma muy ineficaz. Hemos comprobado experimentalmente que
;; para profundidades impares funciona decentemente pero para las pares juega muy mal.

;; Devuelve la lista de posiciones adecuadas por la cual se va a valorar el tablero
(defun posiciones-heuristicas (tablero)
  (loop for i from 0 to *columnas* collect
        (if (null (primera-posicion-vacia tablero i))
            (primera-posicion-ocupada tablero i)
            (primera-posicion-vacia tablero i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES HEURiSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nucleo base de la heuristica, valora cada posibilidad por separado y suma
;; los resultados
(defun nucleo  (tablero posicion color)
  (loop for x in (rango-accesible tablero posicion color) 
        when (> (length x) 3) 
        summing
        (nucleo-aux 
         (distancia-minima x posicion) 
         (cuenta-fichas-consecutivas x) 
         (cuenta-fichas x)
         (centrado (second posicion)))))

;; Calcula el valor de cada una de las posibilidades (representadas por una
;; secuencia de posiciones y nil) Devuelve una valoracion adecuada.
(defun nucleo-aux (distancia consecutivas fichas centrado)
  (cond
   ((= 1 consecutivas) ;;Tablero vacio
    centrado)
   ((< 3 consecutivas)
    ;; Si hay tres del mismo color en linea desde esa posicion hemos ganado
    (/ *maximo-valor* distancia))
   ((= 3 consecutivas)
    ;; 	(* (/ *medio-valor* (max 1 distancia)) fichas))
    (/ *medio-valor* distancia))
   ;; 		da mucha prioridad a cuando tienes dos consecutivas
   (t
    (* (- (* *columnas* fichas ) distancia)))))

;; Nucleo base de la heuristica para el contrincante
;; valora mucho mas los movimientos peligrosos del contrario
;; para evitar que gane a toda costa

(defun nucleo-contrincante (tablero posicion color)
  (loop for x in (rango-accesible tablero posicion (contrincante color) )
        when (> (length x) 3) 
        summing
        (nucleo-contrincante-aux 
         (distancia-minima x posicion) 
         (cuenta-fichas-consecutivas x) 
         (cuenta-fichas x)
         (centrado (second posicion)))))

;; Calcula el valor de cada una de las posibilidades (representadas por una
;; secuencia de posiciones y nil) Devuelve una valoracion adecuada.
(defun nucleo-contrincante-aux (distancia consecutivas fichas centrado)
  (cond
   ((= 1 consecutivas) ;;Tablero vacio
    centrado)
   ((< 3 consecutivas)
    ;; Si hay tres del mismo color en linea desde esa posicion hemos ganado
    (/ (* *columnas* *minimo-valor*) distancia))
   ((= 3 consecutivas)
    ;; 	(* (/ *medio-valor* (max 1 distancia)) fichas))
    (* -1 (/ (* *columnas* *medio-valor*) distancia)))
   ;; 		da mucha prioridad a cuando tienes dos consecutivas
   (t
    (* -1 (- (* *columnas* fichas) distancia)))))


;; Heuristica aleatoria
(defun heuristica-1 (tablero lista-valores jugador)
  (random 100))

;; Primer intento de heuristica, solo tiene en cuenta el numero de
;; fichas consecutivas
(defun heuristica-2 (tablero posicion color)
  (loop for x in (rango-accesible tablero posicion color)
        when (> (length x) 3) 
        summing
        (cuenta-fichas-consecutivas x)))

;; Valora adecuadamente si el tablero es beneficioso para nosotros
(defun heuristica-3 (tablero posicion color)
  (nucleo tablero posicion color))

;; Mejora de la heuristica que ahora tiene en cuenta los movimientos
;; peligroso de nuestro contrincante
(defun heuristica-4 (tablero posicion color)
  (let 
      ((heuristica-favor (nucleo tablero posicion color))
       (heuristica-contra (nucleo-contrincante tablero posicion color))) ;; Le da menos prioridad a ganar el
    (if (> heuristica-favor (abs heuristica-contra))
     heuristica-favor 
     heuristica-contra)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCIONES AUXILIARES DE LA HEURiSTICA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cuenta el maximo de fichas consecutivas de de una secuencia
;; una ficha esta representada por su posicion (i j)
;; un hueco se representa por un nil
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

;; cuenta el numero de elementos no nulos (fichas) en una secuencia
(defun cuenta-fichas (secuencia)
  (loop for x in secuencia count (not (null x))))

;; Devuelve el maximo entero de la lista, y si la lista es vacia devuelve 0
(defun maximo (lista)
  (if  (null lista)
      0
      (apply #'max
             (loop for x in lista when (not (null x)) collect x)))) ;; Hay que filtrar los nil ya que max no los reconoce

;; Devuelve la posicion de la primera casillla ocupada de la columna
(defun primera-posicion-ocupada (tablero columna)
  (let ((fila 
         (loop for i from 0 to *filas* until (aref tablero i columna) count t)))
    (if (> fila *filas*)
	nil
	(list fila columna))))

;; Devuelve la posicion de la primera casillla vacia de la columna
(defun primera-posicion-vacia (tablero columna)
  (let (( fila 
          (- *filas* (loop for i from *filas* downto 0 
                           until (null (aref tablero i columna)) count t))))
    (if (> 0 fila)
	nil (list fila columna))))

;; Nos devuelve el contrincante del color que le pasemos
(defun contrincante (color)
  (if (eq color *color-humano*)
      *color-maquina*
      *color-humano*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES DE RANGOS DE VALORES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Genera una secuencia con el rango de valores accesibles desde una posicion
;; en el tablero y que conecten con un color

;; nos devuelve la distancia minima en columnas de una lista de posiciones a una
;; posicion dada
(defun distancia-minima (lista pos)
  (loop for x in lista when (not (null x)) minimize (distancia x pos)))

;; Nos devuelve la distancia entre dos posiciones (x y) (a b) abs (y -a)
(defun distancia (posx posy)
 (cond 
  ((equal posx posy) *columnas*)
  ((eq (second posx) (second posy)) 1)
  (t (abs (- (second posx) (second posy))))))

(defun centrado (columna)
(if (< columna (/ *columnas* 2))
(mod columna (/ *columnas* 2))
(- *columnas* columna)))

;; Esta funcion de rango en la encargada de dada una posion devolver todas posiciones
;; interesantes y alcanzables desde el punto de vista analitico para nuestro juego.
;; mira si la posicion es accesible y si no la corta ninguna ficha de otro color
;; devuelve una lista de listas de posiciones donde se encuentran nuestras fichas y
;; de nil que representan los huecos que hay entre nuestras posiciones
(defun rango-accesible (tablero pos color)
(if (or (null (aref tablero (first pos) (second pos))) (eq (aref tablero (first pos) (second pos)) color))
  (loop for x in
	(list 
         (seccion-fila-accesible tablero (first pos) (second pos) color) 
         (seccion-columna-accesible tablero (first pos) (second pos) color) 
         (seccion-diagonal-izq-accesible tablero (first pos) (second pos) color) 
         (seccion-diagonal-der-accesible tablero (first pos) (second pos) color))
	when (< 3 (length x)) collect x) ;; filtro que tenga longitud minimo de 4
   nil))

;; Funcion que dice si la posicion inferior esta ocupada o no
(defun inacesible (tablero f c)
  (if (pos-invalida (+ f 1) c)
      nil ;;tamos en el fondo del tablero
      (null (aref tablero (+ f 1) c))))

;; Funcion de corte,devuelve T solo si es distinto color
(defun corte (x y)
  (not (or (eq x y) (null x))))

;; Devuelve t para una posicion invalida en la matriz
(defun pos-invalida (f c)
  (or
   (> 0 f)
   (> 0 c)
   (> f *filas*)
   (> c *columnas*)))

;; devuelve la fila en la que se encuentra nuestra ficha
(defun seccion-fila-accesible (tablero f c color)
  (append 
   (reverse 
    (loop for i from (- c 1) downto 0  ;;tiene en cuenta el centro
          until 
          (or 
           (corte (aref tablero f i) color) 
           (inacesible tablero f i))
          collect
          (if (null (aref tablero f i))
              nil
              (list f i))))
   (list (list f c)) ;; contamos el centro como una ficha de nuestro color	
   (loop for i from (+ 1 c) to *columnas* 
         until 
         (or 
          (corte (aref tablero f i) color) 
          (inacesible tablero f i))
         collect
         (if (null (aref tablero f i))
             nil
             (list f i)))))

;; devuelve la columna en la que se encuentra nuestra ficha
(defun seccion-columna-accesible (tablero f c color)
  (append 
   (loop for i from 0 to (- f 1) until (corte (aref tablero i c) color)
         collect
         (if (null (aref tablero i c))
             nil
             (list i c)))
   (list (list f c)) ;; contamos el centro como una ficha de nuestro color	
   ;; las posiciones arriba no estan ocupadas
   (loop for i from (+ 1 f) to *filas* until (corte (aref tablero i c) color)
         collect
         (if (null (aref tablero i c))
             nil
             (list i c)))))

;; devuelve la diagonal izquierda en la que se encuentra nuestra ficha
(defun seccion-diagonal-izq-accesible (tablero f c color) 
  (append
   (reverse ;; tiene que estar al reves
    (loop for i from 1 to (max *filas* *columnas*)  ;;tiene en cuenta el centro
          until (or 
                 (pos-invalida (- f i) (- c i)) 
                 (corte (aref tablero (- f i) (- c i)) color) 
                 (inacesible tablero (- f i) (- c i))) 
          collect
          (if (null (aref tablero  (- f i) (- c i)))
              nil
              (list(- f i) (- c i)))))
   (list (list f c));; contamos el centro como una ficha de nuestro color
   (loop for i from 1 to *filas* 
         until (or 
                (pos-invalida (+ f i) (+ c i)) 
                (corte (aref tablero (+ f i) (+ c i)) color) 
                (inacesible tablero (+ f i) (+ c i))) 
         collect
         (if (null (aref tablero  (+ f i) (+ c i)))
             nil
             (list (+ f i) (+ c i))))))

;; devuelve la diagonal derecha en la que se encuentra nuestra ficha
(defun seccion-diagonal-der-accesible (tablero f c color) 
  (append
   (reverse ;; tiene que estar al reves
    (loop for i from 1 to *columnas* ;; tiene en cuenta el centro
          until (or 
                 (pos-invalida (+ f i) (- c i)) 
                 (corte (aref tablero (+ f i) (- c i)) color) 
                 (inacesible tablero (+ f i) (- c i))) 
          collect
          (if (null (aref tablero  (+ f i) (- c i)))
              nil
              (list (+ f i) (- c i)))))
   (list (list f c));; contamos el centro como una ficha de nuestro color
   (loop for i from 1 to *filas* 
         until (or 
                (pos-invalida (- f i) (+ c i)) 
                (corte (aref tablero (- f i) (+ c i)) color) 
                (inacesible tablero (- f i) (+ c i))) 
         collect
         (if (null (aref tablero  (- f i) (+ c i)))
             nil
             (list (- f i) (+ c i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES PARA COMPARAR HEURiSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables para compara_heurs
(defvar *fichero-compara_heurs* "compara_heurs.txt")
(defvar *procedimiento2*)


;; Recibe los nombres de dos funciones heuristicas y genere un fichero de texto con la partida que
;; resulta si MIN utiliza la primera heuristica y MAX la segunda
;; NOTA no funciona, gana siempre la que pones primera
(defun compara_heurs (heuristica1 heuristica2 profundidad)
  (setf *procedimiento* (list 'minimax-a-b-ch profundidad heuristica1))
  (setf *procedimiento2* (list 'minimax-a-b-ch profundidad heuristica2))
  (with-open-file (str *fichero-compara_heurs* :direction :output :if-exists :supersede)
                  (crea-nodo-j-inicial 'max)
                  (if (es-estado-final *estado-inicial*)
                      (analiza-final-ch *nodo-j-inicial* str)
                      (jugada-maquina-ch2 *nodo-j-inicial* str)))) ;; MAX usa la segunda heuristica

;; Funcion llamada cuando es el turno de la maquina de la heuristica 1 en compara_heurs
;; Juega con *color-humano*
(defun jugada-maquina-ch1 (nodo-j canal)
  (escribe-nodo-j nodo-j canal)
  (format canal "~%___________________________________________________~%")
  (format canal "~%Turno: ~a~%" (third *procedimiento*))
  (format t "~&Turno: ~a.~%" (third *procedimiento*))
  (let ((siguiente (aplica-decision-ch *procedimiento* nodo-j)))
    (setf *ultimo-movimiento* (compara-tableros (estado nodo-j) (estado siguiente)))
    (if (es-estado-final (estado siguiente))
        (analiza-final-ch siguiente canal)
        (jugada-maquina-ch2 siguiente canal))))

;; Funcion llamada cuando es el turno de la maquina de la heuristica 2 en compara_heurs
;; Juega con *color-maquina*
(defun jugada-maquina-ch2 (nodo-j canal)
  (escribe-nodo-j nodo-j canal)
  (format canal "~%___________________________________________________~%")
  (format canal "~%Turno: ~a.~%" (third *procedimiento2*))
  (format t "~&Turno: ~a.~%" (third *procedimiento2*))
  (let ((siguiente (aplica-decision-ch *procedimiento2* nodo-j)))
    (setf *ultimo-movimiento* (compara-tableros (estado nodo-j) (estado siguiente)))
    (if (es-estado-final (estado siguiente))
        (analiza-final-ch siguiente canal)
        (jugada-maquina-ch1 siguiente canal))))

;; Devuelve el nodo siguiente segun una jugada de la IA para compara_heurs
(defun aplica-decision-ch (procedimiento nodo-j)
  (funcall (symbol-function (first procedimiento)) nodo-j (first (rest procedimiento)) (second (rest procedimiento))))

;; Algoritmo MINIMAX con poda ALFA-BETA para compara_heurs
(defun minimax-a-b-ch (nodo-j profundidad heuristica
                              &optional (alfa *minimo-valor*)
                              (beta *maximo-valor*))
  (if (or (es-estado-final (estado nodo-j)) (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica-ch (estado nodo-j)
                                           (jugador nodo-j) heuristica))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica-ch (estado nodo-j)
                                                 (jugador nodo-j) heuristica))
            (if (eq (jugador nodo-j) 'max)
                (maximizador-a-b
                 (sort sucesores #'> :key (lambda (nodo) (f-e-estatica-ch (estado nodo) 'min heuristica)))
                 profundidad alfa beta)
                (minimizador-a-b
                 (sort sucesores #'< :key (lambda (nodo) (f-e-estatica-ch (estado nodo) 'max heuristica)))
                 profundidad alfa beta))))))

;; Devuelve una valoracion heuristica para un nodo (jugada) para compara_heurs
;; Parece que no tenga sentido comprobar las posiciones para el color del jugador contrario, pero al igual
;; que es-estado-ganador o analiza-final resulta que el jugador que recibimos como parametro no es otro que
;; el del ultimo nodo creado, un nodo sucesor del cual queremos conocer su heuristica pero para el jugador
;; que echo ultimo, es decir, el jugador anterior
(defun f-e-estatica-ch (tablero jugador heuristica)
  (cond
    ((es-estado-ganador tablero jugador 'max) *minimo-valor*) ;; Vemos si gana nuestro contrincante
    ((es-estado-ganador tablero jugador 'min) (* *columnas* *maximo-valor*)) ;; Vemos si ganamos nosotros
    ((equal jugador *jugador-maquina*)
     (loop for posicion in (posiciones-heuristicas tablero) summing
;;      (loop for posicion in (fila-superior tablero) summing
           (funcall (symbol-function heuristica) tablero posicion *color-humano*)))
    ((equal jugador *jugador-humano*)
     (loop for posicion in (posiciones-heuristicas tablero) summing
;;      (loop for posicion in (fila-superior tablero) summing
           (funcall (symbol-function heuristica) tablero posicion *color-maquina*)))))

;; Comprueba el resultado de la partida
;; Hay que tener en cuenta que se analiza un nodo para un jugador que ya ha echado su ficha, por eso todo
;; parece pensado para su contrincante
(defun analiza-final-ch (nodo-j-final &optional (canal t))
  (escribe-nodo-j nodo-j-final canal)
  (cond ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'min)
         (format t "~&La ~a ha ganado~%" (third *procedimiento2*))
         (format canal "~%~%La ~a ha ganado~%" (third *procedimiento2*))) ;; Heuristica 2 gana
        ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'max)
         (format t "~&La ~a ha ganado~%" (third *procedimiento*))
         (format canal "~%~%La ~a ha ganado~%" (third *procedimiento*))) ;; Heuristica 1 gana
        (t (format t "~&Empate~%")
	   (format canal "~%~%Empate~%"))))

;; Lanza el menu de la aplicacion compilado
(defun jugar()
  (compile-file "conecta4.lsp")
  (load "conecta4")
  (competicion))

(defun lanzar()
  (compile-file "conecta4.lsp")
  (load "conecta4")
  (menu))

;; Lanzador automatico de la aplicacion
(defun info ()
(format t "~%Teclea (jugar) para iniciar una partida rápida contra la máquina")
(format t "~%Teclea (lanzar) para probar todas las posibilidades del juego"))

(info)