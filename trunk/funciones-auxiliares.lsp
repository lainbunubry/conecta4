(load "aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARBITRACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variable con la información del algoritmo a usar
(defvar *procedimiento*)

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
(defun analiza-final (nodo-j-final)
  (escribe-nodo-j nodo-j-final)
  (cond ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'max)
         (format t "~&La maquina ha ganado"))
        ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) 'min)
         (format t "~&El humano ha ganado"))
        (t (format t "~&Empate"))))

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
  (format t "~%Mi recomendación:~&")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
		(escribe-nodo-j siguiente)))							;; TODO - Analizar como se imprime esto
														;; y dar la información adecuadamente

;; Determina si el juego ha llegado a su final
(defun es-estado-final (tablero)
	(or
		(movimientos-legales tablero)
		(cuenta-4-en-horizontal tablero *colora*)
		(cuenta-4-en-horizontal tablero *colorb*)
		(cuenta-4-en-vertical tablero *colora*)
		(cuenta-4-en-vertical tablero *colorb*)
		(cuenta-4-en-diagonal tablero *colora*)
		(cuenta-4-en-diagonal tablero *colorb*)
		))

(defun cuenta-4-en-horizontal (tablero color)
	(apply #'or
		(loop for x in
			(loop for i from 0 to 5 collect
				(cuenta-fichas-consecutivas (loop for j from 0 to 6 collect (aref tablero j i)) color))
			collect (> x 4))))

(defun cuenta-4-en-vertical (tablero color)
	(apply #'or
		(loop for x in
			(loop for i from 0 to 5 collect
				(cuenta-fichas-consecutivas (loop for j from 0 to 6 collect (aref tablero i j)) color))
			collect (> x 4))))

(defun cuenta-4-en-diagonal (tablero color)
	)

;; Determina si ha ganado el jugador dado
(defun es-estado-ganador (tablero jugador turno)		;; TODO - Ha de comprobar si el jugador
											;; que ha ganado es el del turno
	)

;; Devuelve el nodo siguiente según una jugada de la IA
(defun aplica-decision (procedimiento nodo-j)		;; TODO - Debe llamar al MINIMAX y dar un nuevo nodo
	)

;; Devuelve el nodo siguiente según el movimiento dado por el jugador
(defun aplica-movimiento (movimiento tablero)		;; TODO - Se echa la ficha en la columna dicha y amén XD
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITMO MINIMAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
