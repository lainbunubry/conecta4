;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES PARA COMPARAR HEURÍSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables para compara_heurs
(defvar *heuristica1*)
(defvar *heuristica2*)
(defvar *fichero-compara_heurs* "compara_heurs.txt")
(defvar *procedimiento2*)
(defvar *profundidad* 4)

;; Recibe los nombres de dos funciones heurísticas y genere un fichero de texto con la partida que
;; resulta si MIN utiliza la primera heurística y MAX la segunda
(defun compara_heurs (heuristica1 heuristica2 profundidad)
	(setf *procedimiento* (list 'minimax-a-b-ch profundidad heuristica1))
	(setf *procedimiento2* (list 'minimax-a-b-ch profundidad heuristica2))
	(with-open-file (str *fichero-compara_heurs* :direction :output :if-exists :supersede)
		(crea-nodo-j-inicial 'max)
		(if (es-estado-final *estado-inicial*)
			(analiza-final *nodo-j-inicial* str)
			(jugada-maquina-ch2 *nodo-j-inicial* str)))) ;; MAX usa la segunda heurística

;; Función llamada cuando es el turno de la máquina de la heurística 1 en compara_heurs
(defun jugada-maquina-ch1 (nodo-j canal)
	(format t "~%DEBUG - Entrando en jugada-maquina-ch1")
	(escribe-nodo-j nodo-j canal)
	(format canal "~%Turno heurística 1.~&")
	(let ((siguiente (aplica-decision-ch *procedimiento* nodo-j)))
		(if (es-estado-final (estado siguiente))
			(analiza-final siguiente canal)
			(jugada-maquina-ch2 siguiente canal))))

;; Función llamada cuando es el turno de la máquina de la heurística 2 en compara_heurs
(defun jugada-maquina-ch2 (nodo-j canal)
	(format t "~%DEBUG - Entrando en jugada-maquina-ch2")
	(escribe-nodo-j nodo-j canal)
	(format canal "~%Turno heurística 2.~&")
	(let ((siguiente (aplica-decision-ch *procedimiento2* nodo-j)))
		(if (es-estado-final (estado siguiente))
			(analiza-final siguiente canal)
			(jugada-maquina-ch1 siguiente canal))))

;; Devuelve el nodo siguiente según una jugada de la IA para compara_heurs
(defun aplica-decision-ch (procedimiento nodo-j)
	(funcall (symbol-function (first procedimiento)) nodo-j (first (rest procedimiento)) (second (rest procedimiento))))

;; Algoritmo MINIMAX con poda ALFA-BETA para compara_heurs
(defun minimax-a-b-ch (nodo-j profundidad heuristica
                           &optional (alfa *minimo-valor*)
                           (beta *maximo-valor*))
  (format t "~%DEBUG - Entrando en minimax-a-b")
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

;; TODO - Actualizar si se cambia el de conecta4.lsp
;; Devuelve una valoración heurística para un nodo (jugada) para compara_heurs
;; Parece que no tenga sentido comprobar las posiciones para el color del jugador contrario, pero al igual
;; que es-estado-ganador o analiza-final resulta que el jugador que recibimos como parámetro no es otro que
;; el del último nodo creado, un nodo sucesor del cual queremos conocer su heurística pero para el jugador
;; que echó último, es decir, el jugador anterior
(defun f-e-estatica-ch (tablero jugador heuristica)
  (cond
    ((es-estado-ganador tablero jugador 'min) (* *columnas* *maximo-valor*))
    ((es-estado-ganador tablero jugador 'max) *minimo-valor*)
    ((equal jugador *jugador-maquina*)
    		(loop for posicion in (posiciones-heuristicas tablero) summing
			(funcall (symbol-function heuristica) tablero posicion *color-humano*)))
    ((equal jugador *jugador-humano*)
		(loop for posicion in (posiciones-heuristicas tablero ) summing
	  		(funcall (symbol-function heuristica) tablero posicion *color-maquina*)))))
