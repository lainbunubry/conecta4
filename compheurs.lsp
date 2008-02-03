;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES PARA COMPARAR HEURÍSTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables para compara_heurs
(defvar *heuristica1*)
(defvar *heuristica2*)
(defvar *fichero-compara_heurs*)
(defvar *procedimiento2*)

;; TODO - Peta infinito xD
;; Recibe los nombres de dos funciones heurísticas y genere un fichero de texto con la partida que
;; resulta si MIN utiliza la primera heurística y MAX la segunda
(defun compara_heurs (heuristica1 heuristica2)
	(setf *procedimiento* (list 'heuristica1 *profundidad*)) ;; TODO - Obviamente mal, el procedimiento es minimax
	(setf *procedimiento2* (list 'heuristica2 *profundidad*)) ;;TODO - Idem q anterior
	(setq *fichero-compara_heurs* (open "compara_heurs.txt" :direction :input))
	(crea-nodo-j-inicial 'max)
	(es-estado-final *estado-inicial*)
	(analiza-final *nodo-j-inicial*)
	(jugada-maquina-compara_heurs-2 *nodo-j-inicial*)
	(close *fichero-compara_heurs*)) ;; MAX usa la segunda heurística

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