(load "funciones-auxiliares.lsp")
(load "conecta4.lsp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Representacion de estados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definicion de variables
(defvar *filas* 6)
(defvar *columnas* 7)
(defvar *nodo-j-inicial*)
(defvar *estado-inicial* (make-array '(6 7)))
(defvar *jugador-humano* 'humano)
(defvar *jugador-humano-1* 'humano-1)
(defvar *jugador-maquina* 'maquina)
(defvar *jugador-maquina-1* 'maquina-1)

;Estructura que representa un nodo del árbol de búsqueda
(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nil)
                   (:print-function escribe-nodo-j))
  estado ;Tablero modificado
  jugador 
  valor) ;Valor heuristico de la nueva jugada

;Funcion que muestra por pantalla el nodo por el canal t (pantalla) y profundidad
(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
  (format canal "~%Estado : ~a~%Jugador : ~a"
          (estado nodo-j)
          (jugador nodo-j)))

;(escribe-nodo-j *nodo-j-inicial*)

;Función que inicializa *nodo-j-inicial*
(defun crea-nodo-j-inicial (jugador)
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
                 :jugador jugador)))

;(crea-nodo-j-inicial '(rojo))
;(crea-nodo-j-inicial '(*jugador-humano*))

;Función que crea un nodo
(defun crea-nodo-j (jugador estado valor)
    (crea-nodo-j :estado estado
                 :jugador jugador
		:valor valor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restricciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones Heuristicas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
