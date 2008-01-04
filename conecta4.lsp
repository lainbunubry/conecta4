(load "funciones-auxiliares.lsp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Representacion de estados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Definicion de variables
(defvar *filas* 6)
(defvar *columnas* 7)
(defvar *nodo-j-inicial*)
(defvar *estado-inicial* (make-array '(6 7)))
(defvar *jugador-humano* 'humano)
(defvar *jugador-humano-1* 'humano-1)
(defvar *jugador-maquina* 'maquina)
(defvar *jugador-maquina-1* 'maquina-1)

;; Estructura que representa un nodo del árbol de búsqueda
(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nil)
                   (:print-function escribe-nodo-j))
  estado 		;; Tablero modificado
  jugador
  valor) 		;; Valor heuristico de la nueva jugada

;; Crea un nuevo nodo del árbol de búsqueda
(defun crea-nodo-j (estado jugador)
	)

;; Funcion que muestra por pantalla el nodo por el canal t (pantalla) y profundidad
(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
  (format canal "~%Estado : ~a~%Jugador : ~a"
          (estado nodo-j)
          (jugador nodo-j)))

;; (escribe-nodo-j *nodo-j-inicial*)

;; Función que inicializa *nodo-j-inicial*
(defun crea-nodo-j-inicial (jugador)
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
                 :jugador jugador)))

;; (crea-nodo-j-inicial '(rojo))
;; (crea-nodo-j-inicial *jugador-humano*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones Heuristicas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primera funcion heuristica, cuenta el numero de piezas del mismo
;; color que hay en un rango de 3 posiciones
(defun funcion-heuristica-1 (tablero lista-valores jugador)
	(loop for pos in lista-valores count (igual tablero pos color)))

(defun igual (tablero pos color)
	(eq (aref tablero (first pos) (second pos)) color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones de rangos de valores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; genera el rango de valores de la matriz donde vamos a contar
;; las fichas que esten consecutivas
;; f y c representan la el numero de fila y de columna (f,c)

;; Devuelve una lista de listas de posiciones filas,columnas y diagonales
(defun rango-posiciones (f c)
	(list (seccion-fila f) (seccion-columna c) (seccion-diagonal-izq f c) (seccion-diagonal-der f c)))

;; Fila con un rango + - 3
(defun seccion-fila (f)
  (let ((inicio (max (- f 3) 0))
	(fin (min (+ f 3) *columnas*)))
    (loop for i from inicio to fin collect (list f i))))

;; Columna con un rango de + - 3
(defun seccion-columna (c)
  (let ((inicio (max (- c 3) 0))
	(fin (min (+ c 3) *filas*))) 
    (loop for i from inicio to fin collect (list i c))))

;; Diagonal izquierda con un rango de + - 3
(defun seccion-diagonal-izq (f c) 
    (list
	(loop for i from 3 downto 0 when (and (> (- f i) -1) (> (- c i) -1)) collect
	(list (- f i) (- c i)))
	(loop for i from 1 to 3 when (and (< (+ f i) *filas*) (< (+ c i) *columnas*)) collect
	(list (+ f i) (+ c i)))))

;; diagonal derecha con un rango de + - 3
(defun seccion-diagonal-der (f c) 
    (list
	(loop for i from 3 downto 0 when (and (> (- f i) -1) (< (+ c i) *columnas*)) collect
	(list (- f i) (+ c i)))
	(loop for i from 1 to 3 when (and (< (+ f i) *filas*) (> (- c i) -1)) collect
	(list (+ f i) (- c i)))))

;; Ejemplo
;; (rango-posiciones 2 2)
;; (((2 0) (2 1) (2 2) (2 3) (2 4) (2 5)) ((0 2) (1 2) (2 2) (3 2) (4 2) (5 2)) (((0 0) (1 1) (2 2)) ((3 3) (4 4) (5 5)))
;;  (((0 4) (1 3) (2 2)) ((3 1) (4 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restricciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
