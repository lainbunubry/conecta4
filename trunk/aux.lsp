;;Madre Mía.

;Estructura que representa el tablero con las fichas
(defstruct (juego (:constructor crea-tablero)
                   (:conc-name nil)
                   (:print-function muestra-tablero))
  fichas
  posiciones)

; Constructor del tablero
;------------------------------
;notas : No se como coño hacer que lo cree de n dimensiones
;he puesto 5 por poner
(defun crea-tablero-en-blanco ()
  (setf *tablero* (make-array '(5 5))))

(defparameter *fila* 4)
(defparameter *columna* 4)
;(crea-tablero-en-blanco)

;Función que imprime el tablero
;------------------------------

(defun imprime-tablero (a)
  (let* ((dim (array-dimensions a))
	 (f (first dim))
	 (c (second dim)))
    (format t "~%")
    (escribe-linea-aux c)
    (loop for i from 0 to (- f 1)
	  do (loop for j from 0 to (- c 1)
		   do (if (equal (aref a i j) NIL)
			  (format t "| ")
			(format t "|~a" (aref a i j))))
	  (format t "|~%")
	  (escribe-linea-aux c))))

(defun escribe-linea-aux (col)
       (loop for i from 0 to (- col 1)
               do (format t "+-"))
       (format t "+~%"))

;(imprime-tablero *tablero*)

;Funcion de inserción de las fichas
;------------------------------------
;inserta una ficha correctamente en la columna que tu quieras
;notas: Si la columna esta llena no peta
(defun inserta-ficha-en-fila (tablero columna color)
  (setf (aref tablero 
	      (primera-posicion-vacia tablero columna)
	      columna)
	color))

;(inserta-ficha-en-fila *tablero* 1 'R)
;(imprime-tablero *tablero*)

(defun posicion-vacia (tablero f c)
  (if (eq nil (aref tablero f c))
      t ;vacia
    nil))
;(posicion-vacia *tablero* 1 1)      

(defun primera-posicion-vacia (tablero columna)
    (loop for i from *fila* downto 0
	  minimize i
	  until(posicion-vacia tablero i columna)))
;(primera-posicion-vacia *tablero* 1)

;Funciones de contar
;-------------------------------
(defun mismo-color (tablero i j color)
  (if (eq (aref tablero i j) color)
      t
    nil))
;(mismo-color *tablero* 1 1 'r)

(defun cuenta-lineas (tablero

;devuelve un rango de 3 posicion con centro f
(defun seccion-fila (tablero f)
  (let ((inicio (max (- f 3) 0))
	(fin (min (+ f 3) *columna*)))
    (loop for i from inicio to fin collect (list f i))))
(defun seccion-columna (tablero c)
  (let ((inicio (max (- f 3) 0))
	(fin (min (+ f 3) *fila*))) 
    (loop for i from inicio to fin collect (list i c))))
(seccion-fila tablero 3)
;(defun seccion-diagonal-