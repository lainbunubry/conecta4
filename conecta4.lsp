(defvar *procedimiento*)

(defun juego (&key (empieza-la-maquina? nil)
                   (procedimiento ’(minimax 5)))
  (setf *procedimiento* procedimiento)
  (cond (empieza-la-maquina? (crea-nodo-j-inicial ’max)
                             (if (es-estado-final *estado-inicial*)
                                 (analiza-final *nodo-j-inicial*)
                                 (jugada-maquina *nodo-j-inicial*)))
        (t (crea-nodo-j-inicial ’min)
           (if (es-estado-final *estado-inicial*)
               (analiza-final *nodo-j-inicial*)
               (jugada-humana *nodo-j-inicial*)))))

(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nil)
                   (:print-function escribe-nodo-j))
  estado
  jugador
  valor)

(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
  (format canal "~%Estado : ~a~%Jugador : ~a"
          (estado nodo-j)
          (jugador nodo-j)))

(defvar *nodo-j-inicial*)

(defun crea-nodo-j-inicial (jugador)
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
                 :jugador jugador)))
(defun analiza-final (nodo-j-final)
  (escribe-nodo-j nodo-j-final)
  (cond ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) ’max)
         (format t "~&La maquina ha ganado"))
        ((es-estado-ganador (estado nodo-j-final)
                            (jugador nodo-j-final) ’min)
         (format t "~&El humano ha ganado"))
        (t (format t "~&Empate"))))

(defun jugada-maquina (nodo-j)
  (escribe-nodo-j nodo-j)
  (format t "~%Mi turno.~&")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
    (if (es-estado-final (estado siguiente))
        (analiza-final siguiente)
        (jugada-humana siguiente))))

(defun movimientos-legales (estado)
  (loop for m in *movimientos*
        when (aplica-movimiento m estado)
        collect m))

(defun escribe-movimientos (movimientos)
  (format t "~%Los movimientos permitidos son:")
  (let ((numero 0))
    (loop for m in movimientos
          do
          (if (= (mod numero 3) 0)
              (format t "~%      ~a (~a)" m numero)
              (format t "      ~a (~a)" m numero))
          (setf numero (+ numero 1)))))

(defun jugada-humana (nodo-j)
  (escribe-nodo-j nodo-j)
  (let ((movimientos (movimientos-legales (estado nodo-j))))
    (escribe-movimientos movimientos)
    (format t "~%Tu turno: ")
    (let ((m (read)))
      (cond ((and (integerp m)
                  (< -1 m (length movimientos)))
             (let ((nuevo-estado
                    (aplica-movimiento (nth m movimientos) (estado nodo-j))))
               (cond (nuevo-estado
                      (let ((siguiente (crea-nodo-j
                                        :estado nuevo-estado
                                        :jugador ’max)))
                        (if (es-estado-final nuevo-estado)
                            (analiza-final siguiente)
                          (jugada-maquina siguiente))))
                     (t (format t "~&   El movimiento ~a no se puede usar. " m)
                        (jugada-humana nodo-j)))))
            (t (format t "~&   ~a es ilegal. " m)
               (jugada-humana nodo-j))))))

(defun minimax (nodo-j profundidad)
  (if (or (es-estado-final (estado nodo-j))
          (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                        (jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                              (jugador nodo-j)))
            (if (eq (jugador nodo-j) ’max)
                (maximizador sucesores profundidad)
                (minimizador sucesores profundidad))))))

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

(defun contrario (jugador)
  (if (eq jugador ’max) ’min ’max))

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
          (if (eq (jugador nodo-j) ’max)
              (maximizador-a-b
               (sort sucesores #’>
                     :key (lambda (nodo) (f-e-estatica (estado nodo) ’min)))
               profundidad alfa beta)
            (minimizador-a-b
             (sort sucesores #’<
                   :key (lambda (nodo) (f-e-estatica (estado nodo) ’max)))
             profundidad alfa beta))))))

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
