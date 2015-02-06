(in-package :user)
;;;
;;; Andre Santos
;;; 82118
;;; File: G023.lisp   

;estado
(defstruct estado
  	nQueen				
  	tab					
)

;cria estado
(defun makeState (nqueen tab)
	(let ((init
		(make-estado
  			:nQueen nqueen
  			:tab tab
  		)
	)) 
	init))

;conta numero rainha por linha
(defun nQueen (arr)
	(let ((y (array-dimension arr 0)) (x (array-dimension arr 1)) (count 0))
		(dotimes (i y)
			(dotimes (j x)
				( if (equal (aref arr i j) t)
					(incf count)
				)
			)
		)
		count))

;funcao objectivo
(defun isGoal (actualState)
	(if (equal (estado-nQueen actualState) (array-dimension (estado-tab actualState) 0))
		;colocar tabuleiro no formato correcto
		(progn 
			(dotimes (i (estado-nQueen actualState))
				(dotimes (j (estado-nQueen actualState))
					(if (equal (aref (estado-tab actualState) i j) 'x)
						(setf (aref (estado-tab actualState) i j) nil)
					)
				)
			)
			t)))


;funcao sucessores
(defun succs (actualState)
	(let ((tab (estado-tab actualState)) (n (estado-nQueen actualState)) (sucessors nil) (size (array-dimension (estado-tab actualState) 0)) )
		;varrer linha
		(dotimes (i size)
			(if (equal (aref tab n i) nil);e sucessor
				(let ((newtab (copy-array tab)))
					;numero rainhas = y onde vamos trabalhar, posicao x, tamanho , copia tabuleiro pai 
					(movs n i size newtab);funcao destrutiva
					(setf (aref newtab n i) t)
					(setf sucessors (append (list (makeState (1+ n) newtab)) sucessors));adicionar
				)
			)
		) sucessors ))

;funcao heuristica
;conta quantas vezes encontra nil em tudo
;preferimos numero mais alto celulas nao atacadas
;quanto maoir for count melhor é estado
(defun nUnattack (actualState)
	(let ((count 0) (tab (estado-tab actualState)) (nqueen (estado-nQueen actualState)) (size (array-dimension (estado-tab actualState) 0)))
		(dotimes (i size)
			;para otimizar
			;so me interessa ver abaixo
			(if (>= i nqueen)
				(dotimes (j size)
					(if (equal (aref tab i j) nil)
						(incf count)
					)
				)
			)
		)
		count ))

;verifica se ponto esta dentro tabuleiro
(defun posvalida (point size)
	(let ((x (car point)) (y (cdr point)))
		(and (>= x 0) (< x size) (>= y 0) (< y size))
	))

;lista movimentos validos
(defun movs (y x size tab)
	(dotimes (i size)
		(if (/= i 0)
			(let (
				(mov1 (cons (+ y i) (- x i)))
				(mov2 (cons (+ y i) x))
				(mov3 (cons (+ y i) (+ x i))))
				(if (equal (posvalida mov1 size) t)	
					(setf (aref tab (+ y i) (- x i)) 'x)
				)
				(if (equal (posvalida mov2 size) t)	
					(setf (aref tab (+ y i) x) 'x)
				)
				(if (equal (posvalida mov3 size) t)	
					(setf (aref tab (+ y i) (+ x i)) 'x)
				)
			)
		)
	) tab )

;posicao rainha na linha row
(defun getQuePos (row actualState size)
	(dotimes (i size)
		(if (equal (aref (estado-tab actualState) row i) t)
			(return (list row i)); retorna y x
		)))

(defun resolve-problema (tab tipo)
	(last (car (time 
		(procura 
			(cria-problema 
				(makeState 0 tab) 
				(list #'succs)
				:objectivo? #'isGoal 
				:estado= #'equal 
				:heuristica #'nUnattack) 
			tipo :espaco-em-arvore? T
   )))))