#lang racket
(require racket/class
         racket/set
         lazy/force
         racket/promise
         racket/serialize
         compatibility/mlist
         (only-in racket/list shuffle argmax))

;;--------------------------------------------------------------------
;; Реализация минимакса с альфа-бета отсечением
(define (minimax tree)
  (define (minimax-h node alpha beta max-player)
    (define (next-max x v)
      (if (or (null? x) (<= beta v)) 
                v
                (next-max (cdr x)
                      (max v (minimax-h (car x) v beta (not max-player)))))
    )
    (define (next-min x v)
      (if (or (null? x) (<= v alpha)) 
                v
                (next-min (cdr x)
                      (min v (minimax-h (car x) alpha v (not max-player)))))
    )
    (cond 
         ((number? node) node)
         ((null? node) 0.0)
         (max-player (next-max node alpha))
         (else (next-min node beta)))
          
  )
  (!(minimax-h tree -inf.0 +inf.0 #f))
 )


;;--------------------------------------------------------------------
;; описание класса, реализующего логику и оптимальную стратегию произвольной игры с нулевой суммой и полной информацией
(define game%
  (class object%
    (super-new)
 
    ;; виртуальные методы для задания правил игры
    (init-field my-win?         ; State -> Bool
                my-loss?        ; State -> Bool
                draw-game?      ; State -> Bool
                my-move         ; State Move -> State
                opponent-move   ; State Move -> State
                possible-moves  ; State -> (list Move)
                show-state)     ; State -> Any
 
    ;; optimal-move :: State -> Move
    ;; выбор оптимального хода по минимаксу 
    ;; из нескольких оптимальных выбирается один случайно
    (define/public ((optimal-move look-ahead) S)
      (!(argmax (lambda (m) (!(minimax (game-tree S m look-ahead))))
                 (shuffle (possible-moves S)))))
 
    ;; game-tree :: State -> (Move -> (Tree of Real))
	;; построение дерева с оценками
    (define (game-tree St m look-ahead)
	   ;; вспомогательная функция, строящая закольцованный список из пары элементов
      (define (help a b) (begin (define l (mlist a b a)) (set-mcdr! l (mcons b l)) l))
      (define (new-ply moves i s)	  
        (cond
          ((my-win? s) +inf.0) ; в выигрышной позиции оценка = + бесконечность
          ((my-loss? s) -inf.0) ; в проигрышной = - бесконечность
          ((draw-game? s)     0) ; при ничье = 0
          ((>= i look-ahead)  (f-h s)) ; если исчерпана глубина, то используется эвристическая оценка 
          (else (map (lambda (x) (new-ply (mcdr moves) (+ 1 i) ((mcar moves) s x)))
                     (possible-moves s))) ; рассматриваем все возможные ходы и строим их оценки
		))
     (new-ply (help opponent-move my-move) 1 (my-move St m))
    )
 
    ;; make-move :: State (State -> Move) -> (Move State Symbol)
    (define/public (make-move S move)
      (cond
        ((my-win? S) (values '() S 'win))
        ((my-loss? S)   (values '() S 'loss))         
        ((draw-game? S) (values '() S 'draw))   
        (else (let* ((m* (! (move S)))
                     (S* (my-move S m*)))
                (cond
                  ((my-win? S*)    (values m* S* 'win)) 
                  ((draw-game? S*) (values m* S* 'draw))
                  (else            (values m* S* 'next))))))
	)
  ))
 
;;--------------------------------------------------------------------
;; Реализация класса игрока
;; Параметр `game` указывает, в какая игра решается.
(define (interactive-player game)
  (class game
    (super-new)
 
    (inherit-field show-state)
    (inherit make-move optimal-move)
 
    (init-field name
                [look-ahead 4]
                [opponent 'undefined]
                [move-method (optimal-move look-ahead)])
 
    (define/public (your-turn S)
      (define-values (m S* status) (make-move S move-method))
      (! (printf "\n~a makes move ~a\n" name m))
      (! (show-state S*))
      (! (case status
           ['stop (displayln "The game was interrupted.")]
           ['win  (printf "\n~a won the game!" name)]
           ['loss (printf "\n~a lost the game!" name)]
           ['draw (printf "\nDraw!")]
           [else (send opponent your-turn S*)])))))
 
 
;;--------------------------------------------------------------------
;; макрос для описания партнеров в игре
(define-syntax-rule 
  (define-partners game (A #:win A-wins #:move A-move) 
                        (B #:win B-wins #:move B-move))
  (begin
    (define A (class game 
                (super-new 
                 [my-win?  A-wins]
                 [my-loss? B-wins]
                 [my-move  A-move]
                 [opponent-move B-move])))
    (define B (class game 
                (super-new 
                 [my-win?  B-wins]
                 [my-loss? A-wins]
                 [my-move  B-move]
                 [opponent-move A-move])))))
 
;;--------------------------------------------------------------------
;; функция, инициализирующая игру
(define (start-game p1 p2 initial-state)
  (set-field! opponent p1 p2)
  (set-field! opponent p2 p1)
  (send p1 your-turn initial-state))
;; пример использования
;; (!(start-game player-A player-B empty-board)) 

;;--------------------------------------------------------------------
;; Описания крестиков-ноликов
 
;; Структура представляющая ситуацию на игровой доске. В структуре два поля -- список координат крестиков и список координат ноликов
(serializable-struct board (x o x-score o-score) #:transparent)
 
;; геттеры полей структуры
(define xs board-x)
(define os board-o)
(define x-score board-x-score)
(define o-score board-o-score)

;; в начале игровая доска пуста
(define empty-board (board (set) (set) 0 0))
 
;; перечень всех ячеек игровой доски
(define all-cells
  (set
   '(1 1 1) '(1 1 2) '(1 1 3) '(1 1 4) 
   '(1 2 1) '(1 2 2) '(1 2 3) '(1 2 4) 
   '(1 3 1) '(1 3 2) '(1 3 3) '(1 3 4) 
   '(1 4 1) '(1 4 2) '(1 4 3) '(1 4 4) 

   '(2 1 1) '(2 1 2) '(2 1 3) '(2 1 4) 
   '(2 2 1) '(2 2 2) '(2 2 3) '(2 2 4) 
   '(2 3 1) '(2 3 2) '(2 3 3) '(2 3 4) 
   '(2 4 1) '(2 4 2) '(2 4 3) '(2 4 4) 

   '(3 1 1) '(3 1 2) '(3 1 3) '(3 1 4) 
   '(3 2 1) '(3 2 2) '(3 2 3) '(3 2 4) 
   '(3 3 1) '(3 3 2) '(3 3 3) '(3 3 4) 
   '(3 4 1) '(3 4 2) '(3 4 3) '(3 4 4) 

   '(4 1 1) '(4 1 2) '(4 1 3) '(4 1 4) 
   '(4 2 1) '(4 2 2) '(4 2 3) '(4 2 4) 
   '(4 3 1) '(4 3 2) '(4 3 3) '(4 3 4) 
   '(4 4 1) '(4 4 2) '(4 4 3) '(4 4 4)
  )
)

;; получить пустые ячейки игровой доски  
(define (free-cells b)
  (set-subtract all-cells (xs b) (os b)))

;; Все возможные линии игровой доски
(define winning-positions
  ( list
(set '(1 1 1) '(1 2 1) '(1 3 1) '(1 4 1)) (set '(2 1 1) '(2 2 1) '(2 3 1) '(2 4 1)) (set '(3 1 1) '(3 2 1) '(3 3 1) '(3 4 1)) (set '(4 1 1) '(4 2 1) '(4 3 1) '(4 4 1))

(set '(1 1 1) '(2 1 1) '(3 1 1) '(4 1 1)) (set '(1 2 1) '(2 2 1) '(3 2 1) '(4 2 1)) (set '(1 3 1) '(2 3 1) '(3 3 1) '(4 3 1)) (set '(1 4 1) '(2 4 1) '(3 4 1) '(4 4 1))

(set '(1 1 1) '(2 2 1) '(3 3 1) '(4 4 1)) (set '(1 4 1) '(2 3 1) '(3 2 1) '(4 1 1)) (set '(1 1 2) '(1 2 2) '(1 3 2) '(1 4 2)) (set '(2 1 2) '(2 2 2) '(2 3 2) '(2 4 2))
(set '(3 1 2) '(3 2 2) '(3 3 2) '(3 4 2)) (set '(4 1 2) '(4 2 2) '(4 3 2) '(4 4 2)) (set '(1 1 2) '(2 1 2) '(3 1 2) '(4 1 2)) (set '(1 2 2) '(2 2 2) '(3 2 2) '(4 2 2))
(set '(1 3 2) '(2 3 2) '(3 3 2) '(4 3 2)) (set '(1 4 2) '(2 4 2) '(3 4 2) '(4 4 2)) (set '(1 1 2) '(2 2 2) '(3 3 2) '(4 4 2)) (set '(1 4 2) '(2 3 2) '(3 2 2) '(4 1 2))

(set '(1 1 3) '(1 2 3) '(1 3 3) '(1 4 3)) (set '(2 1 3) '(2 2 3) '(2 3 3) '(2 4 3)) (set '(3 1 3) '(3 2 3) '(3 3 3) '(3 4 3)) (set '(4 1 3) '(4 2 3) '(4 3 3) '(4 4 3))

(set '(1 1 3) '(2 1 3) '(3 1 3) '(4 1 3)) (set '(1 2 3) '(2 2 3) '(3 2 3) '(4 2 3)) (set '(1 3 3) '(2 3 3) '(3 3 3) '(4 3 3)) (set '(1 4 3) '(2 4 3) '(3 4 3) '(4 4 3))

(set '(1 1 3) '(2 2 3) '(3 3 3) '(4 4 3)) (set '(1 4 3) '(2 3 3) '(3 2 3) '(4 1 3)) (set '(1 1 4) '(1 2 4) '(1 3 4) '(1 4 4)) (set '(2 1 4) '(2 2 4) '(2 3 4) '(2 4 4))
(set '(3 1 4) '(3 2 4) '(3 3 4) '(3 4 4)) (set '(4 1 4) '(4 2 4) '(4 3 4) '(4 4 4)) (set '(1 1 4) '(2 1 4) '(3 1 4) '(4 1 4)) (set '(1 2 4) '(2 2 4) '(3 2 4) '(4 2 4))
(set '(1 3 4) '(2 3 4) '(3 3 4) '(4 3 4)) (set '(1 4 4) '(2 4 4) '(3 4 4) '(4 4 4)) (set '(1 1 4) '(2 2 4) '(3 3 4) '(4 4 4)) (set '(1 4 4) '(2 3 4) '(3 2 4) '(4 1 4))

(set '(1 1 1) '(1 1 2) '(1 1 3) '(1 1 4)) (set '(1 2 1) '(1 2 2) '(1 2 3) '(1 2 4)) (set '(1 3 1) '(1 3 2) '(1 3 3) '(1 3 4)) (set '(1 4 1) '(1 4 2) '(1 4 3) '(1 4 4))
(set '(2 1 1) '(2 1 2) '(2 1 3) '(2 1 4)) (set '(2 2 1) '(2 2 2) '(2 2 3) '(2 2 4)) (set '(2 3 1) '(2 3 2) '(2 3 3) '(2 3 4)) (set '(2 4 1) '(2 4 2) '(2 4 3) '(2 4 4))
(set '(3 1 1) '(3 1 2) '(3 1 3) '(3 1 4)) (set '(3 2 1) '(3 2 2) '(3 2 3) '(3 2 4)) (set '(3 3 1) '(3 3 2) '(3 3 3) '(3 3 4)) (set '(3 4 1) '(3 4 2) '(3 4 3) '(3 4 4))
(set '(4 1 1) '(4 1 2) '(4 1 3) '(4 1 4)) (set '(4 2 1) '(4 2 2) '(4 2 3) '(4 2 4)) (set '(4 3 1) '(4 3 2) '(4 3 3) '(4 3 4)) (set '(4 4 1) '(4 4 2) '(4 4 3) '(4 4 4))
(set '(1 1 1) '(1 2 2) '(1 3 3) '(1 4 4)) (set '(1 1 4) '(1 2 3) '(1 3 2) '(1 4 1)) (set '(2 1 1) '(2 2 2) '(2 3 3) '(2 4 4)) (set '(2 1 4) '(2 2 3) '(2 3 2) '(2 4 1))

(set '(3 1 1) '(3 2 2) '(3 3 3) '(3 4 4)) (set '(3 1 4) '(3 2 3) '(3 3 2) '(3 4 1)) (set '(4 1 1) '(4 2 2) '(4 3 3) '(4 4 4)) (set '(4 1 4) '(4 2 3) '(4 3 2) '(4 4 1))

(set '(1 1 1) '(2 1 2) '(3 1 3) '(4 1 4)) (set '(1 1 4) '(2 1 3) '(3 1 2) '(4 1 1)) (set '(1 2 1) '(2 2 2) '(3 2 3) '(4 2 4)) (set '(1 2 4) '(2 2 3) '(3 2 2) '(4 2 1))

(set '(1 3 1) '(2 3 2) '(3 3 3) '(4 3 4)) (set '(1 3 4) '(2 3 3) '(3 3 2) '(4 3 1)) (set '(1 4 1) '(2 4 2) '(3 4 3) '(4 4 4)) (set '(1 4 4) '(2 4 3) '(3 4 2) '(4 4 1))

(set '(1 1 1) '(2 2 2) '(3 3 3) '(4 4 4)) (set '(1 1 4) '(2 2 3) '(3 3 2) '(4 4 1)) (set '(4 1 1) '(3 2 2) '(2 3 3) '(1 4 4)) (set '(4 1 4) '(3 2 3) '(2 3 2) '(1 4 1))
)
)


;;находим список линий на доске, проходящих через позицию cell
(define (find-lines cell)
   (filter
     (lambda (line)
       (set-member? line cell)
     )
     winning-positions
   )
)

;; hash из позиции на доске в список линий проходящих через нее
(define cell2lines
  (make-hash
    (let loop ((lst '()) (cells (set->list all-cells)))
        (if (empty? cells) lst
            (loop (cons (cons (car cells) (find-lines (car cells))) lst) (cdr cells))
        )
    )
  )
)

;; получить все ячейки игровой доски, не занятые ходами одного из игроков 
(define (open-4-cells os/xs b)
  (set-subtract all-cells (os/xs b)))

;; получить количество линий игровой доски, открытых для одного из игроков
(define (count-open-4 l)
    (foldl (lambda (x y) (if (subset? x l) (+ 1 y) y)) 0 winning-positions))


;; проверка, является ли ситуация на игровой доске выигрышной
;; победа достигается в случае полного заполнения поля и большего числа линий у выбранного игрока
(define ((wins? s p) b)
  (and (set-empty? (free-cells b)) (> (s b) (p b)))
)
  

;; хэш-таблица для мемоизированной функции эвристичечкой оценки
(define f-h-hash (make-hash))

;; если есть записанная хэш-таблица в файле, то можно считать её из файла (define f-h-hash (read-hash "f-h.txt"))
(define (read-hash filename) (with-input-from-file filename (lambda () (deserialize (read)))))
; (define f-h-hash (read-hash "f-h.txt"))

;; мемоизированная функция эвристической оценки позиции
(define (f-h-memo s)  
	(let ((prev-calc (hash-ref f-h-hash s #f)))
		(if prev-calc prev-calc
			(let ((current-calc (f-h s)))
				(hash-set! f-h-hash s current-calc)
				current-calc
)	)	)	)


;; функция эвристической оценки позиции
;; из количества линий, открытых для крестиков, вычитается количество линий, открытых для ноликов
(define (f-h s)
  (- (count-open-4 (open-4-cells os s)) (count-open-4 (open-4-cells xs s)))
)  

(define (score-addition move s)
  (foldl (lambda (line score) (if (subset? (set-subtract line (set move)) s) (+ score 1) score)) 0 (hash-ref cell2lines move))
)


;; функции для осуществления ходов игроков
(define (x-move b m)  (board (set-add (xs b) m) (os b) (+ (x-score b) (score-addition m (xs b))) (o-score b)))
(define (o-move b m)  (board (xs b) (set-add (os b) m) (x-score b) (+ (o-score b) (score-addition m (os b)))))
 
;; вывод игровой доски в текстовом виде
(define (show-board b)
  (for ([i '(4 3 2 1)])
    (for ([z '(1 2 3 4)])
      (printf "~a " i)
      (for ([j '(1 2 3 4)])
        (display (cond
                   ((set-member? (os b) (list j i z)) "|o")
                   ((set-member? (xs b) (list j i z)) "|x")
                   (else "| "))))
      (display "|  "))
    (displayln "")
  )
  (displayln "   1 2 3 4      1 2 3 4      1 2 3 4      1 2 3 4  ")
  (printf "x-score: ~a\n" (x-score b))
  (printf "o-score: ~a\n-----------------------------------------------\n" (o-score b))
)
;;--------------------------------------------------------------------
;; Описание класса-игры крестики-нолики
(define tic-tac%
  (class game%
    (super-new
     [draw-game?       (lambda (x)   ;ничья достигается при полном заполнении поля и равном количестве заполненных линий каждым игроком 
                           (and (set-empty? (free-cells x)) (= (x-score x) (o-score x)))     
                       )]
     [possible-moves   (compose set->list free-cells)]
     [show-state       show-board])))
 
;; описания партнеров для крестиков-ноликов
(define-partners tic-tac%
  (x% #:win (wins? x-score o-score) #:move x-move)
  (o% #:win (wins? o-score x-score) #:move o-move))

 
;; объекты-игроки, принимающие ввод пользователя
;; проверка ввода частичная
(define (input-move m)
            (case m
                   ('q (exit))
                ('(1 1 1) m) ('(1 1 2) m) ('(1 1 3) m) ('(1 1 4) m) 
		('(1 2 1) m) ('(1 2 2) m) ('(1 2 3) m) ('(1 2 4) m) 
		('(1 3 1) m) ('(1 3 2) m) ('(1 3 3) m) ('(1 3 4) m) 
		('(1 4 1) m) ('(1 4 2) m) ('(1 4 3) m) ('(1 4 4) m) 
		('(2 1 1) m) ('(2 1 2) m) ('(2 1 3) m) ('(2 1 4) m) 
		('(2 2 1) m) ('(2 2 2) m) ('(2 2 3) m) ('(2 2 4) m) 
		('(2 3 1) m) ('(2 3 2) m) ('(2 3 3) m) ('(2 3 4) m) 
		('(2 4 1) m) ('(2 4 2) m) ('(2 4 3) m) ('(2 4 4) m) 
		('(3 1 1) m) ('(3 1 2) m) ('(3 1 3) m) ('(3 1 4) m) 
		('(3 2 1) m) ('(3 2 2) m) ('(3 2 3) m) ('(3 2 4) m) 
		('(3 3 1) m) ('(3 3 2) m) ('(3 3 3) m) ('(3 3 4) m) 
		('(3 4 1) m) ('(3 4 2) m) ('(3 4 3) m) ('(3 4 4) m) 
		('(4 1 1) m) ('(4 1 2) m) ('(4 1 3) m) ('(4 1 4) m) 
		('(4 2 1) m) ('(4 2 2) m) ('(4 2 3) m) ('(4 2 4) m) 
		('(4 3 1) m) ('(4 3 2) m) ('(4 3 3) m) ('(4 3 4) m) 
		('(4 4 1) m) ('(4 4 2) m) ('(4 4 3) m) ('(4 4 4) m) 
                   (else (input-move (read))))
				   )
(define user-A 
  (new (force (interactive-player x%)) 
       [name "User X"]
       [move-method 
        (lambda (b) (input-move (read)))]
		)
 )
(define user-B 
  (new (force (interactive-player o%)) 
       [name "User O"]
       [move-method 
        (lambda (b) (input-move (read)))]
		)
 )
;; старт игры двух человек
; (!(start-game user-A user-B empty-board))

(define la 3)
 
;; объекты-игроки ИИ
(define player-A (new (force (interactive-player x%)) [name "A"] [look-ahead la]))
 
(define player-B (new (force (interactive-player o%)) [name "B"] [look-ahead la]))

;; в конце игры можно записать хэш-таблицу в файл (write-hash "f-h.txt")
(define (write-hash filename) (with-output-to-file filename (lambda () (write (serialize f-h-hash)))))
(display "Initialization complete\n-----------------------------------------------")
;(!(start-game user-A user-B empty-board))
(!(start-game player-A player-B empty-board))
;(hash-ref cell2lines '(1 1 1))
