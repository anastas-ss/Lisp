(defun both-empty-p (list1 list2) (cond
  ( (null (null list1)) nil) ; не пустой 
  ( (null list2) T)
  ))

(defun list2 (l1 l2)
  (cons l1 (cons l2 nil)))
  
(defun mix2 (L1 L2) (cond
    ((null L1) (list (list2 nil (car L2)))) ; Если L1 пуст, но L2 не пуст
    ((null L2) (list (list2 (car L1) nil))) ; Если L2 пуст, но L1 не пуст
    (t (cons (list2 (car L1) (car L2)) ; Создаем пару из элементов списков
             (mix2 (cdr L1) (cdr L2))))
)) ; Рекурсивно обрабатываем оставшиеся элементы

;; Тестирование
(print (mix2 '() '()))          ; => (NIL NIL)
(print (mix2 '() '(1 2)))       ; => (NIL 1)
(print (mix2 '(1 2) '()))       ; => (1 NIL)
(print (mix2 '(1 2) '(3 4)))    ; => ((1 3) (2 4))
(print (mix2 '(1 2 3) '(3 4)))   ; => ((1 3) (2 4)) без последнего кортежа с nil
