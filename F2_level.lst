(defun level (N S) (cond
    ((null S) nil)
    ((= N 0) S)
    ((atom (car S)) (level N (cdr S)))
    (t (cons (level (- N 1) (car S)) (level N (cdr S))))
))


;; Тестирование
       
(print (level 1 '(a (12 9) b (3))))       
(print (level 1 '(a 3 2 5 b)))
(print (level 3 '(((a(5)8) b) 7 (g(())) )) )
(print (level 2 '(((a(5)8) b) 7 (g(())) )) )
(print (level 4 '(((a(5)8) b) 7 (g(())) )) )