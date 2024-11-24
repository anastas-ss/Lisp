(defun deep (S) 
    (car (dp S 0)))
(defun dp (S N) (cond
                    ((atom S) (list S N))
                    ((null S) nil)
                    ((> (cadr (dp (car S) (+ N 1))) (cadr (dp (cdr S) N))) 
                         (dp (car S) (+ N 1)))
                    (t (dp (cdr S) N))
))
                    
                  

;; Тестирование
       
(print (deep '(a (12 9) b (3))))       
(print (deep '(a 3 2 5 b)))
(print (deep '(((a(5)8) b) 7 (a(())) )) )
