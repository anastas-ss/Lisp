(defun list2 (l1 l2)
  (cons l1 (cons l2 nil)))
  
(defun pair (L) (cond
    ((eq L nil) nil) ; ((eq L nil) (cons nil nil)) => (nil)
    ((eq (cdr L) nil) (cons
                        (list2 (car L) nil) nil))
    (t (cons
            (list2 (car L) (car(cdr L))) (pair (cdr (cdr L)))))
))


;; Тестирование
(print (pair '()))       
(print (pair '(1 2)))       
(print (pair '(1 2 3 4)))    
(print (pair '(1 2 3 4 5)))
