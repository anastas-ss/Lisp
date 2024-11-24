(defun rev (L r) (cond
    ((eq L nil) r)
    (t (rev (cdr L) (cons (car L) r)))
))

(defun search_start (N L) (cond
    ((eq (cdr L) nil) (car L)) ; if N > len(L) => L[-1] elem
    ((= N 0) (car L))
    (t (search_start (- N 1) (cdr L)))
))

(defun elem (N L) (cond
    ((eq L nil) nil)
    ((= N 0) nil)
    ((> N 0) (search_start (- N 1) L))
    (t (search_start (- (- 0 N) 1) (rev L nil)))
))


;; Тестирование
(print (elem 1 '()))       
(print (elem 2 '(a (12 9) b (3))))       
(print (elem -3 '(a (12) b (3))))
(print (elem -2 '(a (12) b (3))))