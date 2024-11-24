(defun rev (L r) (cond
    ((eq L nil) r)
    (t (rev (cdr L) (cons (car L) r)))
))

(defun fun (a S) (cond
    ((null S) (cons (list a 1) S))
    ((eq a (caar S)) (cons (list a (+ (car(cdr(car S))) 1)) (cdr S)))
    (t (cons (car S) (fun a (cdr S))))
))

(defun freq (S)
    (rev (fr S ()) nil))

(defun fr (S b) (cond
    ((null S) b)
    ((atom S) (fun S b))
    (t (fr (car S) (fr (cdr S) b)))
))

;; Тестирование
       
(print (freq '(a (12 9) b (3))))       
(print (freq '(a 3 2 5 b)))
(print (freq '(((a(5)8) b) 7 (a(())) )) )
(print (freq '(((a(5)8) b) 7 (g(())) )) )
(print (freq '(((a(5)8) b) 7 (g(())) )) )
