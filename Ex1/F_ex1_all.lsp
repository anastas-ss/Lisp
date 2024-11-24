;pair
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


;elem
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


;level
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


;freq
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


;deep
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


;collection
(defun  fun (a r) (cond
                      ((null r) (cons a r))
                      ((member a r) r)
                      (t (cons a r))
))

(defun collection (l)
    (col l ()))
(defun col (l r) (cond
                     ((null l) r)
                     ((atom (car l)) (col (cdr l) (fun (car l) r)))
                     (t (col (car l) (col (cdr l) r)))
))

;; Тестирование    
(print (collection '(a (12 9) b (3))))       
(print (collection '(a 3 2 5 b)))
(print (collection '(((a(5)8) b) 7 (a(())) )) )