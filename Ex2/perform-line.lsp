# Точки на плоскости заданы с помощью Лисповского списка: ((x1 y1) (x2 y2)…).

# представить как ((K1 A1 B1 C1 ((x1 y1)(x2 y2))) (K2 A2 B2 C2 ((x3 y3)(x4 y4)(x5 y5))))


;; Функция для представления прямой
;; из двух точек (x1 y1) (x2 y2) сделать (K1 A1 B1 C1 ((x1 y1)(x2 y2)))
;; k = - a/b

(defun calc_a (p1 p2)(cond
   (t (- (cadr p2) (cadr p1))) ;; a = y2 - y1
   ))

(defun calc_b (p1 p2)(cond
   (t (- (car p1) (car p2))) ;; b = x1 - x2
   ))
(defun calc_c (p1 p2)(cond
   (t (+ ( *(calc_a p1 p2)(car p1)) ( *(calc_b p1 p2)(cadr p1)))) ;; c = a ⋅ x₁ + b ⋅ y₁
   ))
(defun calc_k (a b)(cond
   ((/= b 0) (- ( / a b) 0)) ;; k = - a/b if b != 0 else A
   (t 'a)
   ))
(defun perform_line (p1 p2)(cond
   (t (list (calc_k (calc_a p1 p2)(calc_b p1 p2))(calc_a p1 p2) (calc_b p1 p2) (calc_c p1 p2) p1 p2))
   ))

;; Тестирование
;; (print (calc_a '(1 6) '(0 2))) 
;; (print (calc_b '(1 6) '(0 2))) 
;; (print (calc_c '(1 6) '(0 2)))
;; (print (calc_k (calc_a '(1 6) '(0 2)) (calc_b '(1 6) '(0 2))))
(print(perform_line '(4 6) '(2 3)))
