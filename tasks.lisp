Тема 1
11 вариант: 2,3,9,11,12,13,21,43,45,48

;;; №3
;;; Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим. 
;;; Фенина Евгения 402-И


(defun rep1 (lst src dest)
((lambda (a b)
  (cond ((null lst) nil)
        ((equal a src) (cons dest (rep1 b src dest)))
        ((listp a) (cons (rep1 a src dest) (rep1 b src dest)))
        (t (cons a (rep1 b src dest))))) (car lst) (cdr lst)))


(rep1 '(1 2 3 (((1 2 3) (1 2 3)))) 1 'a)
(A 2 3 (((A 2 3) (A 2 3))))

;;; №9
;;; Определите функцию, разделяющую исходный список на два подсписка. 
;;;В первый из них должны попасть элементы с нечетными номерами, 
;;;во второй — элементы с четными номерами.
;;; Фенина Евгения 402-И


(defun cr-even-odd-lst (lst)
	(cond 
		((null lst) nil)
        ((null (cdr lst)) (list lst))
		(t
			((lambda (prev-resault)
				(list
	                (cons (car lst) (car prev-resault))
	                (cons (cadr lst) (cadr prev-resault))))
			(cr-even-odd-lst (cddr lst)))
        )
     )
)


;;;№11. Определите функцию, осуществляющую разделение исходного списка на два	
;;;подсписка. В первый из них должно попасть указанное количество элементов	
;;;с начала списка, во второй — оставшиеся элементы.
;;; Фенина Евгения 402-И

(defun separate(input-list n)
  (if input-list
      (if (zerop n)
          (cons nil (cons input-list nil))
          ((lambda (elem result) (cons (cons elem (car result)) (cdr result)))
(car input-list) (separate (cdr input-list) (1- n))))))

(separate '(1 2 3 4 5) 0)
(NIL (1 2 3 4 5))		

;;; №12
;;; Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним. 
;;; Фенина Евгения 402-И


(defun repltw (lst)
((lambda (a b c d)
 (cond ((null b) lst)
       ((equal a d) (cons a (repltw c)))
       (t (cons a (repltw b)))))(car lst) (cdr lst) (cddr lst) (cadr lst)))


(repltw '(a a  b b n n ))
(A B N)

;;; №13
;;; Определите функцию, удаляющую в исходном списке все повторные вхождения элементов. 
;;; Фенина Евгения 402-И


(defun mem(a lst)
(cond
	((null lst) nil)
	((equal a (car lst)) T)
	(t (mem a (cdr lst)))
)
)

(defun del(lst)
(cond 
	((null lst) nil)
	(t  ((lambda (x y)
	(cond
		((mem x y) (del y))
		(t (cons x (del y)))
	)
	)
	(car lst) (cdr lst)
	))
)
) 

 (del '(1 2 2 3 5 4))
(1 2 3 5 4)

;;; №21
;;; Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне. 
;;; Фенина Евгения 402-И

(defun del(a b)
((lambda (x y)
(cond ((null b) nil)
	((equal a x) y)
	(cons x (del a y))))(car b) (cdr b)))

 (del 'a '(a v d l))
(V D L)



;;; 45. 
;;;Определить расстояние между городами по координатам x и y
;;; Фенина Евгения 402-И

(defun dist (city1 city2)
	(let
		(
			(x1 (get city1 'x))
			(y1 (get city1 'y))
			(x2 (get city2 'x))
			(y2 (get city2 'y))
		)
		(sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))
	)
)

(defun set-city (&key city x y)
	(setf (get city 'x) x)
	(setf (get city 'y) y)
)

(defun print (city1 city2)
	(format t "City1 = ~a, City2 = ~a, distance => ~a~%" city1 city2 (dist city1 city2))
)

(set-city :city 'Simfer :x 1 :y 2)
(set-city :city 'Piter :x 4 :y -2)

(print 'Simfer 'Piter) ; 5
(print 'Simfer 'Simfer) ; 0

Тема 2

;;; #1
;;; Определите FUNCALL через функционал APPLY.
;;; Фенина Евгения 402-И


(defun fan-call (func &rest args) (apply func args))

(fc-call #'* 1 2 3)
6

;;; №3. Определите функционал (APL-APPLY f x), который применяет каждую функцию  
;;; fi списка (f1 f2 ... fn) к соответствующему элементу списка                  
;;; x = (x1 x2 ... xn) и возвращает список, сформированный из результатов.  
;;; Фенина Евгения 402-И

(defun apl (funk-list el-list)
  (cond ((null funk-list) nil) ((null el-list) nil)
        (t
         (cons (funcall (car funk-list) (car el-list))
               (apl (cdr funk-listT) (cdr el-list))))))

(apl-apply `(list cdr car) `((A B C) (1 2 3) (a1 b2 c3)))	
(((A B C)) (2 3) A1)

;;;№5. Определите функциональный предикат (НЕКОТОРЫй пред список), который      
;;;истинен, когда, являющейся функциональным аргументом предикат пред истинен   
;;;хотя бы для одного элемента списка список. 
;;; Фенина Евгения 402-И
   
(defun -try (p lst)
   (not (null (mapcan #'(lambda (x) (if (funcall p x) (list t) nil) ) lst))))

;;; #7
;;; Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы,
;;; которые не обладают свойством, наличие которого проверяет предикат пред.
;;; Фенина Евгения 402-И


(defun del-if-not (pred lst)
	(mapcan #'(lambda (x) (if (funcall pred x) (list x) nil)) lst)
) 