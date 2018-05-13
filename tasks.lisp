���� 1
11 �������: 2,3,9,11,12,13,21,43,45,48

;;; �3
;;; ���������� �������, ���������� � �������� ������ ��� ��������� ��������� �������� ������. 
;;; ������ ������� 402-�


(defun rep1 (lst src dest)
((lambda (a b)
  (cond ((null lst) nil)
        ((equal a src) (cons dest (rep1 b src dest)))
        ((listp a) (cons (rep1 a src dest) (rep1 b src dest)))
        (t (cons a (rep1 b src dest))))) (car lst) (cdr lst)))


(rep1 '(1 2 3 (((1 2 3) (1 2 3)))) 1 'a)
(A 2 3 (((A 2 3) (A 2 3))))

;;; �9
;;; ���������� �������, ����������� �������� ������ �� ��� ���������. 
;;;� ������ �� ��� ������ ������� �������� � ��������� ��������, 
;;;�� ������ � �������� � ������� ��������.
;;; ������ ������� 402-�


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


;;;�11. ���������� �������, �������������� ���������� ��������� ������ �� ���	
;;;���������. � ������ �� ��� ������ ������� ��������� ���������� ���������	
;;;� ������ ������, �� ������ � ���������� ��������.
;;; ������ ������� 402-�

(defun separate(input-list n)
  (if input-list
      (if (zerop n)
          (cons nil (cons input-list nil))
          ((lambda (elem result) (cons (cons elem (car result)) (cdr result)))
(car input-list) (separate (cdr input-list) (1- n))))))

(separate '(1 2 3 4 5) 0)
(NIL (1 2 3 4 5))		

;;; �12
;;; ���������� �������, ���������� � �������� ������ ��� ������ ������ ���������� �������� �����. 
;;; ������ ������� 402-�


(defun repltw (lst)
((lambda (a b c d)
 (cond ((null b) lst)
       ((equal a d) (cons a (repltw c)))
       (t (cons a (repltw b)))))(car lst) (cdr lst) (cddr lst) (cadr lst)))


(repltw '(a a  b b n n ))
(A B N)

;;; �13
;;; ���������� �������, ��������� � �������� ������ ��� ��������� ��������� ���������. 
;;; ������ ������� 402-�


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

;;; �21
;;; ���������� �������, ��������� �� ������ ������ ��������� ������� �������� �� ������� ������. 
;;; ������ ������� 402-�

(defun del(a b)
((lambda (x y)
(cond ((null b) nil)
	((equal a x) y)
	(cons x (del a y))))(car b) (cdr b)))

 (del 'a '(a v d l))
(V D L)



;;; 45. 
;;;���������� ���������� ����� �������� �� ����������� x � y
;;; ������ ������� 402-�

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

���� 2

;;; #1
;;; ���������� FUNCALL ����� ���������� APPLY.
;;; ������ ������� 402-�


(defun fan-call (func &rest args) (apply func args))

(fc-call #'* 1 2 3)
6

;;; �3. ���������� ���������� (APL-APPLY f x), ������� ��������� ������ �������  
;;; fi ������ (f1 f2 ... fn) � ���������������� �������� ������                  
;;; x = (x1 x2 ... xn) � ���������� ������, �������������� �� �����������.  
;;; ������ ������� 402-�

(defun apl (funk-list el-list)
  (cond ((null funk-list) nil) ((null el-list) nil)
        (t
         (cons (funcall (car funk-list) (car el-list))
               (apl (cdr funk-listT) (cdr el-list))))))

(apl-apply `(list cdr car) `((A B C) (1 2 3) (a1 b2 c3)))	
(((A B C)) (2 3) A1)

;;;�5. ���������� �������������� �������� (��������� ���� ������), �������      
;;;�������, �����, ���������� �������������� ���������� �������� ���� �������   
;;;���� �� ��� ������ �������� ������ ������. 
;;; ������ ������� 402-�
   
(defun -try (p lst)
   (not (null (mapcan #'(lambda (x) (if (funcall p x) (list t) nil) ) lst))))

;;; #7
;;; ���������� ������ (�������-����-�� ���� ������), ��������� �� ������ ������ ��� ��������,
;;; ������� �� �������� ���������, ������� �������� ��������� �������� ����.
;;; ������ ������� 402-�


(defun del-if-not (pred lst)
	(mapcan #'(lambda (x) (if (funcall pred x) (list x) nil)) lst)
) 