

;;;在我写的解释器内对Ycombinator.scm作测试
;;在ikarus中作测试是最好的 简直是一种享受 因为它返回的结果是有缩进的适合人去阅读

(load "interpreter.scm")

;;不用define如何表达出递归函数？
;;首先对某一有限递归层次以内起作用的函数是可以写出来的
;;（称这种函数为有限层递归函数）
;;对non-primitive函数的测试：
;length0
(value
 '(lambda (l)
    (cond ((null? l)
	   0)
	  (else
	   (add1 (eternity (cdr l)))))))
;length1
(value
'(lambda (l)
   (cond 
    ((null? l)
     0)
    (else
     (add1 ((lambda (l)
	      (cond 
	       ((null? l)
		0)
	       (else
		(add1 (eternity (cdr l))))))
	    (cdr l)))))))
;length2
(value
 '(lambda (l)
    (cond
     ((null? l)
      0)
     (else
      (add1 ((lambda (l)
	       (cond
		((null? l)
		 0)
		(else
		 (add1 ((lambda (l)
			  (cond
			   ((null? l)
			    0)
			   (else
			    (add1 (eternity (cdr l))))))
			(cdr l))))))
	     (cdr l)))))))

;;对作用值的测试：
;length0
(value
 '((lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 (eternity (cdr l))))))
   '()))
;length1
(value
 '((lambda (l)
     (cond 
      ((null? l)
       0)
      (else
       (add1 ((lambda (l)
		(cond 
		 ((null? l)
		  0)
		 (else
		  (add1 (eternity (cdr l))))))
	      (cdr l))))))
   '(1)))
;length2
(value
 '((lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1 ((lambda (l)
		(cond
		 ((null? l)
		  0)
		 (else
		  (add1 ((lambda (l)
			   (cond
			    ((null? l)
			     0)
			    (else
			     (add1 (eternity (cdr l))))))
			 (cdr l))))))
	      (cdr l))))))
   '(1 2)))


;;第一次抽象：用一个特殊的算子的迭代来描述某个特殊的递归过程，从而写出有限层递归函数
;（算子指以函数为参数函数为值的函数）
;对non-primitive函数的测试：
(define eternity
  (lambda (x)
    (eternity x)))
;length0
(value
 '((lambda (length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (length (cdr l)))))))
   eternity))
;length1
(value
 '((lambda (length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    eternity)))
;length2
(value
 '((lambda (length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    ((lambda (length)
       (lambda (l)
	 (cond ((null? l)
    		0)
	       (else
		(add1 (length (cdr l)))))))
     eternity))))

;对作用值的测试：
;length0
(value
 '(((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    eternity)
   '()))
;length1
(value
 '(((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    ((lambda (length)
       (lambda (l)
	 (cond ((null? l)
		0)
	       (else
		(add1 (length (cdr l)))))))
     eternity))
   '(1)))
;length2
(value
 '(((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    ((lambda (length)
       (lambda (l)
	 (cond ((null? l)
		0)
	       (else
		(add1 (length (cdr l)))))))
     ((lambda (length)
	(lambda (l)
	  (cond ((null? l)
		 0)
		(else
		 (add1 (length (cdr l)))))))
      eternity)))
   '(1 2)))


;;第二次抽象：用一个对应于某个迭代次数的算子来描述有限次的迭代过程，作用在某个特殊的算子上，从而写出有限层递归函数
;对non-primitive的测试：
;length0
(value
 '((lambda (mk-length)
     (mk-length eternity))
   (lambda (length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (length (cdr l)))))))))
;length1
(value
 '((lambda (mk-length)
     (mk-length 
      (mk-length eternity)))
   (lambda (length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (length (cdr l)))))))))
;length2
(value
 '((lambda (mk-length)
     (mk-length
      (mk-length
       (mk-length eternity))))
   (lambda (length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (length (cdr l)))))))))

;对作用的测试：
;length0
(value
 '(((lambda (mk-length)
      (mk-length eternity))
    (lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l))))))))
   '()))
;length1
(value
 '(((lambda (mk-length)
      (mk-length 
       (mk-length eternity)))
    (lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l))))))))
   '(1)))
;length2
(value
 '(((lambda (mk-length)
      (mk-length
       (mk-length
	(mk-length eternity))))
    (lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l))))))))
   '(1 2)))

;; ;all names are equal,but some names are more equal than others.
;; ;length0
;; ((lambda (mk-length)
;;    (mk-length eternity))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond ((null? l)
;; 	    0)
;; 	   (else
;; 	    (add1 (length (cdr l))))))))
;; ;make names more equal 函数的意义改变了吗？
;; ((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    (lambda (l)
;;      (cond ((null? l)
;; 	    0)
;; 	   (else
;; 	    (add1 (mk-length (cdr l))))))))

;;测试non-primitive函数：
(value
 '((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (mk-length (cdr l)))))))))
;;测试作用：
(value
 '(((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (mk-length (cdr l))))))))
   '()))

;; ;length1
;; ((lambda (mk-length)
;;    (mk-length 
;;     (mk-length eternity)))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond ((null? l)
;; 	    0)
;; 	   (else
;; 	    (add1 (length (cdr l))))))))
;; ;make names more equal 函数的意义改变了吗？
;; ((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    (lambda (l)
;;      (cond ((null? l)
;; 	    0)
;; 	   (else
;; 	    (add1 ((mk-length eternity) (cdr l))))))))

;;测试non-primitive函数：
(value
 '((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 ((mk-length eternity) (cdr l)))))))))

;;测试作用：
(value
 '(((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 ((mk-length eternity) (cdr l))))))))
   '(1)))

;;终极测试：
(value
 '(((lambda (le)
      ((lambda (f)
	 (f f))
       (lambda (f)
	 (le (lambda (x) ((f f) x))))))
    (lambda (length)
      (lambda (l)
	(cond ((null? l) 0)
	      (else
	       (add1 (length (cdr l))))))))
   '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))

