
;;;the little schemer 不光是教人们编程
;;;它的 对话体 同时还教人们应该如何学习
;;;耐心 恒久的努力 认真弄清最简单的例子 勤奋地去练习

;;（这个文本不直接load到解释器中使用的）

;;;这里的代码来自``the little schemer''的章节：...and again,and again,and again,...

;;;首先介绍partial function

;;partial function:
;指这样的递归函数（考虑递归函数的工作方式）
;对一部分输入值它会在有限次层展开后停止
;而对其他部分的输入值 递归函数就这些值而展开时 不会停止
;（相反的术语是：total function）
;（在明确地定义拓扑结构之前 尽量避免使用拓扑学术语 去讨论类似收敛性的性质）

;;第七诫：
;对于某个递归函数
;如果找到某个对输入值的某一方面的度量（用自然数）
;使这一度量随着递归函数展开的层次的深入而严格递减
;那么就能保证这个递归函数是一个total function

;;假设有一个探究函数性质的函数（谓词）名叫will-stop?
;它能判断一个函数是否是total的
;往证这样的函数的存在性将导出矛盾
;首先作简化 不妨假设它判断的是作用于'()的函数
;考虑它作用于函数bad的值 即考虑bad作用于'()是否会停止
;就展示了不协调性 即矛盾
(define bad
  (lambda (x)
    (and (will-stop? bad)
	 (eternity x))))
(define eternity
  (lambda (x)
    (eternity x)))


;;;其次介绍Ycombinator

;;lambda表达式的作用：
;通常只有当我命名一个非递归函数后
;我才能呼唤它让它作用于自变量
;而lambda表达式使得我可以换直接写出一个非递归函数
;然后指示它让它作用于自变量 而不用给函数命名
;;（Ycombinator的作用：）
;;可是代码对于递归函数的描述是表面上的循环定义
;需要命名函数后才能在定义中循环呼唤它
;为了不给函数命名而可以直接使用它 人们必须另想办法
;办法之一就是使用Ycombinator
;（又比如McCarthy使用label来标记出循环的位置）

;;下面这一段总结自sicp：
;代码对递归函数的描述
;即递归函数的看似循环的定义 可以被看作由函数方程确定函数
;（通常人们说循环定义是错误的
;然而其实经过适当形变后方程通常都给出某些迭代方程
;它们就是循环定义的 但它们并不是错误的 而是有精确语义的
;这里是通过反过来考虑而把循环定义理解为方程 使得一个人首先在观念上对它不再排斥）
;任意一个方程的一个未知元都可以看作一个约束变元（尽管一个惯例是用这个变元来命名方程的解）
;在一个递归函数的定义中 用define命名函数时使用的名称 就是代表未知函数的约束变元
;;Ycombinator是以递归函数为不动点的算子><
;可以说它把上面的函数方程解了出来
;即把函数方程的解用函数显式表示出来
;这个解甚至可以在scheme的解释器中进行计算
;（这里体现了人们对函数的追求 或者更本质地说 是对计算的追求
;因为尽管函数并不总是能用显式计算的 或更进一步高效地用显式计算的
;但是关系的函数表示可能暗示着关系的宜计算性）
;（这里“不动点”是一个拓扑学术语 精确的讨论需要明确一个拓扑结构 并且考虑算子的收敛性 不动点的存在性与唯一性 等等）><
;;用例子来解释如下：
(define ^
  (lambda (x n)
    (cond ((= n 0)
	   1)
	  (else
	   (* x (^ x (- n 1)))))))
(define F
  (lambda (g)
    (lambda (x n)
      (cond ((= n 0)
	     1)
	    (else
	     (* x (g x (- n 1))))))))
;形式上^变为F的过程 就^是变为可以被Y来作用而得到^的形式的过程
;可以这样来理解：
;F中g的第二次出现标出了^的位置
;而g的第一次出现表明这个位置就是被循环调用的位置
;有限次迭代F就发现 对于任意起始函数g
;F的N次迭代对于所有x和小于N的n与^有相同的值
;所以如果用一个可以作无穷循环的函数来迭代F就可以生成^
;;考虑下面的获得无限循环的方式：
((lambda (x) (x x)) (lambda (x) (x x)))
(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))
(Y F)=(F (Y F))
;Y为函数空间中的算子
;但是这个函数没法被实际调用 因为它的递归层次的加深不会停止


;;;回到the little schemer
;想要写出一个可以在scheme中被实际调用的Ycombinator
;（首先要弄清解释器的行为方式）

;;下面用两个平行的例子来作说明
;其中第一个很容易在我写的解释器中被求值
;而要对第二个求值就需要先给我写的解释器增加关于数值计算的内建函数

;;定义一个递归函数
(define length
  (lambda (l)
    (cond ((null? l)
	   0)
	  (else
	   (add1 (length (cdr l)))))))
(define ^
  (lambda (x n)
    (cond ((= n 0)
	   1)
	  (else
	   (* x (^ x (- n 1)))))))

;;不用define如何表达出递归函数？
;;首先对某一有限递归层次以内起作用的函数是可以写出来的
;;（称这种函数为有限层递归函数）
;length0
(lambda (l)
  (cond ((null? l)
	 0)
	(else
	 (add1 (eternity (cdr l))))))
;length1
(lambda (l)
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
;length2
(lambda (l)
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
;^0
(lambda (x n)
  (cond ((= n 0)
	 1)
	(else
	 (* x (whatever x (- n 1))))))
;^1
(lambda (x n)
  (cond ((= n 0)
	 1)
	(else
	 (* x ((lambda (x n)
		 (cond ((= n 0)
			1)
		       (else
			(* x (whatever x (- n 1)))))) 
	       x (- n 1))))))
;^2
(lambda (x n)
  (cond ((= n 0)
	 1)
	(else
	 (* x ((lambda (x n)
		 (cond ((= n 0)
			1)
		       (else
			(* x ((lambda (x n)
				(cond ((= n 0)
				       1)
				      (else
				       (* x (whatever x (- n 1))))))
			      x (- n 1))))))
	       x (- n 1))))))

;;第一次抽象：用一个特殊的算子的迭代来描述某个特殊的递归过程，从而写出有限层递归函数
;（算子指以函数为参数函数为值的函数）
;length0
((lambda (length)
   (lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 (length (cdr l)))))))
 eternity)
;length1
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
  eternity))
;length2
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
  ((lambda (length)
     (lambda (l)
       (cond ((null? l)
	      0)
	     (else
	      (add1 (length (cdr l)))))))
   eternity)))
;^0
((lambda (g)
   (lambda (x n)
     (cond ((= n 0)
	    1)
	   (else
	    (* x (g x (- n 1)))))))
 eternity)
;^1
((lambda (g)
   (lambda (x n)
     (cond ((= n 0)
	    1)
	   (else
	    (* x (g x (- n 1)))))))
 ((lambda (g)
   (lambda (x n)
     (cond ((= n 0)
	    1)
	   (else
	    (* x (g x (- n 1)))))))
  eternity))
;^2
((lambda (g)
   (lambda (x n)
     (cond ((= n 0)
	    1)
	   (else
	    (* x (g x (- n 1)))))))
 ((lambda (g)
    (lambda (x n)
     (cond ((= n 0)
	    1)
	   (else
	    (* x (g x (- n 1)))))))
  ((lambda (g)
     (lambda (x n)
       (cond ((= n 0)
	      1)
	     (else
	      (* x (g x (- n 1)))))))
   eternity)))

;;第二次抽象：用一个对应于某个迭代次数的算子来描述有限次的迭代过程，作用在某个特殊的算子上，从而写出有限层递归函数
;length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 (length (cdr l))))))))
;length1
((lambda (mk-length)
   (mk-length 
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 (length (cdr l))))))))
;length2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 (length (cdr l))))))))
;^0
((lambda (mk^)
   (mk^ eternity))
 (lambda (x n)
   (cond ((= n 0)
	  1)
	 (else
	  (* x (g x (- n 1)))))))
;^1
((lambda (mk^)
   (mk^
    (mk^ eternity)))
 (lambda (x n)
   (cond ((= n 0)
	  1)
	 (else
	  (* x (g x (- n 1)))))))
;^2
((lambda (mk^)
   (mk^
    (mk^
     (mk^ eternity))))
 (lambda (x n)
   (cond ((= n 0)
	  1)
	 (else
	  (* x (g x (- n 1)))))))
;;all names are equal,but some names are more equal than others.
;length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 (mk-length (cdr l))))))))
;length1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 ((mk-length eternity) (cdr l))))))))

;最后终于又得到length！之所以如此是因为只有用到的时候else中的(mk-length mk-length)才会被求一次值
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l)
	    0)
	   (else
	    (add1 ((mk-length mk-length) (cdr l))))))))   

;;第三次也是最后一次抽象：to get back the function that looks like length,by extracting a value and give it a name.
;;;写Ycombinator的方式跟解释器的求值方式有关吗？
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    (mk-length mk-length))))
;;跟踪这个函数就发现它能回到第一次抽象的那种样子，只不过eternity变成了：
((lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    (mk-length mk-length)))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    (mk-length mk-length))))

;;to extract a value and give it a name:
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond ((null? l)
	       0)
	      (else
	       (add1 (length (cdr l)))))))
    (lambda (x) ((mk-length mk-length) x)))));这样作只是为了形式上的强调吗？
;;then extract a value and give it a name:
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x) ((mk-length mk-length) x))))))
 (lambda (l)
   (cond ((null? l)
	  0)
	 (else
	  (add1 (length (cdr l)))))))
;;now we get Y-combinator
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x) ((mk-length mk-length) x))))))

;;test
(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
(define length
  (Y (lambda (length)
       (lambda (l)
	 (cond ((null? l) 0)
	       (else
		(add1 (length (cdr l)))))))))

;;对比一下能用的Y和不能用的Y：
(define Y
  (lambda (f)
    ((lambda (y) (y y))
     (lambda (y) (f (lambda (x) ((y y) x)))))))
(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))



;;这样 我只不过把书上的代码抄下来了而已
;这部分的笔记远远没有结束
;理解还不够透彻
;希望之后能让解释器在求值的过程中展示自己的行为
;从而使我对Y的作用有更好的理解

