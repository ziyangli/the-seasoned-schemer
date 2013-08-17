
;;;the little schemer 不光是教人们编程
;;;它的 对话体 同时还教人们应该如何学习
;;;耐心 恒久的努力 认真弄清最简单的例子 勤奋地去练习

;;（这个文本是可以直接load到一个解释器中使用的）

;;;这里的代码来自``the little schemer''的章节：what is the value of all this?
;;更改的部分为：
;1 apply的名字改为myapply避免测试时与元解释器内建的函数名冲突
;2 增加对'()的处理
;3 为了测试的目的而改变一个报错方式

;;必要的术语：
;1 我写的解释器直接被称作“我写的解释器”（当然这其实是Friedman写的解释器）
;2 我写的解释器所嵌入的解释器被称作“元解释器”

;;彻底弄清解释器的行为方式至关重要（比如当需要写出Ycombinator的时候）
;;彻底弄清解释器的行为方式的最好方法就是把它写出来
;“这里写出一个scheme解释器”是指写出一个value函数 在另一个scheme解释器（元解释器）中调用这个value函数
;;写好的value函数的调用方式将是：
; > (value 'EXP)
;; 或：
; > (value (quote EXP)
;（重点在于 需要“引用”才能让函数value按我的意图接收S-exp 因为在元解释器中作为参数的S-exp会被预先求值）
;;其中“需要被我写的解释器正常接受的S-exp”记为EXP
;;用数理逻辑里常用的作归纳定义的表达方式定义如下：
;;（这种表达方式 人们美其名曰BNF 可以描述一个基数为阿列夫零的无穷集中的所有元素 正则表达是也有同样功能）
;;（分成多个式子来写是为了展示解释器中的分类方式）
; EXP ::= ATOM | LIST
; ATOM ::= <数字> | <逻辑值> | <字符串> [即形如'~~~的东西 其中不能出现左右小括号和空格等等字符]><{这个“等等字符”需要明确}
; LIST ::= (EXP EXP ...) [括号中可以有任意有限多个EXP 可以是零个]
;;（书中原来的解释器不处理'() 对'()的处理是我加的 我认为这样是合理的）

;;那么写value函数的基本思路自然就是 先给所有需要被求值的S-exp（即EXP）分类 再按类型求值

;;;按书中的顺序 首先是一些辅助函数与数据结构：
;;entry是用来记录一组names与一组values之间的命名关系的数据结构
;考虑怎样一个树适合用来完成这个任务就明白entry的构造了
;即一个names的list和一个同等长度的values的list作成的pair
;（pair这个术语被滥用了 这里指只含有两个元素的list 其他地方又指形如(A . B)的东西）
(define build-pair
  (lambda (a b)
    (cons a (cons b '()))))
;为什么在文本中在用build-pair对new-entry来作定义之前必须先定义build-pair？><
;这是测试中遇到的问题
;难道那些解释器解析文本的时候中途遇到某个未定义的name不会往下看吗？
(define new-entry build-pair)
(define first
  (lambda (l)
    (car l)))
(define second
  (lambda (l)
    (cadr l)))
;当在entry中找不到name时返回(entry-f name)
;（-f这个字母 书中理解为function 其实也可以理解为false 即error 即“出错了！”）
;考虑下面的environment数据结构就知道
;当在一个entry中找不到name时不把这个name扔掉
;是因为还要在environment中的下一个entry中找name
;（之所以需要entry与emvironment这两个数据结构
;是因为解释器解释lambda表达式与lambda表达式的嵌套时会临时形成这样的数据结构）><
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))
;当然要先把entry中的names与values拆开 然后交给辅助函数处理
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond 
     ((null? names)
      (entry-f name))
     ((eq? name (car names))
      (car values))
     (else
      (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))
;;environment(aka.table)是entries的list
;考虑在environment中查找name的方式就知道
;这个数据结构使得一个name所对应的新value可以覆盖它所对应的旧value
(define extend-table cons)
(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table)
	   (table-f name))
	  (else
	   (lookup-in-entry name 
			    (car table)
			    (lambda (name)
			      (lookup-in-table name (cdr table) table-f)))))))


;;按解释器作用方式（或者说 求值方式 或者说 解释方式）对EXP作分类
;也就是说解释器 即value函数 必须识别出所有类型 并且按类型分别作用
;只有7种类型而已
(define value
  (lambda (e)
    (meaning e '())))
;meaning指在某个特殊的environment中求值
;之所以需要meaning作为value的辅助函数
;是因为尽管最开始待求值的EXP是不知道任何环境的
;但是在递归的过程中会临时生成environment
;（这种类型的辅助函数在``the seasoned schemer''的第一章中有详细的讨论）
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
;首先对EXP的一个过渡性的分类是ATOM与LIST
;;这里有涉及到如何报错的一些问题的：
;元解释器中atom被定义为非null且非pair（术语pair被滥用了 这里它指形如(A . B)的东西）
;因此交给list-to-action处理的是元解释器中的null和pair
;如果简化一下而不考虑pair那么交给list-to-action处理的是元解释器中的list
;即那些带括号的东西 且括号中的多个东西用空格隔开的而不是用点隔开的
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e)
      (atom-to-action e))
     (else
      (list-to-action e)))))
;ATOM类EXP对应*const与*identifer两类作用方式（identifer本意“标识符”在这里指变元）
;;这里可以体会一下从解释器到lisp的基本公理的化归（数学或者逻辑学的兴趣）
;尽管下面使用了谓词number?但者是非本质的
;因为自然数这个数据结构很容易用lisp的基本公理实现
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e)
      *const)
     ((eq? e #t)
      *const)
     ((eq? e #f)
      *const)
     ((eq? e 'cons)
      *const)
     ((eq? e 'car)
      *const)
     ((eq? e 'cdr)
      *const)
     ((eq? e 'null?)
      *const)
     ((eq? e 'eq?)
      *const)
     ((eq? e 'atom?)
      *const)
     ((eq? e 'zero?)
      *const)
     ((eq? e 'add1)
      *const)
     ((eq? e 'sub1)
      *const)
     ((eq? e 'number?)
      *const)
     (else
      *identifer))))
;LIST类EXP对用*null *quote *lambda *cond *application五类作用方式
(define list-to-action
  (lambda (e)
    (cond ((null? e)
	   *null)
	  ((atom? (car e))
	   (cond ((eq? (car e) 'quote)
		  *quote)
		 ((eq? (car e) 'lambda)
		  *lambda)
		 ((eq? (car e) 'cond)
		  *cond)
		 (else
		  *application)));><
	  (else
	   *application))))


;;下面是不同类型的作用的细节
;注意 它们与上面执行分类功能的代码是分离的
;而上面的代码单纯的执行分类的任务没有递归
;所以递归的任务全在作用的细节中完成

;最简单的是*const
;它把数字与逻辑值按原样输出
;而给字符串常量贴上primitive的标签
;以表明是这个解释器中所配备的基本函数与谓词
(define *const
  (lambda (e table)
    (cond ((number? e)
	   e)
	  ((eq? e #t)
	   #t)
	  ((eq? e #f)
	   #f)
	  (else
	   (cons 'primitive (cons e '()))))))

;前面所叙述的enrty与environment数据结构以及相关函数是为且仅为*identifer这种类型的作用而准备的
;注意一个形如'~~~的string需要被解释的时候才会被认为是name（或者说identifer）
;否则它就被认为是一个单纯的string
(define *identifer
  (lambda (e table)
    (lookup-in-table e table notfound)))
;当需要用到lookup-in-table的第四个参数的时候就是找不到某个名称所对应的值的时候
;书中使用(car '()))) 这样就使用户得到一个元解释器中的错误信息
;而这里使用一个字符串 即形如'~~~的东西来报错
;好处是 这个字符串标记了错误之后还可以被尽量地处理
;直到实在错上加错了为止 这个性质究竟是好是坏就因理解方式而异了
;;这写问题只有在实践中才会被处理 在纯理论讨论中是不作处理的
;因为就这里的认识论而言“错误永远都是非本质的”
(define notfound
  (lambda (name)
    ;(car '())))
    'an-error-occur:at-least-one-name-is-not-found-in-the-current-environment))

;最最简单的*null
(define *null
  (lambda (e table)
    '()))

;*quote完成函数到数据的转换
;它之所以能完成这种任务是因为McCarthy把函数与数据的表达方式设计成了一致的
;其实它仅仅是使得人们可以简单的给EXP前面加一个东西
;而使得这个前面加了东西的EXP经过解释器时仅仅是再去掉前面的东西
;因为在解释器中被函数作用的参数需要先被求值（只有亲自写出一个解释器之后才能很好的理解这一点）
;所以为了把EXP直接当作被函数作用的参数就需要这种技巧
;它的存在使得我在我写的这个解释器里再写一个解释器成为可能><
;（考虑 在元解释器中对解释器的测试）><
;（考虑 在元解释器中写解释器时对我写的解释器中的函数的处理方式）><
(define *quote
  (lambda (e table)
    (text-of e)))
(define text-of second)

;被*lambda来作用的是non-primitive
;它们是以基本函数和基本谓词为基础 在lambda表达式的语义下被表达出来的函数和谓词
;经*lambda作用后得到的是：
;“记录当时environment的table” 与 “型参formals” 与 “函数体body” 所形成的list（在后面这部分被称作closure）
;前面再贴上一个non-primitive的标签
;函数就是以这种方式在我的解释器中被视为数据的
(define *lambda
  (lambda (e table)
    (list 'non-primitive
	  (cons table (cdr e)))))
(define table-of
  (lambda (non-primitive)
    (car non-primitive)))
(define formals-of
  (lambda (non-primitive)
    (cadr non-primitive)))
(define body-of
  (lambda (non-primitive)
    (caddr non-primitive)))

;cond的功能是分支控制
;它把谓词的结果转化成其他的东西
;有点像“反谓词” 但严格意义上讲这种EXP的类型是独特的
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)
;如果没有一个条件是真的 那么最后一层递归的cond-line为'() 会出现(car '())的错误
;保持cond-lines的结尾是else 就不会出现这种错误
;;注意这里递归调用了meaning
(define evcon
  (lambda (lines table)
    (cond ((else? (question-of (car lines)))
	   (meaning (answer-of (car lines)) table))
	  ((meaning (question-of (car lines)) table)
	   (meaning (answer-of (car lines)) table))
	  (else
	   (evcon (cdr lines) table)))))
(define question-of
  (lambda (x)
    (car x)))
(define answer-of
  (lambda (x)
    (cadr x)))
(define else?
  (lambda (x)
    (cond ((atom? x) 
	   (eq? x 'else))
	  (else
	   #f))))

;只剩最后一个作用方式*application了
;它的任务繁重 它是使用递归调用的主要部分（另外还使用递归调用的部分就只是*cond而已）
;被它作用的EXP是一个car为函数的list
;递归调用meaning作用于这个出现在car位置的函数
;使它成为以我定义的方式在我写的解释器里表达函数语义的EXP
;即前面贴着primitive或non-primitive标签的EXP 这类EXP中是有参数arguments（或称为 实参）的
;这里还必须把参数的意义（在某个environment下）追问到底 这又是需要递归调用meaning的地方
;（被函数作用的参数是先被完全求值的 这是我写的解释器的一个极重要特征）
;执行追问所有argunmets意义的这部分任务的是evlis(evaluation list)函数
(define evlis
  (lambda (args table)
    (cond ((null? args)
	   '())
	  (else
	   (cons (meaning (car args) table)
		 (evlis (cdr args) table))))))
(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)
;apply是重要的函数（为了避免与元解释器中的apply冲突而重命名为myapply）
;value和apply作为解释器的两部分 如太极生两仪
;（写解释器的方式不止有这一中 sicp中有另一种很酷的写法 它们在性质上有何区别？）><
;apply的第一参数为在我写的解释器里表达函数语义的EXP 第二参数为在我的解释器的语境下被充分求值了的参数list
;首先当然要分类 前面之所以要区分primitive non-primitive这两类函数就是因为它们被apply作用的方式不同
(define myapply
  (lambda (fun vals)
    (cond ((primitive? fun)
	   (apply-primitive (second fun) vals))
	  ((non-primitive? fun)
	   (apply-closure (second fun) vals)))))
(define primitive?
  (lambda (l)
    (eq? (car l) 'primitive)))
(define non-primitive?
  (lambda (l)
    (eq? (car l) 'non-primitive)))
(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))
;为什么要重新定义atom?这个谓词为:atom?
;因为所希望得到的atom?应该把函数判断为#t
;可是以我定义的方式在我写的解释器里表达函数语义的EXP
;在元解释器中的语义却是一个普通的list
;所以仅用元解释器里的atom?作apply-primitive中的谓词是不够的
(define :atom?
  (lambda (x)
    (cond ((atom? x)
	   #t)
	  ((null? x)
	   #f)
	  ((eq? (car x) 'primitive)
	   #t)
	  ((eq? (car x) 'non-primitive)
	   #t)
	  (else
	   #f))))
;apply-closure必须先通过extend-table来更新environment
;然后把closure的body在新的environment中求值
;注意这一切都是在我的解释器的语境下的
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table (new-entry (formals-of closure) vals)
			   (table-of closure)))))

;;;下面的补充函数是为了测试用的
(define atom?
  (lambda (x)
    (and (not (pair? x))
      	 (not (null? x)))))
(define add1
  (lambda (x)
    (+ 1 x)))
(define sub1
  (lambda (x)
    (- x 1)))
