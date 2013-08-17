
;;;下面是为interpreter.scm写的一个测试
;;注意 这个文档本身不被直接load
;;而需要把代码复制到一个解释器里去执行
;;这样作测试虽然有些繁琐但是很容易理解
;;这也可以被当作一个用来熟悉scheme各个解释器的学习的过程

;;;注意与测试所使用的解释器的细节的相关性
;;;不同解释器的一些相关细节记录如下：
;;;（不考虑chez系列因为它不开源）

;;racket
;load入的函数名不会覆盖racket内建的函数名
;（这是非正常行为 其他解释器都不会有这种行为）
;因此只有更改书中apply的函数名才能作测试 因为apply是racket内建的
;更改apply名称为myapply之后测试不知何故还是失败><（输入没有回复）
;内建add1与sub1 而不是1+与1-

;;mit-scheme(R5RS)
;load代码时不作编译
;内建1+ 而没有1-与add1与sub1

;;guile(R5RS)
;名字最酷 因此建议使用
;（其实是GNU Ubiquitous Intelligent Language for Extensions）
;load代码时会作编译
;内建1+与1- 而没有add1与sub1

;;chicken(R5RS)
;这个名称下的是编译器 其解释器名为csi
;内建add1与sub1 没有1+与1-

;;sheme48(R5RS)
;没有内建add1与sub1与1+与1-
;load interpreter.scm时会报错 因为这个解释器会提前检查代码中的错误
;因为我代码中的(car '())而报错
;因此为了用它来作测试需要一些处理：
;代码中的(car '())是报错用的
;因此可以更改报错方式 比如用字符串来报错

;;ikarus(R6RS)
;（看上去很像chez）
;这个解释器其实是个实时的编译器
;如果不更改apply的名称让它与内建的函数重名
;输入apply查看解释结果 表面上看是把内建函数覆盖了
;但是测试还是会出错 可能在value调用函数apply的时候调用的还是内建的函数
;内建add1与sub1 没有1+与1-

;;larceny
;名字更酷 因此建议使用
;><

;;gambit
;><

;;对书中的原来的代码作适当的修改之后在上面racket之外的所有解释器中测试都不成问题

;;;测试方式是：
;;在元解释器中调用value函数如下：
; > (value 'EXP)
;; 或：
; > (value (quote EXP)
;;需要“引用”才能让函数value按我的意图接收S-exp
;;因为在元解释器中作为参数的S-exp会被预先求值

;;;需要被我写的解释器正常接受的S-exp记为EXP
;;用数理逻辑里常用的作归纳定义的表达方式定义如下：
;;（分成多个表达是来写是为了展示解释器中的分类方式）
; EXP ::= ATOM | LIST
; ATOM ::= <数字> | <逻辑值> | <字符串> [即形如'~~~的东西 其中不能出现左右小括号和空格等等字符]><{这个“等等字符”需要明确}
; LIST ::= (EXP EXP ...) [括号中可以有任意有限多个EXP 可以是零个]
;;（书中原来的解释器不处理'() 对'()的处理是我加的 我认为这样是合理的）

;;;按作用方式分类测试
;;其实不用考虑*identifer
;因为需要被以这种方式作用的S-exp只在value函数的作用过程中临时产生 而不作为valeu函数的输入
;对*identifer的测试可以看作对报错情况的测试
(value 'not-const)

;;*const
;数字与逻辑值
(value 1)
;下面两个其实已经测试到*application了
(value '(add1 1))
(value '(sub1 1))
(value #t)
(value #f)

;基本函数primitive
(value 'add1)
(value 'car)
(value 'atom?)

;;*null
(value '())

;;*quote
(value '(quote a-quoted-string))
(value '(quote (a-quoted-string)))
(value '(quote (lambda (x) (add1 (add1 x)))))

;;*lambda 非基本函数non-primitive
(value '(lambda (x) (add1 (add1 x))))

;;*cond
;这里其实已经测试到*application了
(value '(cond ((eq? 1 2) 123) (else 321)))
(value '(cond ((eq? 1 kkk) 123) (else 321)))
(value '(cond ((eq? cons car) 123) (else 321)))

;;*application
;对这中类型作用的测试
;就是对value作用于那些以 函数作用于参数 为语义的S-exp的测试
;因此之前的某些已经测试到这种作用情况了
(value '(cons 1 '()))
(value '((lambda (x) (cons 'drink (cons x '())))
	 'milk))
(value '((lambda (y) (cond 
		      ((eq? y 'thirst)
		       ((lambda (x) (cons 'drink (cons x '())))
			'water))
		      ((eq? y 'not-thirst)
		       ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
			'water))
		      (else
		       'what-ever)))
	 'thirst))
(value '((lambda (y) (cond 
		      ((eq? y 'thirst)
		       ((lambda (x) (cons 'drink (cons x '())))
			'water))
		      ((eq? y 'not-thirst)
		       ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
			'water))
		      (else
		       'what-ever)))
	 'not-thirst))
;这里用到的是有else保护的cond
(value '((lambda (y) (cond 
		      ((eq? y 'thirst)
		       ((lambda (x) (cons 'drink (cons x '())))
			'water))
		      ((eq? y 'not-thirst)
		       ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
			'water))
		      (else
		       'what-ever)))
	 'do-not-tell-you))
;;而如果不用else就会可能报错
;报出的是元解释器中的(car '())错误
;(value '((lambda (y) (cond 
;		      ((eq? y 'thirst)
;		       ((lambda (x) (cons 'drink (cons x '())))
;			'water))
;		      ((eq? y 'not-thirst)
;		       ((lambda (x) (cons 'do (cons 'not (cons 'drink (cons x '())))))
;			'water))))
;	 'do-not-tell-you))


;通篇复制这个文档到解释器中执行
;测试结果是：（我使用的是ikarus）
;an-error-occur:at-least-one-name-is-not-found-in-the-current-environment
;> 1
;> 2
;> 0
;> #t
;> #f
;> (primitive add1)
;> (primitive car)
;> (primitive atom?)
;> ()
;> a-quoted-string
;> (a-quoted-string)
;> (lambda (x) (add1 (add1 x)))
;> (non-primitive (() (x) (add1 (add1 x))))
;> 321
;> 321
;> 321
;> (1)
;> (drink milk)
;> (drink water)
;> (do not drink water)
;> what-ever
