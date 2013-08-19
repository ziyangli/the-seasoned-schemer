
;;;the little and seasoned schemer不光是教人们编程
;;;它的 对话体 同时还教人们应该如何学习
;;;耐心 恒久的努力 认真弄清最简单的例子 勤奋地去练习

;;这个文档描述一些``the seasoned schemer''中必要的辅助函数和惯例
;;需要的时候可以把代码粘贴到这个文档中 通过load这个文档来作测试

(define atom?
  (lambda (x)
    (and (not (pair? x))
	 (not (null? x)))));'()不是atom
(define add1
  (lambda (x)
    (+ x 1)))
(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
(define-syntax letcc
  (syntax-rules ()
    ((letcc var body ...)
     (call-with-current-continuation
      (lambda (var)  body ... )))))
(define-syntax try
  (syntax-rules ()
    ((try var a . b)
     (letcc success
	    (letcc var (success a)) . b))))
;;以上是the little schemer和the seasoned schemer中的约定
;;为了教学的目的 这里通过作约定而把scheme剪裁成了 更简单的版本
;;简约到一个初学者写的解释器就能完成其中的功能
;;（不去想这个编程语言最多能作多少 而是想它最少能做多少
;;从最本源的地方不带妄想地行动 佛法谓之初心也）
;;其他的约定是：
;1 null?只作用于list
;2 eq?只作用于两个non-numeric atom



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
