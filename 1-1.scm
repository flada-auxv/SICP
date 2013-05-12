;;;;;;;;;;;;;;;;;;;; ex 1.3 ;;;;;;;;;;;;;;;;;;;
;; 3つの数を引数としてとり、大きい二つの数の二乗の和を返す手続きを定義せよ。
(define (square n)
  (* n n))

;; "一番小さい数字を除く"・"二乗する"の手続きが混ざってる
(define (square-of-larger-two a b c)
  (cond ((< a b)
	 (if (< a c) (+ (square b) (square c))
		     (+ (square a) (square b))))
	((< b c) (+ (square a) (square c)))
	(else (+ (square a) (square b)))))

;; 分けたらこんな感じ
(define (square-of-larger-two2 a b c)
  (+ (square a)
     (square b)
     (square c)
     (- (square (smallest-of-three a b c)))))

(define (smallest-of-three a b c)
  (cond ((< a b)
	 (if (< a c)
	     a
	     c))
	((< b c) b)
	(else c)))

;;;;;;;;;;;;;;;;;;;; ex 1.4 ;;;;;;;;;;;;;;;;;;;;
;; われわれの評価モデルは、演算子が合成式である組み合わせでも使える事を観察せよ。
;; それに従って、次の手続きの振る舞いを述べよ.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 基本式が演算子である場合と同様に、この合成式の組み合わせの要素を評価して得た手続きを、被演算子に作用させる。
;; すなわち、bが0より大きければ+という手続きを、bが0以下であれば-という手続きを得て、その手続きをa・bという被演算子に作用させる。
;; つまり、bが負数の場合は減算する事で、aにbの絶対値を加算する様に振る舞う事になる。

;;;;;;;;;;;;;;;;;;;; ex 1.5 ;;;;;;;;;;;;;;;;;;;;
;; (問題文簡約)作用的順序の評価と正規順序の評価について下の例を用いてその振る舞いの違いを説明せよ。
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; (test 0 (p))

;; 作用的順序の評価とは「引数を評価し作用させる」方法であり、(test 0 (P))は合成手続きである演算子のtestが評価され、
;; 被演算子の0と(p)の評価結果をそれに作用させる事になる。
;; しかし、pの定義はbodyにおいてpを評価するようになっており、その堂々巡りから抜け出せない。

;; 一方で、正規順序の評価とは「完全に展開し、完訳する」方法であり、testを評価した後にpを評価しない。つまり、
(if (= 0 0)
    0
    (p))
;; となり、問題文にあるifの評価規則からpの評価は最終的にも回避される為、前者とは異なり0という評価結果を得るだろう。

;; また、実際に(test 0 (p))をscheme処理系(goshしか試してない)で評価させると、評価結果得を得られない事から
;; 作用的順序の評価に従っているという事が予測される。

;;;;;;;;;;;;;;;;;;;;; 1.1.7 ;;;;;;;;;;;;;;;;;;;;;
;; Newton法による平方根
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
    
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;; ex 1.6  ;;;;;;;;;;;;;;;;;;;;;
;; (問題文簡約)下の処理を評価した時、何が起こるか。
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter2 (improve guess x)
		     x)))
(define (sqrt2 x)
  (sqrt-iter2 1.0 x))
; (sqrt2 10000)

;; new-ifは通常の合成手続きなのでその評価をする際には引数を全て評価しそれを作用させていくことになる。
;; 対してifについては述語が評価された結果によって、then-clause/else-clauseのいずれかしか評価されない(特殊形式)。
;; 次に、sqrt-iter2におけるnew-ifの使われ方を見てみると、仮引数else-clauseに対応する式が
;; (sqrt-iter2 (improve guess x) x)と与えられいる。
;; どのような引数guess/xが与えられてもelse-clause式が評価され再帰呼び出しが行われるので無限ループへと陥る事になる。

;; 下の例が分かりやすい。
(new-if (= 0 1) (define a 10) (define b 10))
; a => 10, b => 10
(if (= 0 1) (define a 20) (define b 20))
; a => 10, b => 20

;;;;;;;;;;;;;;;;;;;;; ex 1.7 ;;;;;;;;;;;;;;;;;;;;;
; (square (sqrt 0.0001)) ;=> 1.3631669067247127e-4 => 0.00013631669067247127 結構な誤差が発生している
; (sqrt 10000000000000)  ;=>  帰ってこない

;; ある繰り返しから次へのguessの変化に着目し、変化が予測に比べて非常に小さくなった時に止める様に改良する。
(define (diff-good-enough? guess prev)
  (< (abs (- 1 (/ guess prev))) 0.001))
  
(define (sqrt-iter3 guess prev x)
  (if (diff-good-enough? guess prev)
      guess
      (sqrt-iter3 (improve guess x)
		 guess
		 x)))
(define (sqrt3 x)
  (sqrt-iter3 1.0 2.0 x))

; (square (sqrt3 0.0001))     ;=> 1.0000000050981486e-4 => 0.00010000000050981486
; (sqrt3 1000000000000000000) ;=> 1.0000000691683831e9
; 改善されている！

;;;;;;;;;;;;;;;;;;;;; ex 1.7 ;;;;;;;;;;;;;;;;;;;;;
;; (問題文簡約)yがxの立方根の近似なら、よりより近似を得る方法はcr-improveの通りである。
;; 平方根を求めた時と同じ様にして立方根の場合の手続きを実装せよ。
(define (cr-improve x y)
  (/ (+
      (/ x
	 (square y))
      (* 2.0 y))
     3.0))

(define (cr-good-enough? improved y)
  (< (abs (- improved y)) 0.001))

(define (curt-iter x y)
  (define improved (cr-improve x y))
  (if (cr-good-enough? improved y)
      improved
      (curt-iter x improved)))

(define (curt x)
  (curt-iter x 1.0))

;; cr-improveは問題文の数式をs式に直したものだけど、ここで与えられる"xとy"と以前の"xとguess"でだいぶ混乱した。
;; 立方根の近似を求める式で与えられる"xとy"に沿うように引数の取り方など、平方根の時より微修正している。
;; x:元の数値, y:近似値(最初は推測), improved:よりよい立方根の近似を求める式の評価結果

