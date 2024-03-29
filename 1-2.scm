;;;;;;;;;;;;;;;;;;;; 1.2.1 ;;;;;;;;;;;;;;;;;;;;;;;;;
; ・n! = n * (n - 1) * (n - 2) * ... 3 * 2 * 1 = n * (n - 1)!
; ・1!は1に等しい
; 以上の規定から階乗関数の手続きを下のように表せる
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; (factorial 4)
; (* 4 (factorial 3))
; (* 4 (* 3 (factorial 2)))
; (* 4 (* 3 (* 2 (factorial 1))))
; (* 4 (* 3 (* 2 1)))
; (* 4 (* 3 2))
; (* 4 6)
; 24
; 乗算が即座に実行されず列をなしていく様子を遅延演算の列を作る、という様に表す。
; これによって特徴づけられるこのようなプロセスを再帰的プロセスという。
; このプロセスの実行には、後に実行する演算を覚えておく必要がある。


; また、別の表し方として、1からnまで数えるカウンタとともに部分積を保持し、カウンタ(counter)と積(product)を
; product = counter * product
; counter = counter + 1
; と擬似的に表すような規則に従って変化させるとすると、n!はカウンタがnを超えた時の積の値である事になる。

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (< max-count counter)
      product
      (fact-iter (* product counter)
		 (+ counter 1)
		 max-count)))
      
; (factorial 4)
; (fact-iter   1 1 4)
; (fact-iter   1 2 4)
; (fact-iter   2 3 4)
; (fact-iter   6 4 4)
; (fact-iter  24 5 4)
; 24           p c n
; こちらのプロセスでは任意のnに対して覚えておくのはp,c,nで表した3つの状態変数のみである。
; このようなプロセスを反復的プロセスと言う。
; 一般に、その状態が一定個数の状態変数、状態が移ったとき状態変数がどのように更新されるかの規則、
; プロセスの終了を規定する終了テストという要素によって総括できる。

; ここでプロセスと手続きの違いに注意。
; 再帰的手続きとは構文上の事実を、再帰的プロセスとは演算の様を表す。
; (反復的プロセスは再帰的手続きによって表されると言えば分かりやすいか)


;;;;;;;;;;;;;;;;;;;; ex 1.9 ;;;;;;;;;;;;;;;;;;;;;;;;;
;; 以下の手続きについて、(+ 4 5)を評価するときに生成するプロセスを示せ。
;; また、そのプロセスは反復的か再帰的か。
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; 再帰的プロセス

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; 反復的プロセス

;;;;;;;;;;;;;;;;;;;; ex 1.10 ;;;;;;;;;;;;;;;;;;;;;;;;;
; Ackermann関数
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10) ;=> 1024
(A 2 4)  ;=> 65536
(A 3 3)  ;=> 65536

; Aを上で定義した手続きとして、次の手続きを考える。
(define (f n) (A 0 n)) 
(define (g n) (A 1 n)) 
(define (h n) (A 2 n)) 
(define (k n) (* 5 n n)) 
; ここで、正の整数nに対して手続きf,gおよびhが計算する関数の簡潔な数学的定義を述べよ。
; 例えば(k n)は5nの2乗を計算する。

; 答え
; (f n) = 2n
; (g n) = 2^n
; (h n) = 2^2^...(n回)

; 展開してみるとすると
; (A 0 3)
; (* 2 3)
; 6

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
; (A 0 (A 0 (A 0 (A 0 64))))
; (A 0 (A 0 (A 0 128))))
; (A 0 (A 0 256))))
; (A 0 512))))
; 1024

; 愚直にやるとこうなるけども、(f n) = (A 0 n) = 2 * nを利用して
; プロセスが一番膨張している所で (f (f (f (f (f (f (f (f (f (A 1 1)))))))))) となり、
; fの個数がn - 1個で、それらを掛け合わせた結果に (A 1 1) = 2 をさらに掛け合わせている事から
; (g n) = (2 ^ (n - 1)) * 2 = 2 ^ n という事が分かる。

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2))))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2)))))
; (A 1 (A 0 (A 0 4)))))
; (A 1 (A 0 8))
; (A 1 16) ;=> 2 ** 16 = 65536

; こちらも同様にして(A 1 16)までいかずとも
; (A 1 (A 1 (A 1 (A 2 1) の段階で (g (g (g 2))) となる。
; 良く分からないが 2 ^ 2 ^ ... (n回) という事?
