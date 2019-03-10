;;;; D - 一刀両断

; -*- 問題文 -*-{{{
;高橋くんは鍛錬の結果、空手チョップで木の板を切断できるようになりました。空手チョップの軌道を表す線分と板の形を表す多角形が与えられるので、板がいくつに切断されたか求めてください。
;
;## 入力
;入力は以下の形式で標準入力から与えられる。
;
;```
;Ax Ay Bx By
;N
;X1 Y1
;X2 Y2
;:
;XN YN
;```
;
;1行目には、線分の端点の座標 Ax,Ay,Bx,By がスペース区切りで与えられる。
;2行目には、多角形の頂点数 N(3≦N≦100)が与えられる。
;3行目からの N 行では、各頂点の座標 Xi,Yiがスペース区切りで与えられる。
;入力で与えられる座標は-1000以上1000以下の整数である。
;
;入力で与えられる線分と多角形は以下の性質を満たす。
;- 多角形の頂点は反時計回りの順で与えられる。
;- 多角形の頂点は線分から0.1以上離れている。
;- 線分の端点は多角形から0.1以上離れている。
;- 線分の端点は多角形の外部にある。
;- 多角形の連続する３頂点が一直線上に並ぶことはない。
;
;すなわち、以下のような入力は与えられない。
;- A,B:多角形の頂点が線分上にある。
;- C:線分の端点が多角形の辺上にある。
;- D:多角形の辺と線分が重なる。
;- E:線分の端点が多角形の内部にある。
;
;## 出力
;板がいくつに切断されるかを出力せよ。出力の末尾には改行をつけること。
;
;## 入力例1
;```
;-2 0 2 0
;4
;1 1
;-1 1
;-1 -1
;1 -1
;```
;
;## 出力例1
;```
;2
;```
;
;## 入力例2
;```
;-3 1 3 1
;8
;2 2
;1 2
;1 0
;-1 0
;-1 2
;-2 2
;-2 -1
;2 -1
;```
;
;## 出力例2
;```
;3
;```
;;}}}

;;; -*- 本体 -*-{{{
(defun solve (stream)
  "問題を解く"
  (defparameter *in-stream* stream)
  (defparameter *demarcation* (get-segment (read-point) (read-point)))
  (defparameter *vertex-num* (read *in-stream*))
  (let ((first-point (read-point)))
    (walk-along 2 (get-segment first-point (read-point)) 0 first-point)))

(defun walk-along (index segment acc &optional (first-point nil))
  "領域の頂点を始点(1番)から巡る
   index 現在の頂点の番号
   segment 現在の頂点を終点とする線分
   acc 分断領域数の計算用"
  (if (intersect-segments-p *demarcation* segment) (incf acc))
  (cond ((< index *vertex-num*) (walk-along (1+ index) (get-segment (cdr segment) (read-point)) acc first-point))
        ((= *vertex-num* index) (walk-along (1+ index) (get-segment (cdr segment) first-point) acc))
        ((> index *vertex-num*) (floor (+ (/ acc 2) 1)))))
;}}}

; -*- ユーティリティ -*-{{{
(defun get-segment (p1 p2)
  "線分を取得する
   p1 始点(x1 . y1)
   p2 終点(x2 . y2)
   return ((x1 . y1) . (x2 . y2))"
  (cons p1 p2))

(defun read-point ()
  "*in-stream*から点の座標を読み込む
   return (x . y)"
  (cons (read *in-stream*) (read *in-stream*)))
;}}}

;;; -*- 交差判定 -*-{{{
(defun intersect-segments-p (segment1 segment2)
  "線分同士の交差判定
   -----segment1----->
   -----segment2----->
   return t 交差している | nil 交差していない"
  (and (intersect-lines-p segment1 segment2)
       (intersect-lines-p segment2 segment1)))

(defun intersect-lines-p (line1 line2)
  "直線同士の交差判定
   a -----line1-----> b
   c -----line2-----> d
   return t 交差している | nil 交差していない"
  (let* ((a (car line1)) (b (cdr line1)) (c (car line2)) (d (cdr line2))
         (sign-c (- (* (- (cdr a) (cdr b)) (- (car c) (car a)))
                    (* (- (car a) (car b)) (- (cdr c) (cdr a)))))
         (sign-d (- (* (- (cdr a) (cdr b)) (- (car d) (car a)))
                    (* (- (car a) (car b)) (- (cdr d) (cdr a))))))
    (< (* sign-c sign-d) 0)))
;}}}


;;; -*- テスト -*-{{{

;;; テストデータ
(defparameter *test-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A" (sb-posix:getcwd) "/testdata/input/")))
(defparameter *test-data-list* '("01.txt" "02.txt" "03.txt" "04.txt" "05.txt"))

;;; 期待値データ
(defparameter *expected-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A" (sb-posix:getcwd) "/testdata/output/")))
(defparameter *expected-data-list* '("01.txt" "02.txt" "03.txt" "04.txt" "05.txt"))


(defun test-solve (test-data-path expected-data-path)
  "テスト
   test-data-path テストデータのパス
  expected-data-path 期待値データのパス"
  (with-open-file (in-test test-data-path)
    (with-open-file (in-expected expected-data-path)
      (let ((actual (solve in-test))
            (expected (read in-expected)))
        (princ "  actual   ") (princ actual) (princ #\newline)
        (princ "  expected ") (princ expected) (princ #\newline)
        (if (eql actual expected)
            t
            nil)))))


;;; 全テストデータに対してテスト実施
(if (every (lambda (data)
             (let ((test-data (first data))
                   (expected-data (second data)))
               (princ "test data:     ") (princ test-data) (princ #\newline)
               (princ "expected data: ") (princ expected-data) (princ #\newline)
               (test-solve
                 (merge-pathnames *test-data-dir* test-data)
                 (merge-pathnames *expected-data-dir* expected-data))))
           (apply #'mapcar #'list (list *test-data-list* *expected-data-list*)))
    (princ "OK")
    (princ "NG"))

;}}}

