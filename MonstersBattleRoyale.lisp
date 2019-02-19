;;;; C - Grand Garden

; -*- 問題文 -*-{{{
;N体のモンスターが居て、それぞれ 1, 2, ..., Nと番号付けられています。
;
;はじめ、モンスター iの体力は A_iです。
;以降、体力が1以上のモンスターを生きているモンスターと呼びます。
;生きているモンスターが1体になるまで以下を繰り返します。
;- ランダムに1体の生きているモンスターがランダムに別の生きているモンスターに攻撃します。
;- その結果、攻撃されたモンスターの体力を攻撃したモンスターの体力と同じ値だけ減らします。
;
;最後に生き残ったモンスターの最終的な体力の最小値を求めてください。
;
;制約
;* 入力は全て整数である。
;* 2 <= N_i <= 10^5
;* 1 <= A_i <= 10^9
;
;入力
;入力は以下の形式で標準入力から与えられる。
;N
;A_1 A_2 ... A_N
;
;出力
;最後に生き残ったモンスターの最終的な体力の最小値を出力せよ。

;}}}


;;; -*- 本体 -*-{{{

(defun solve-corner-cutting-version (stream)
  "問題を解く(手抜きバージョン)
   stream 入力"
  (let ((monster-num (read stream)))
    (labels ((multi-gcd (current-monster-id acc)
               (if (< current-monster-id monster-num)
                   (multi-gcd (1+ current-monster-id) (gcd (read stream) acc))
                   acc)))
      (multi-gcd 1 (gcd (read stream))))))

(defun solve (stream)
  "問題を解く (真面目にgcd実装したバージョン)
   stream 入力"
  (let ((monster-num (read stream)))
    (labels ((my-gcd (a b)
               (let ((remainder (mod a b)))
                 (if (= remainder 0) b (my-gcd b remainder))))
             (multi-gcd (current-monster-id acc)
               (if (< current-monster-id monster-num)
                   (multi-gcd (1+ current-monster-id) (my-gcd (read stream) acc))
                   acc)))
      (multi-gcd 2 (my-gcd (read stream) (read stream))))))
;}}}


;;; -*- テスト -*-{{{

;;;; テストデータ
;(defparameter *test-data-dir*
;  (sb-ext:native-pathname
;    (format nil "~A~A"
;            (sb-posix:getcwd)
;            "/testdata/004_AtCoder_abc116_c_GrandGarden/input/")))
;(defparameter *test-data-list*
;  '("sample01.txt"
;    "sample02.txt"
;    "sample03.txt"
;    "test10.txt"
;    "test11.txt"
;    "test12.txt"
;    "test13.txt"
;    "test14.txt"
;    "random01.txt"
;    "random02.txt"
;    "random03.txt"
;    "random04.txt"))
;
;;;; 期待値データ
;(defparameter *expected-data-dir*
;  (sb-ext:native-pathname
;    (format nil "~A~A"
;            (sb-posix:getcwd)
;            "/testdata/004_AtCoder_abc116_c_GrandGarden/output/")))
;(defparameter *expected-data-list*
;  '("out_sample01.txt"
;    "out_sample02.txt"
;    "out_sample03.txt"
;    "out_test10.txt"
;    "out_test11.txt"
;    "out_test12.txt"
;    "out_test13.txt"
;    "out_test14.txt"
;    "out_random01.txt"
;    "out_random02.txt"
;    "out_random03.txt"
;    "out_random04.txt"))
;
;
;(defun test-solve (test-data-path expected-data-path)
;  "テスト
;   test-data-path テストデータのパス
;  expected-data-path 期待値データのパス"
;  (with-open-file (in-test test-data-path)
;    (with-open-file (in-expected expected-data-path)
;      (let ((actual (solve in-test))
;            (expected (read in-expected)))
;        (princ "  actual   ") (princ actual) (princ #\newline)
;        (princ "  expected ") (princ expected) (princ #\newline)
;        (if (eql actual expected)
;            t
;            nil)))))
;
;
;;;; 全テストデータに対してテスト実施
;(if (every (lambda (data)
;             (let ((test-data (first data))
;                   (expected-data (second data)))
;               (princ "test data:     ") (princ test-data) (princ #\newline)
;               (princ "expected data: ") (princ expected-data) (princ #\newline)
;               (test-solve
;                 (merge-pathnames *test-data-dir* test-data)
;                 (merge-pathnames *expected-data-dir* expected-data))))
;           (apply #'mapcar #'list (list *test-data-list* *expected-data-list*)))
;    (princ "OK")
;    (princ "NG"))

;}}}
