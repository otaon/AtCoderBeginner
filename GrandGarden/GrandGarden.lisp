;;;; C - Grand Garden

; -*- 問題文 -*-{{{

; 問題文
; 花壇にN本の花が咲いており、それぞれ 1, 2, ......, N と番号が振られています。
; 最初、全ての花の高さは 0 です。
; 数列 h={h1, h2, h3, ......} が入力として与えられます。
; 以下の「水やり」操作を繰り返すことで、すべての k(1≦k≦N) に対して花kの高さをh_kにしたいです。
;   - 整数 l,rを指定する。l≦x≦r を満たすすべてのxに対して、花 x の高さを1高くする。
; 条件を満たすための最小の「水やり」操作の回数を求めてください。

; 制約
; - 1≦N≦100
; - 0≦hi≦100
; - 入力はすべて整数である。

; 入力
; 入力は以下の形式で標準入力から与えられます。
;
; ```
; N
; h_1 h_2 h_3 ...... h_N
; ```

; 出力
; 条件を満たすような最小の「水やり」操作の回数を出力してください。

;}}}


;;; -*- 本体 -*-{{{

(defun solve (stream)
  "問題を解く
   stream 入力"
  (let ((flower-num (read stream)))
    (labels ((count-watering (waterings current height &optional (last-height 0))
               (if (< height last-height)
                   (setf waterings (+ waterings (- last-height height))))
               (if (< current flower-num)
                   (count-watering waterings (1+ current) (read stream) height)
                   (+ waterings height))))
      (count-watering 0 1 (read stream)))))

;}}}


;;; -*- テスト -*-{{{

;;; テストデータ
(defparameter *test-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A"
            (sb-posix:getcwd)
            "/testdata/input/")))
(defparameter *test-data-list*
  '("sample01.txt"
    "sample02.txt"
    "sample03.txt"
    "test10.txt"
    "test11.txt"
    "test12.txt"
    "test13.txt"
    "test14.txt"
    "random01.txt"
    "random02.txt"
    "random03.txt"
    "random04.txt"))

;;; 期待値データ
(defparameter *expected-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A"
            (sb-posix:getcwd)
            "/testdata/output/")))
(defparameter *expected-data-list*
  '("out_sample01.txt"
    "out_sample02.txt"
    "out_sample03.txt"
    "out_test10.txt"
    "out_test11.txt"
    "out_test12.txt"
    "out_test13.txt"
    "out_test14.txt"
    "out_random01.txt"
    "out_random02.txt"
    "out_random03.txt"
    "out_random04.txt"))


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
