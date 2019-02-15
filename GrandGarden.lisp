;;; C - Grand Garden

;;; -*- 問題文 -*-
;;; 問題文
;;; 花壇にN本の花が咲いており、それぞれ 1, 2, ......, N と番号が振られています。
;;; 最初、全ての花の高さは 0 です。
;;; 数列 h={h1, h2, h3, ......} が入力として与えられます。
;;; 以下の「水やり」操作を繰り返すことで、すべての k(1≦k≦N) に対して花kの高さをh_kにしたいです。
;;;   - 整数 l,rを指定する。l≦x≦r を満たすすべてのxに対して、花 x の高さを1高くする。
;;; 条件を満たすための最小の「水やり」操作の回数を求めてください。

;;; 制約
;;; - 1≦N≦100
;;; - 0≦hi≦100
;;; - 入力はすべて整数である。

;;; 入力
;;; 入力は以下の形式で標準入力から与えられます。
;;;
;;; ```
;;; N
;;; h_1 h_2 h_3 ...... h_N
;;; ```

;;; 出力
;;; 条件を満たすような最小の「水やり」操作の回数を出力してください。

;;; ---------------------------------------------
;;; 本体

(defun solve (stream)
  "問題を解く
   stream 入力"
  (let ((flower-heights (make-array (read stream))))
    (labels ((read-heights (size &optional (pos 0))
               (if (< pos size)
                   (progn (setf (elt flower-heights pos) (read stream))
                          (read-heights size (1+ pos))))))
      (read-heights (length flower-heights)))
    (labels ((count-watering ()
               ))
      )))


;;; ---------------------------------------------
;;; テスト

;;; テストデータのパス
(defparameter *test-data-dir* (sb-ext:native-pathname (format nil "~A~A" (sb-posix:getcwd) "/testdata/004_AtCoder_abc116_c_GrandGarden/input/")))
(defparameter *test-data-list* '("sample01.txt"
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

;;; 期待値データのパス
(defparameter *expected-data-dir* (sb-ext:native-pathname (format nil "~A~A" (sb-posix:getcwd) "/testdata/004_AtCoder_abc116_c_GrandGarden/output/")))
(defparameter *expected-data-list* '("out_sample01.txt"
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
  (with-open-file (*in-test* test-data-path)
    (with-open-file (*in-expected* expected-data-path)
      (let ((expected (read *in-expected*))
            (actual (read *in-test*)))
        (if (eql expected actual)
            t
            nil)))))


;;; 全テストデータに対してテスト
(if (member nil
            (mapcar (lambda (test-data expected-data)
                      (test-solve
                        (merge-pathnames *test-data-dir* test-data)
                        (merge-pathnames *expected-data-dir* expected-data)))
                    *test-data-list*
                    *expected-data-list*))
    (princ "NG")
    (princ "OK"))

