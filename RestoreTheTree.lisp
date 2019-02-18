;;; D - Restore the Tree

;;; -*- 問題文 -*-
;;; N 頂点の根付き木があり、その頂点には 1 から N までの番号が振られています。
;;; 根以外の各頂点には、その親から一本の有向辺が伸びています。
;;; なお、根は頂点 1 とは限りません。

;;; 高橋くんは、このグラフに M 本の新たな有向辺を書き加えました。
;;; 書き足された各辺 u -> v は、ある頂点 u からその子孫であるような頂点 v に向かって伸びています。

;;; 高橋くんが辺を書き加えたあとの N 頂点、 N ~ 1+M 辺の有向グラフが与えられます。
;;; より具体的には、N ~ 1+M 組の整数のペア (A1, B1), ..., (A_{N ~ 1+M}, B_{N ~ 1+M}) が与えられ、これらは i 番目の辺が頂点 Ai から頂点 Biに向かって伸びていることを表します。

;;; 元の根付き木を復元してください。

;;; -*- 制約 -*-
;;; 3 <= N
;;; 1 <= M
;;; N + M <= 10^5
;;; 1 <= A_i, B_i <= N
;;; A_i != B_i
;;; i != j ならば (A_i, B_i) != (A_j, B_j)
;;; 入力されるグラフは、N 頂点の根付き木に問題文中の条件を満たす M 本の辺を書き足すことで得られる。

;;; -*- 入力 -*-
;;; 入力は以下の形式で標準入力から与えられる。
;;; N          M
;;; A1         B1
;;; :
;;; A_{N-1+M}    B_{N-1+M}

;;; -*- 出力 -*-
;;; N行出力せよ。
;;; i 行目には、頂点 i が元の木の根であれば 0 を出力し、そうでなければ元の木で頂点 iの親を表す整数を出力すること。
;;; なお、元の木は一意に定まることが示せる。


;;; -*- 本体 -*-{{{
(defun solve-required-mass-stack-ver (stream);{{{
  "問題を解く
   stream 入力"
  (let* ((node-number (read stream))  ; ノード数
         (fake-link-number (read stream))  ; 追加されたリンク数
         (children-nodes-array (make-array node-number :initial-element nil))  ; [ノードid] => '(子ノードid1 子ノードid2 ...)
         (parent-node-array (make-array node-number :initial-element '(0 . 0))))  ; [ノードid] => (親ノードid . 深さ)
    (labels ((read-inputs (&optional (current-line 1))
               (if (<= current-line (+ (1- node-number) fake-link-number))
                   (let ((parent (read stream))
                         (child  (read stream)))
                     ;; 親-子供達Arrayを初期化
                     (setf (elt children-nodes-array (1- parent)) (cons child (elt children-nodes-array (1- parent))))
                     ;; 誰かの子供であることが確定したノードはとりあえず親をnilに設定
                     (setf (elt parent-node-array (1- child)) (cons nil 0))
                     ;todo
                     ;(princ "children-nodes-array") (princ children-nodes-array) (princ #\newline) (princ #\newline)
                     ;(princ "parent-node-array") (princ parent-node-array) (princ #\newline) (princ #\newline)
                     (read-inputs (1+ current-line)))))
             (find-root (arr)
               (1+ (position-if (lambda (x)
                                  (eql (car x) 0))
                                arr)))
             (solve-core (root)
               (labels ((traverse (current-node parent-node depth)
                          ;; 既に同じ親が登録されていたら終了
                          (if (eql parent-node (elt parent-node-array (1- current-node)))
                              (return-from solve-core))
                          ;; 現在の深さが、前に親ノードを登録した時よりも深い場合、親ノードを更新する
                          (let ((current-parent (elt parent-node-array (1- current-node))))
                            (if (> depth (cdr current-parent))
                                (setf (elt parent-node-array (1- current-node)) (cons parent-node depth))))
                          ;todo あとで消す
                          ;(princ "  parent-node-array (af)") (princ parent-node-array) (princ #\newline)
                          ;;; 子供に対して再帰呼び出し
                          ;(princ "  next children :") (princ ch) (princ #\newline) (princ #\newline)
                          (mapc #'traverse
                                (elt children-nodes-array (1- current-node))
                                (make-list (length (elt children-nodes-array (1- current-node))) :initial-element current-node)
                                (make-list (length (elt children-nodes-array (1- current-node))) :initial-element (1+ depth)))
                          ))
                 (mapc #'traverse
                       (elt children-nodes-array (1- root))
                       (make-list (length (elt children-nodes-array (1- root))) :initial-element root)
                       (make-list (length (elt children-nodes-array (1- root))) :initial-element 1)))))
      (princ "    reading inputs ... ")
      (read-inputs)
      (princ "    done.")
      (princ #\newline)
      (princ "    solving ... ")
      (solve-core (find-root parent-node-array))
      (princ "    done.")
      (princ #\newline)
      (princ "    making list")
      (princ #\newline)
      (mapcar #'car (coerce parent-node-array 'list))
      )))
;}}}

(defun solve (stream)
  "問題を解く
   stream 入力"
  (let* ((node-number (read stream))  ; ノード数
         (fake-link-number (read stream))  ; 追加されたリンク数
         (children-nodes-array (make-array node-number :initial-element nil))  ; [ノードid] => '(子ノードid1 子ノードid2 ...)
         (parent-node-array (make-array node-number :initial-element 0)))  ; [ノードid] => 親ノードid
    (labels (
             ;; "親=>子"のペアを読み込む
             (read-inputs (&optional (current-line 1))
               (if (<= current-line (+ (1- node-number) fake-link-number))
                   (let ((parent (read stream))
                         (child  (read stream)))
                     (setf (elt children-nodes-array (1- parent)) (cons child (elt children-nodes-array (1- parent))))
                     ;; 誰かの子供であることが確定したノードはとりあえず親をnilに設定
                     (setf (elt parent-node-array (1- child)) nil)
                     (read-inputs (1+ current-line)))))
             ;; ルートを探索して返す
             (find-root ()
               (1+ (position-if (lambda (x)
                                  (equal x 0))
                                parent-node-array)))
             ;; 問題を解く
             (solve-core (root)
               (let ((current-prev-pair-list nil)  ; ((親 . ) ...) 多重度 "現在:子供=1:N"
                     (current-children-list nil))
                 (labels (
                          ;; 現在のノードを元に、その子供のペアのリストを作る
                          (make-parent-children-pair-list (children parent)
                            ; children 子供一覧(child1 child2 child3 ...)
                            ; parent 親 parent
                            ; ret 親と子供のペア一覧 ((parent . child1) (parent . child1) (parent . child1) ...)
                            (labels ((func (chren &optional (acc nil))
                                       ;;(princ "        chren ") (princ chren) (princ #\newline)
                                       ;;(princ "        acc   ") (princ acc) (princ #\newline)
                                       (let ((ch (car chren)))
                                         (if ch
                                             (func (cdr chren) (cons (cons parent ch) acc))
                                             acc))))
                              (func children)))
                          (set-all (current-prev-pair)
                            (let ((prev (car current-prev-pair))
                                  (current (cdr current-prev-pair)))
                              ;;(princ "      [set-all]prev     ") (princ prev) (princ #\newline)
                              ;;(princ "      [set-all]current  ") (princ current) (princ #\newline)
                              (if (equal prev (elt parent-node-array (1- current)))
                                  ;; 既に探索したルートであるため登録しない
                                  nil
                                  (progn
                                    ;; 親配列を更新
                                    (setf (elt parent-node-array (1- current)) prev)
                                    ;; 次の探索対象となる子供リストを更新
                                    (setf current-children-list
                                          (append (make-parent-children-pair-list (elt children-nodes-array (1- current)) current) current-children-list))
                                    ;;(princ "        current-children-list  ") (princ current-children-list) (princ #\newline)
                                    ))))
                          ;; 探索する
                          (traverse ()
                            ;;(princ "      parent-node-array (before)     ") (princ parent-node-array) (princ #\newline)
                            ;;(princ "      current-prev-pair-list(before) ") (princ current-prev-pair-list) (princ #\newline)
                            ;; 現在の子供リストを初期化
                            (setf current-children-list nil)
                            ;; todo 全ての現在ノードに対して、その親を登録する
                            (mapc #'set-all current-prev-pair-list)
                            ;;(princ "      parent-node-array (after)      ") (princ parent-node-array) (princ #\newline)
                            ;;(princ "      current-prev-pair-list(after)  ") (princ current-prev-pair-list) (princ #\newline)
                            ;;(princ #\newline)
                            (setf current-prev-pair-list current-children-list)

                            (if current-children-list
                                (traverse))))
                   ;; ルートの子供をセット
                   (setf current-prev-pair-list (make-parent-children-pair-list (elt children-nodes-array (1- root)) root))
                   ;;(princ "      [first] current-prev-pair-list: ")
                   ;;(princ current-prev-pair-list) (princ #\newline) (princ #\newline)
                   (traverse)))))
      (princ "    reading inputs ... ")
      (read-inputs)
      (princ "    done.")
      (princ #\newline)
      (princ "    root is ") (princ (find-root)) (princ #\newline)
      (princ "    solving ... ")
      (solve-core (find-root))
      (princ "    done.")
      (princ #\newline)
      (princ "    making list")
      (princ #\newline)
      (coerce parent-node-array 'list)
      )))
;}}}


;;; -*- テスト -*-{{{

;;; テストデータ{{{
(defparameter *test-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A"
            (sb-posix:getcwd)
            "/testdata/005_AtCoder_nikkei2019_qual_d_RestoreTheTree/input/")))
(defparameter *test-data-list*
  '(
    ;"a01" "a02" "b03" "b04" "b05" "b06" "b07" "b08" 
    "b09" "b10" "b11" "b12" "b13" "b14" "b15" "b16" "b17" "b18" "b19" "b20" "b21" "b22" "b23" "b24" "b25" "b26" "b27" "b28" "b29" "b30" "b31" "b32" "b33" "b34"
    ))
;}}}

;;; 期待値データ{{{
(defparameter *expected-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A"
            (sb-posix:getcwd)
            "/testdata/005_AtCoder_nikkei2019_qual_d_RestoreTheTree/output/")))
(defparameter *expected-data-list*
  '(
    ;"a01" "a02" "b03" "b04" "b05" "b06" "b07" "b08" 
    "b09" "b10" "b11" "b12" "b13" "b14" "b15" "b16" "b17" "b18" "b19" "b20" "b21" "b22" "b23" "b24" "b25" "b26" "b27" "b28" "b29" "b30" "b31" "b32" "b33" "b34"
    ))
;}}}

(defun test-solve (test-data-path expected-data-path);{{{
  "テスト
   test-data-path テストデータのパス
  expected-data-path 期待値データのパス"
  (with-open-file (in-test test-data-path)
    (princ "  using ") (princ test-data-path) (princ #\newline)
    (with-open-file (in-expected expected-data-path)
      (princ "  using ") (princ expected-data-path) (princ #\newline)
      (labels ((make-result-list (stream)
                 (let ((parent-reversed nil))
                   (loop
                     for s = (read-line stream nil)
                     while s
                     do (setf parent-reversed (cons (parse-integer s) parent-reversed)))
                   (reverse parent-reversed))))
        (let ((actual (solve in-test))
              (expected (make-result-list in-expected)))
          (princ "equal actual expected: ") (princ (equal actual expected)) (princ #\newline)
          (equal actual expected))))))
;}}}

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

