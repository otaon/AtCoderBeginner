;;; D - Restore the Tree

;;; -*- ��蕶 -*-
;;; N ���_�̍��t���؂�����A���̒��_�ɂ� 1 ���� N �܂ł̔ԍ����U���Ă��܂��B
;;; ���ȊO�̊e���_�ɂ́A���̐e�����{�̗L���ӂ��L�тĂ��܂��B
;;; �Ȃ��A���͒��_ 1 �Ƃ͌���܂���B

;;; ��������́A���̃O���t�� M �{�̐V���ȗL���ӂ����������܂����B
;;; ���������ꂽ�e�� u -> v �́A���钸�_ u ���炻�̎q���ł���悤�Ȓ��_ v �Ɍ������ĐL�тĂ��܂��B

;;; �������񂪕ӂ��������������Ƃ� N ���_�A N ~ 1+M �ӂ̗L���O���t���^�����܂��B
;;; ����̓I�ɂ́AN ~ 1+M �g�̐����̃y�A (A1, B1), ..., (A_{N ~ 1+M}, B_{N ~ 1+M}) ���^�����A������ i �Ԗڂ̕ӂ����_ Ai ���璸�_ Bi�Ɍ������ĐL�тĂ��邱�Ƃ�\���܂��B

;;; ���̍��t���؂𕜌����Ă��������B

;;; -*- ���� -*-
;;; 3 <= N
;;; 1 <= M
;;; N + M <= 10^5
;;; 1 <= A_i, B_i <= N
;;; A_i != B_i
;;; i != j �Ȃ�� (A_i, B_i) != (A_j, B_j)
;;; ���͂����O���t�́AN ���_�̍��t���؂ɖ�蕶���̏����𖞂��� M �{�̕ӂ������������Ƃœ�����B

;;; -*- ���� -*-
;;; ���͈͂ȉ��̌`���ŕW�����͂���^������B
;;; N          M
;;; A1         B1
;;; :
;;; A_{N-1+M}    B_{N-1+M}

;;; -*- �o�� -*-
;;; N�s�o�͂���B
;;; i �s�ڂɂ́A���_ i �����̖؂̍��ł���� 0 ���o�͂��A�����łȂ���Ό��̖؂Œ��_ i�̐e��\���������o�͂��邱�ƁB
;;; �Ȃ��A���̖؂͈�ӂɒ�܂邱�Ƃ�������B


;;; -*- �{�� -*-{{{
(defun solve (stream)
  "��������
   stream ����"
  (let* ((node-number (read stream))  ; �m�[�h��
         (fake-link-number (read stream))  ; �ǉ����ꂽ�����N��
         (children-nodes-array (make-array node-number :initial-element nil))  ; [�m�[�hid] => '(�q�m�[�hid1 �q�m�[�hid2 ...)
         (parent-node-array (make-array node-number :initial-element '(0 . 0))))  ; [�m�[�hid] => (�e�m�[�hid . �[��)
    (labels ((read-inputs (&optional (current-line 1))
               (if (<= current-line (+ (1- node-number) fake-link-number))
                   (let ((parent (read stream))
                         (child  (read stream)))
                     ;; �e-�q���BArray��������
                     (setf (elt children-nodes-array (1- parent)) (cons child (elt children-nodes-array (1- parent))))
                     ;; �N���̎q���ł��邱�Ƃ��m�肵���m�[�h�͂Ƃ肠�����e��nil�ɐݒ�
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
                          ;; ���ɓ����e���o�^����Ă�����I��
                          (if (eql parent-node (elt parent-node-array (1- current-node)))
                              (return-from solve-core))
                          ;; ���݂̐[�����A�O�ɐe�m�[�h��o�^�����������[���ꍇ�A�e�m�[�h���X�V����
                          (let ((current-parent (elt parent-node-array (1- current-node))))
                            (if (> depth (cdr current-parent))
                                (setf (elt parent-node-array (1- current-node)) (cons parent-node depth))))
                          ;todo ���Ƃŏ���
                          ;(princ "  parent-node-array (af)") (princ parent-node-array) (princ #\newline)
                          ;;; �q���ɑ΂��čċA�Ăяo��
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


;;; -*- �e�X�g -*-{{{

;;; �e�X�g�f�[�^{{{
(defparameter *test-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A"
            (sb-posix:getcwd)
            "/testdata/005_AtCoder_nikkei2019_qual_d_RestoreTheTree/input/")))
(defparameter *test-data-list*
  '("a01" "a02" "b03" "b04" "b05" "b06" ;"b07" "b08" "b09" "b10" "b11" "b12" "b13" "b14" "b15" "b16" "b17" "b18" "b19" "b20" "b21" "b22" "b23" "b24" "b25" "b26" "b27" "b28" "b29" "b30" "b31" "b32" "b33" "b34"
    ))
;}}}

;;; ���Ғl�f�[�^{{{
(defparameter *expected-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A"
            (sb-posix:getcwd)
            "/testdata/005_AtCoder_nikkei2019_qual_d_RestoreTheTree/output/")))
(defparameter *expected-data-list*
  '("a01" "a02" "b03" "b04" "b05" "b06" ;"b07" "b08" "b09" "b10" "b11" "b12" "b13" "b14" "b15" "b16" "b17" "b18" "b19" "b20" "b21" "b22" "b23" "b24" "b25" "b26" "b27" "b28" "b29" "b30" "b31" "b32" "b33" "b34"
    ))
;}}}

(defun test-solve (test-data-path expected-data-path)
  "�e�X�g
   test-data-path �e�X�g�f�[�^�̃p�X
  expected-data-path ���Ғl�f�[�^�̃p�X"
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
          ;        (princ "  actual   ") (prin1 actual) (princ #\newline)
          ;        (princ "  expected ") (prin1 expected) (princ #\newline)
          (princ "equal actual expected: ") (princ (equal actual expected)) (princ #\newline)
          (equal actual expected)))
      (princ "  close file done") (princ #\newline))
    (princ "  close file done") (princ #\newline) (princ #\newline)))

;;; �S�e�X�g�f�[�^�ɑ΂��ăe�X�g���{
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

