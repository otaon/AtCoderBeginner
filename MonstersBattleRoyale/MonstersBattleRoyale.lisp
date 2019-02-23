;;;; C - Grand Garden

; -*- ��蕶 -*-{{{
;N�̂̃����X�^�[�����āA���ꂼ�� 1, 2, ..., N�Ɣԍ��t�����Ă��܂��B
;
;�͂��߁A�����X�^�[ i�̗̑͂� A_i�ł��B
;�ȍ~�A�̗͂�1�ȏ�̃����X�^�[�𐶂��Ă��郂���X�^�[�ƌĂт܂��B
;�����Ă��郂���X�^�[��1�̂ɂȂ�܂ňȉ����J��Ԃ��܂��B
;- �����_����1�̂̐����Ă��郂���X�^�[�������_���ɕʂ̐����Ă��郂���X�^�[�ɍU�����܂��B
;- ���̌��ʁA�U�����ꂽ�����X�^�[�̗̑͂��U�����������X�^�[�̗̑͂Ɠ����l�������炵�܂��B
;
;�Ō�ɐ����c���������X�^�[�̍ŏI�I�ȑ̗͂̍ŏ��l�����߂Ă��������B
;
;����
;* ���͂͑S�Đ����ł���B
;* 2 <= N_i <= 10^5
;* 1 <= A_i <= 10^9
;
;����
;���͈͂ȉ��̌`���ŕW�����͂���^������B
;N
;A_1 A_2 ... A_N
;
;�o��
;�Ō�ɐ����c���������X�^�[�̍ŏI�I�ȑ̗͂̍ŏ��l���o�͂���B

;}}}


;;; -*- �{�� -*-{{{

(defun solve-corner-cutting-version (stream)
  "��������(�蔲���o�[�W����)
   stream ����"
  (let ((monster-num (read stream)))
    (labels ((multi-gcd (current-monster-id acc)
               (if (< current-monster-id monster-num)
                   (multi-gcd (1+ current-monster-id) (gcd (read stream) acc))
                   acc)))
      (multi-gcd 1 (gcd (read stream))))))

(defun solve (stream)
  "�������� (�^�ʖڂ�gcd���������o�[�W����)
   stream ����"
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


;;; -*- �e�X�g -*-{{{

;;;; �e�X�g�f�[�^
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
;;;; ���Ғl�f�[�^
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
;  "�e�X�g
;   test-data-path �e�X�g�f�[�^�̃p�X
;  expected-data-path ���Ғl�f�[�^�̃p�X"
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
;;;; �S�e�X�g�f�[�^�ɑ΂��ăe�X�g���{
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
