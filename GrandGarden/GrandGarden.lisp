;;;; C - Grand Garden

; -*- ��蕶 -*-{{{

; ��蕶
; �Ԓd��N�{�̉Ԃ��炢�Ă���A���ꂼ�� 1, 2, ......, N �Ɣԍ����U���Ă��܂��B
; �ŏ��A�S�ẲԂ̍����� 0 �ł��B
; ���� h={h1, h2, h3, ......} �����͂Ƃ��ė^�����܂��B
; �ȉ��́u�����v������J��Ԃ����ƂŁA���ׂĂ� k(1��k��N) �ɑ΂��ĉ�k�̍�����h_k�ɂ������ł��B
;   - ���� l,r���w�肷��Bl��x��r �𖞂������ׂĂ�x�ɑ΂��āA�� x �̍�����1��������B
; �����𖞂������߂̍ŏ��́u�����v����̉񐔂����߂Ă��������B

; ����
; - 1��N��100
; - 0��hi��100
; - ���͂͂��ׂĐ����ł���B

; ����
; ���͈͂ȉ��̌`���ŕW�����͂���^�����܂��B
;
; ```
; N
; h_1 h_2 h_3 ...... h_N
; ```

; �o��
; �����𖞂����悤�ȍŏ��́u�����v����̉񐔂��o�͂��Ă��������B

;}}}


;;; -*- �{�� -*-{{{

(defun solve (stream)
  "��������
   stream ����"
  (let ((flower-num (read stream)))
    (labels ((count-watering (waterings current height &optional (last-height 0))
               (if (< height last-height)
                   (setf waterings (+ waterings (- last-height height))))
               (if (< current flower-num)
                   (count-watering waterings (1+ current) (read stream) height)
                   (+ waterings height))))
      (count-watering 0 1 (read stream)))))

;}}}


;;; -*- �e�X�g -*-{{{

;;; �e�X�g�f�[�^
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

;;; ���Ғl�f�[�^
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
  "�e�X�g
   test-data-path �e�X�g�f�[�^�̃p�X
  expected-data-path ���Ғl�f�[�^�̃p�X"
  (with-open-file (in-test test-data-path)
    (with-open-file (in-expected expected-data-path)
      (let ((actual (solve in-test))
            (expected (read in-expected)))
        (princ "  actual   ") (princ actual) (princ #\newline)
        (princ "  expected ") (princ expected) (princ #\newline)
        (if (eql actual expected)
            t
            nil)))))


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
