;;;; D - �꓁���f

; -*- ��蕶 -*-{{{
;��������͒b�B�̌��ʁA���`���b�v�Ŗ؂̔�ؒf�ł���悤�ɂȂ�܂����B���`���b�v�̋O����\�������Ɣ̌`��\�����p�`���^������̂ŁA�������ɐؒf���ꂽ�����߂Ă��������B
;
;## ����
;���͈͂ȉ��̌`���ŕW�����͂���^������B
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
;1�s�ڂɂ́A�����̒[�_�̍��W Ax,Ay,Bx,By ���X�y�[�X��؂�ŗ^������B
;2�s�ڂɂ́A���p�`�̒��_�� N(3��N��100)���^������B
;3�s�ڂ���� N �s�ł́A�e���_�̍��W Xi,Yi���X�y�[�X��؂�ŗ^������B
;���͂ŗ^��������W��-1000�ȏ�1000�ȉ��̐����ł���B
;
;���͂ŗ^����������Ƒ��p�`�͈ȉ��̐����𖞂����B
;- ���p�`�̒��_�͔����v���̏��ŗ^������B
;- ���p�`�̒��_�͐�������0.1�ȏ㗣��Ă���B
;- �����̒[�_�͑��p�`����0.1�ȏ㗣��Ă���B
;- �����̒[�_�͑��p�`�̊O���ɂ���B
;- ���p�`�̘A������R���_���꒼����ɕ��Ԃ��Ƃ͂Ȃ��B
;
;���Ȃ킿�A�ȉ��̂悤�ȓ��͂͗^�����Ȃ��B
;- A,B:���p�`�̒��_��������ɂ���B
;- C:�����̒[�_�����p�`�̕ӏ�ɂ���B
;- D:���p�`�̕ӂƐ������d�Ȃ�B
;- E:�����̒[�_�����p�`�̓����ɂ���B
;
;## �o��
;�������ɐؒf����邩���o�͂���B�o�̖͂����ɂ͉��s�����邱�ƁB
;
;## ���͗�1
;```
;-2 0 2 0
;4
;1 1
;-1 1
;-1 -1
;1 -1
;```
;
;## �o�͗�1
;```
;2
;```
;
;## ���͗�2
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
;## �o�͗�2
;```
;3
;```
;;}}}

;;; -*- �{�� -*-{{{
(defun solve (stream)
  "��������"
  (defparameter *in-stream* stream)
  (defparameter *demarcation* (get-segment (read-point) (read-point)))
  (defparameter *vertex-num* (read *in-stream*))
  (let ((first-point (read-point)))
    (walk-along 2 (get-segment first-point (read-point)) 0 first-point)))

(defun walk-along (index segment acc &optional (first-point nil))
  "�̈�̒��_���n�_(1��)���珄��
   index ���݂̒��_�̔ԍ�
   segment ���݂̒��_���I�_�Ƃ������
   acc ���f�̈搔�̌v�Z�p"
  (if (intersect-segments-p *demarcation* segment) (incf acc))
  (cond ((< index *vertex-num*) (walk-along (1+ index) (get-segment (cdr segment) (read-point)) acc first-point))
        ((= *vertex-num* index) (walk-along (1+ index) (get-segment (cdr segment) first-point) acc))
        ((> index *vertex-num*) (floor (+ (/ acc 2) 1)))))
;}}}

; -*- ���[�e�B���e�B -*-{{{
(defun get-segment (p1 p2)
  "�������擾����
   p1 �n�_(x1 . y1)
   p2 �I�_(x2 . y2)
   return ((x1 . y1) . (x2 . y2))"
  (cons p1 p2))

(defun read-point ()
  "*in-stream*����_�̍��W��ǂݍ���
   return (x . y)"
  (cons (read *in-stream*) (read *in-stream*)))
;}}}

;;; -*- �������� -*-{{{
(defun intersect-segments-p (segment1 segment2)
  "�������m�̌�������
   -----segment1----->
   -----segment2----->
   return t �������Ă��� | nil �������Ă��Ȃ�"
  (and (intersect-lines-p segment1 segment2)
       (intersect-lines-p segment2 segment1)))

(defun intersect-lines-p (line1 line2)
  "�������m�̌�������
   a -----line1-----> b
   c -----line2-----> d
   return t �������Ă��� | nil �������Ă��Ȃ�"
  (let* ((a (car line1)) (b (cdr line1)) (c (car line2)) (d (cdr line2))
         (sign-c (- (* (- (cdr a) (cdr b)) (- (car c) (car a)))
                    (* (- (car a) (car b)) (- (cdr c) (cdr a)))))
         (sign-d (- (* (- (cdr a) (cdr b)) (- (car d) (car a)))
                    (* (- (car a) (car b)) (- (cdr d) (cdr a))))))
    (< (* sign-c sign-d) 0)))
;}}}


;;; -*- �e�X�g -*-{{{

;;; �e�X�g�f�[�^
(defparameter *test-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A" (sb-posix:getcwd) "/testdata/input/")))
(defparameter *test-data-list* '("01.txt" "02.txt" "03.txt" "04.txt" "05.txt"))

;;; ���Ғl�f�[�^
(defparameter *expected-data-dir*
  (sb-ext:native-pathname
    (format nil "~A~A" (sb-posix:getcwd) "/testdata/output/")))
(defparameter *expected-data-list* '("01.txt" "02.txt" "03.txt" "04.txt" "05.txt"))


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

