;; Homework 5 - Recursive Descent Parser
;; Author: Fathaabdi24
;; This is the main parser file.
(defvar *input* nil)
(defvar *current* 0)

(defun peek ()
  (if (< *current* (length *input*))
      (char *input* *current*)
      nil))

(defun consume (expected)
  (if (char= (peek) expected)
      (progn
        (incf *current*)
        t)
      (error "Expected ~a but found ~a" expected (peek))))
(defun parse-I ()
  (consume #\i)
  (parse-E)
  (parse-S)
  (when (char= (peek) #\e)
    (consume #\e)
    (parse-S)))

(defun parse-E ()
  (parse-G)
  (parse-E-Prime))

(defun parse-E-Prime ()
  (when (char= (peek) #\o)
    (consume #\o)
    (parse-G)
    (parse-E-Prime)))

(defun parse-G ()
  (let ((c (peek)))
    (if (member c '(#\x #\y #\z #\w))
        (consume c)
        (error "Expected one of x, y, z, w but found ~a" c))))

(defun parse-S ()
  (let ((c (peek)))
    (cond
      ((char= c #\s) (consume #\s))
      ((char= c #\d)
       (consume #\d)
       (parse-L)
       (consume #\b))
      (t (error "Expected s or d but found ~a" c)))))

(defun parse-L ()
  (consume #\s)
  (parse-L-Prime))

(defun parse-L-Prime ()
  (when (char= (peek) #\s)
    (consume #\s)
    (parse-L-Prime)))

