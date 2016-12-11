(defun buttons-part1 ()
  (list (list (list -1 1) 1)
   (list (list 0 1) 2)
   (list (list 1 1) 3)
   (list (list -1 0) 4)
   (list (list 0 0) 5)
   (list (list 1 0) 6)
   (list (list -1 -1) 7)
   (list (list 0 -1) 8)
   (list (list 1 -1) 9)
   ))

(defun buttons-part2 ()
  (list (list (list 0 2) 1)
   (list (list -1 1) 2)
   (list (list 0 1) 3)
   (list (list 1 1) 4)
   (list (list -2 0) 5)
   (list (list -1 0) 6)
   (list (list 0 0) 7)
   (list (list 1 0) 8)
   (list (list 2 0) 9)
   (list (list -1 -1) 'A)
   (list (list 0 -1) 'B)
   (list (list 1 -1) 'C)
   (list (list 0 -2) 'D)
   ))

(defun move (buttons position delta)
  (if (get-button-number buttons (mapcar '+ position delta))
    (mapcar '+ position delta)
    position))

(defun get-delta (letter)
  (cond
    ((string= letter "U") (list 0 1))
    ((string= letter "R") (list 1 0))
    ((string= letter "D") (list 0 -1))
    ((string= letter "L") (list -1 0))))

(defun get-position (buttons moves position)
  (if (= (length moves) 0)
    position
    (get-position buttons (rest moves)
                  (move buttons position (get-delta (first moves))))))

(defun check-position (position x y)
  (if (and (= (first position) x)
           (= (second position) y))
    't
    nil))

(defun get-button-number (buttons position)
  (second (find position buttons :test
                #'(lambda (a b) (equal a (first b))))))

(defun get-number (buttons moves x y)
  (get-position buttons moves (list x y)))

(defun get-code-internal (buttons x y)
  (format t "Instructions starting from (~a,~a) [Q to exit]: " x y)
  (let ((input (coerce (symbol-name (read)) 'list)))
    (if (string= (first input) "Q")
      nil
      (let ((result (get-number buttons input x y)))
        (format t "Landed to ~a~%~%" (get-button-number buttons result))
        (get-code-internal buttons (first result) (second result)))
      )))

(defun get-code (part-buttons)
  (let ((x (first (first (find 5 part-buttons :test
                               #'(lambda (a b) (equal a (second b)))))))
        (y (second (first (find 5 part-buttons :test
                                #'(lambda (a b) (equal a (second b))))))))
    (get-code-internal part-buttons x y)))

;(get-code)
