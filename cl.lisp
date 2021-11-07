
(defun make-even (x)
  (if (oddp x)
      (+ x 1)
     x))
(make-even 6)

(defun further (x)
  (if (> x 0)
      (+ x 1)
      (- x 1)))
(further 1)
(further -2)
(further 0)

(defun my-not (x)
  (if (equal t x) nil t))

(my-not t)
(my-not nil)

(defun ordered (x y)
  (if (> x y)
      (list y x)
      (list x y)))

(ordered 7 6)
(ordered 1 6)
