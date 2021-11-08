
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

(defun hello-world ()
  (format t "hola, mundo!"))

(hello-world)
(hello-world)

(defun where-is (x)
  (cond ((equal x 'paris) 'france)
        ((equal x 'london) 'england)
        ((equal x 'beijing) 'china)
        (t 'unknown)))
      

(where-is 'london)
(where-is 'beijing)

(defun small-positive-oddp (x)
  (and (< x 100)
       (> x 0)
       (oddp x)))

(small-positive-oddp 9)

(defun how-alike (a b)
  (cond ((equal a b) 'the-same)
        ((and (oddp a) (oddp b)) 'both-odd)
        ((and (not (oddp a)) (not (oddp b))) 'both-even)
        ((and (< a 0) (< b 0)) 'both-negative)
        (t 'not-alike)))

(how-alike 7 7)
(how-alike 3 5)
(how-alike -2 -3)
(how-alike 5 8)

(defun where-is-3 (x)
  (or (and (equal x 'paris) 'france)
      (and (equal x 'london) 'england)
      (and (equal x 'beijing) 'china)
      'unknown))

(where-is-3 'paris)

(mapcar #'square '(1 2 3 4 5))
(find-if #'oddp '(2 4 5 7 12 3 13))

(setf my-words
      '((one un)
        (two deux)
        (three trois)
        (four quatre)
        (five cinq)))

(defun my-assoc (key table)
  (find-if #'(lambda (entry)
               (equal key (first entry)))
           table))

(mapcar #'second my-words)
(my-assoc 'two my-words)


(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun db-dump ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? (y/n): ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(select (where :rating 10 :ripped nil))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(update (where :artist "Dixie Chicks") :rating 11)

(defmacro backwards (expr) (reverse expr))
(backwards ("hello, world" t format))


(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
