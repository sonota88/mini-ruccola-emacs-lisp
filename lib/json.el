(defun json:to-list (xs)
  (mapcar (lambda (x)
            (cond ((stringp x) x)
                  ((arrayp  x) (json:to-list x))
                  (t x)))
          xs))

(defun json:parse (json)
  (json:to-list
   (json-parse-string json)))

(defun print-indent (lv)
  (let ((i 0))
    (while (< i lv)
      (princ "  ")
      (setq i (1+ i)))))

(defun json-print-node (node lv pretty)
  (when pretty (print-indent lv))
  (cond ((integerp node) (princ node))
        ((stringp  node) (princ (format "\"%s\"" node)))
        ((listp    node) (json:print-list node lv pretty))
        (t (panic "unsupported"))))

(defun json:print-list (xs lv pretty)
  (princ "[")
  (when pretty (princ "\n"))
  (u:each-with-index
   xs
   (lambda (it i)
     (json-print-node it (1+ lv) pretty)
     (when (< i (- (length xs) 1))
       (princ ",")
       (unless pretty (princ " ")))
     (when pretty (princ "\n"))))
  (when pretty (print-indent lv))
  (princ "]"))

(defun json:print (xs)
  (json:print-list xs 0 nil))

(defun json:prettyprint (xs)
  (json:print-list xs 0 t))
