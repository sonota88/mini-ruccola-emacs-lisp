(require 'subr-x) ; string-trim

(defun u:read-stdin-all ()
  (with-temp-buffer
    (insert-file-contents "/dev/stdin")
    (buffer-string)))

(defun u:read-file-all (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun puts (arg)
  (princ arg)
  (princ "\n"))

(defun princ-e (arg)
  (princ arg #'external-debugging-output))

(defun prin1-e (arg)
  (prin1 arg #'external-debugging-output))

(defun u:start-with-p (str head)
  (if (< (length head) (length str))
      (let ((size (length head)))
        (string= (substring str 0 size) head))
    nil))

(defun panic (&optional msg)
  (when msg
    (message (concat "PANIC: " msg)))
  (throw 'panic t))

(defun u:string-trim (str)
  (string-trim str) ; subr-x
  )

(defun u:each-with-index (xs fn)
  (let ((i 0)
        (size (length xs)))
    (while (< i size)
      (let ((it (elt xs i)))
        (funcall fn it i))
      (setq i (1+ i))))
  nil)

(defun u:each (xs fn2)
  (let ((i 0)
        (size (length xs)))
    (while (< i size)
      (let ((it (elt xs i)))
        (funcall fn2 it))
      (setq i (1+ i))))
  nil)

(defun u:append-one (xs x)
  (append xs (list x)))

(defun u:alist-get (key alist)
  (cdr (assoc key alist)))
