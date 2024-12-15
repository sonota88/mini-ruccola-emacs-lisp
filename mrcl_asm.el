(add-to-list 'load-path (expand-file-name "."))
(load "lib/utils.el")
(load "lib/json.el")

(defun to-asm-lines (src)
  (let ((lines (seq-remove
                (lambda (line)
                  (or (= 0 (length line))
                      (u:start-with-p (u:string-trim line) "#") ; comment
                      ))
                (split-string src "\n"))))
    (setq asm-lines (mapcar (lambda (line)
                           (split-string
                            (u:string-trim line)
                            " "))
                         lines))))

(defun to-label-map (asm-lines)
  (let ((label-map '()))
    (u:each-with-index
     asm-lines
     (lambda (asm-line addr)
       (when (string= (elt asm-line 0) "label")
         (let ((label (elt asm-line 1)))
           (setq label-map (cons (cons label addr)
                                 label-map))))))
    label-map))

(defun to-machine-code-operand (arg)
  (let ((matched))
    (cond
     ;; /^\[(.+)\]$/ => "mem:#{$1}"
     ((string-match "\\`\\[\\(.+\\)\\]\\'" arg)
      (setq matched (match-string 1 arg))
      (format "mem:%s" matched))
     ;; /^-?[0-9]+$/ => arg.to_i / 手抜き
     ((string-match "\\`\\(-?[0-9]+\\)\\'" arg)
      (setq matched (match-string 1 arg))
      (string-to-number matched))
     (t arg))))

(defun to-insn (asm-line)
  (let ((head (car asm-line))
        (rest (cdr asm-line)))
    (cond ((string= head "label")
           asm-line)
          ((or
            (string= head "jmp")
            (string= head "je")
            (string= head "call"))
           (let ((label-name (car rest)))
             (let ((addr (u:alist-get label-name label-map)))
               (if addr
                   (list head addr)
                 (panic (format "no such label (%s)" label-name))))))
          (t
           (cons head (mapcar 'to-machine-code-operand rest))))))

;; --------------------------------

(let ((src (u:read-stdin-all))
      (label-map))
  (let ((asm-lines (to-asm-lines src)))
  (setq label-map (to-label-map asm-lines))
  (u:each asm-lines
          (lambda (asm-line)
            (let ((insn (to-insn asm-line)))
              (json:print insn)
              (princ "\n"))))))
