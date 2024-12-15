(add-to-list 'load-path (expand-file-name "."))
(load "lib/utils.el")
(load "lib/json.el")

;; --------------------------------

(defvar label-id-max 0)

(defun get-label-id ()
  (setq label-id-max (1+ label-id-max))
  label-id-max)

(defun asm-prologue ()
  (puts "  push bp")
  (puts "  mov bp sp"))

(defun asm-epilogue ()
  (puts "  mov sp bp")
  (puts "  pop bp"))

(defun fn-arg-disp (fn-args lvar)
  (let ((i (seq-position fn-args lvar)))
    (+ i 2)))

(defun lvar-disp (lvars lvar)
  (let ((i (seq-position lvars lvar)))
    (- (+ i 1))))

;; --------------------------------

(defun gen-expr:add ()
  (puts "  pop reg_b")
  (puts "  pop reg_a")
  (puts "  add reg_a reg_b"))

(defun gen-expr:mult ()
  (puts "  pop reg_b")
  (puts "  pop reg_a")
  (puts "  mul reg_b"))

(defun gen-expr:eq-neq (name then-val else-val)
  (let ((label-id (get-label-id)))
    (puts "  pop reg_b")
    (puts "  pop reg_a")
    (puts "  cmp")
    (puts (format "  je then_%d" label-id))
    (puts (format "  mov reg_a %d" else-val))
    (puts (format "  jmp end_%s_%d" name label-id))
    (puts (format "label then_%d" label-id))
    (puts (format "  mov reg_a %d" then-val))
    (puts (format "label end_%s_%d" name label-id))))

(defun gen-expr:eq ()
  (gen-expr:eq-neq "eq" 1 0))

(defun gen-expr:neq ()
  (gen-expr:eq-neq "neq" 0 1))

(defun gen-expr:binary (fn-args lvars expr)
  (let ((op  (elt expr 0))
        (lhs (elt expr 1))
        (rhs (elt expr 2))
        )
    (gen-expr fn-args lvars lhs)
    (puts "  push reg_a")
    (gen-expr fn-args lvars rhs)
    (puts "  push reg_a")

    (cond
     ((string= op "+") (gen-expr:add))
     ((string= op "*") (gen-expr:mult))
     ((string= op "==") (gen-expr:eq))
     ((string= op "!=") (gen-expr:neq))
     (t (panic (format "unsupported operator (%s)" op))))))

(defun gen-expr (fn-args lvars expr)
  (cond
   ((integerp expr)
    (puts (format "  mov reg_a %d" expr)))
   ((listp expr)
    (gen-expr:binary fn-args lvars expr))
   ((stringp expr)
    (cond
     ((member expr lvars)
      (let ((disp (lvar-disp lvars expr)))
        (puts (format "  mov reg_a [bp:%d]" disp))))
     ((member expr fn-args)
      (let ((disp (fn-arg-disp fn-args expr)))
        (puts (format "  mov reg_a [bp:%d]" disp))))
     (t (panic "73"))))
   (t (panic (format "31 unsupported (%s)" (pp-to-string expr))))))

(defun gen-return (fn-args lvars stmt)
  (when (= (length stmt) 2)
    (let ((expr (elt stmt 1)))
      (gen-expr fn-args lvars expr)))
  (asm-epilogue)
  (puts "  ret"))

(defun gen-set:common (fn-args lvars dest expr)
  (gen-expr fn-args lvars expr)
  (cond
   ((member dest lvars)
    (let ((disp (lvar-disp lvars dest)))
      (puts (format "  mov [bp:%d] reg_a" disp))))
   (t
    (panic (format "146 (%s) (%s) (%s)" lvars dest expr)))))

(defun gen-set (fn-args lvars stmt)
  (let ((dest (elt stmt 1))
        (expr (elt stmt 2)))
    (gen-set:common fn-args lvars dest expr)))

(defun gen-funcall (fn-args lvars funcall)
  (let ((funcall-name (car funcall))
        (funcall-args (cdr funcall)))

    (u:each (reverse funcall-args)
            (lambda (arg)
              (gen-expr fn-args lvars arg)
              (puts "  push reg_a")))

    (gen-vmcomment:common (format "call  %s" funcall-name))
    (puts (format "  call %s" funcall-name))
    (puts (format "  add sp %d" (length funcall-args)))))

(defun gen-call (fn-args lvars stmt)
  (let ((funcall (elt stmt 1)))
    (gen-funcall fn-args lvars funcall)))

(defun gen-call-set (fn-args lvars stmt)
  (let ((var-name (elt stmt 1))
        (funcall  (elt stmt 2)))
    (gen-funcall fn-args lvars funcall)
    (let ((disp (lvar-disp lvars var-name)))
      (puts (format "  mov [bp:%d] reg_a" disp)))))

(defun gen-while (fn-args lvars stmt)
  (let* ((expr  (elt stmt 1)) 
         (stmts (elt stmt 2))
         (label-id (get-label-id))
         (label-begin (format "while_%d" label-id))
         (label-end (format "end_while_%d" label-id)))
    (puts (format "label %s" label-begin))
    (gen-expr fn-args lvars expr)
    (puts "  mov reg_b 0")
    (puts "  cmp")
    (puts (format "  je %s" label-end))
    (gen-stmts fn-args lvars stmts)
    (puts (format "  jmp %s" label-begin))
    (puts (format "label %s" label-end))))

(defun gen-case (fn-args lvars stmt)
  (let* ((label-id (get-label-id))
         (when-idx 0)
         (label-end (format "end_case_%d" label-id))
         (label-end-when-head (format "end_when_%d" label-id))
         (when-clauses (cdr stmt)))
    (u:each-with-index when-clauses
                       (lambda (when-clause when-idx)
                         (let ((expr (elt when-clause 1))
                               (stmts (cddr when-clause)))
                           (gen-expr fn-args lvars expr)
                           (puts "  mov reg_b 0")
                           (puts "  cmp")
                           (puts (format "  je %s_%d" label-end-when-head when-idx))

                           (gen-stmts fn-args lvars stmts)

                           (puts (format "  jmp %s" label-end))
                           (puts (format "label %s_%d" label-end-when-head when-idx)))))
    (puts (format "label %s" label-end))))

(defun gen-vmcomment:common (comment)
  (puts (format "  _cmt %s"
                ;; (string-replace " " "~" comment) ; Emacs >= 28
                (replace-regexp-in-string " " "~" comment))))

(defun gen-vmcomment (stmt)
  (let ((comment (elt stmt 1)))
    (gen-vmcomment:common comment)))

(defun gen-stmt (fn-args lvars stmt)
  (let ((head (car stmt)))
    (cond
     ((string= head "return"  ) (gen-return   fn-args lvars stmt))
     ((string= head "set"     ) (gen-set      fn-args lvars stmt))
     ((string= head "call"    ) (gen-call     fn-args lvars stmt))
     ((string= head "call_set") (gen-call-set fn-args lvars stmt))
     ((string= head "while"   ) (gen-while    fn-args lvars stmt))
     ((string= head "case"    ) (gen-case     fn-args lvars stmt))
     ((string= head "_cmt"    ) (gen-vmcomment stmt))
     (t (panic (format "81 unsupported (%s)" head))))))

(defun gen-stmts (fn-args lvars stmts)
  (u:each stmts
          (lambda (stmt)
            (gen-stmt fn-args lvars stmt))))

(defun gen-var (fn-args lvars stmt)
  (puts "  add sp -1")
  (when (= (length stmt) 3)
    (let ((var-name (elt stmt 1))
          (expr     (elt stmt 2)))
      (gen-set:common fn-args lvars var-name expr))))

(defun gen-func-def (fn-def)
  (let ((fn-name (elt fn-def 1))
        (fn-args (elt fn-def 2))
        (stmts   (elt fn-def 3))
        (lvars '()))
    (puts (format "label %s" fn-name))
    (asm-prologue)
    (u:each stmts
            (lambda (stmt)
              (cond ((string= "var" (car stmt))
                     (setq lvars (u:append-one lvars
                                               (elt stmt 1)))
                     (gen-var fn-args lvars stmt))
                    (t (gen-stmt fn-args lvars stmt)))))
    (asm-epilogue)
    (puts "  ret")))

(defun gen-top-stmts (top-stmts)
  (u:each top-stmts 'gen-func-def))

(defun gen-builtin-set-vram ()
  (puts "label set_vram")
  (asm-prologue)
  (puts "  set_vram [bp:2] [bp:3]")
  (asm-epilogue)
  (puts "  ret"))

(defun gen-builtin-get-vram ()
  (puts "label get_vram")
  (asm-prologue)
  (puts "  get_vram [bp:2] reg_a")
  (asm-epilogue)
  (puts "  ret"))

(defun codegen (ast)
  (puts "  call main")
  (puts "  exit")

  (let ((top-stmts (cdr ast)))
    (gen-top-stmts top-stmts))

  (puts "#>builtins")
  (gen-builtin-set-vram)
  (gen-builtin-get-vram)
  (puts "#<builtins"))

;; --------------------------------

(let* ((src (u:read-stdin-all))
       (ast (json:parse src)))
  (codegen ast))
