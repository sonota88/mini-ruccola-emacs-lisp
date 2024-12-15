(add-to-list 'load-path (expand-file-name "."))
(load "lib/utils.el")
(load "lib/json.el")

(defvar tokens)
(defvar pos 0)

;; --------------------------------

(defun token:get-kind (token) (elt token 1))
(defun token:get-str  (token) (elt token 2))

;; --------------------------------

(defun pos:next ()
  (setq pos (1+ pos)))

(defun peek (&optional offset)
  (elt tokens (+ pos
                 (or offset 0))))

(defun peek-and-next ()
  (let ((token (elt tokens pos)))
    (pos:next)
    token))

(defun consume (str-exp)
  (let* ((token (peek))
         (str-act (token:get-str token)))
    (if (string= str-act
                 str-exp)
        (pos:next)
      (panic (format "unexpected token / exp (%s) act (%s)"
                     str-exp str-act)))))

;; --------------------------------

(defun parse-arg ()
  (let* ((token (peek))
         (tkind (token:get-kind token))
         (tstr (token:get-str token)))
    (cond
     ((string= tkind "int")
      (pos:next)
      (string-to-number tstr))
     ((string= tkind "ident")
      (pos:next)
      tstr)
     (t (panic (format "unsupported (%s)" tkind))))))

(defun parse-args ()
  (let ((args '()))
    (if (string= (token:get-str (peek))
                 ")")
        args ; no arguments
      (progn
        (setq args (list (parse-arg))) ; first argument

        (while (string= (token:get-str (peek))
                        ",")
          (consume ",")
          (setq args (u:append-one args
                                   (parse-arg))))
        args))))

;; --------------------------------

(defun parse-expr-term ()
  (let* ((token (peek))
         (kind (token:get-kind token)))
    (cond ((string= kind "int")
           (pos:next)
           (string-to-number (token:get-str token)))
          ((string= kind "ident")
           (pos:next)
           (token:get-str token))
          ((string= kind "sym")
           (let ((expr))
             (consume "(")
             (setq expr (parse-expr))
             (consume ")")
             expr))
          (t (panic (format "100 unsupported (%s)"
                            (pp-to-string (peek))))))))

(defun binop-p (token)
  (member (token:get-str token) '("+" "*" "==" "!=")))

(defun parse-expr ()
  (let ((expr)
        (op)
        (rhs))
    (setq expr (parse-expr-term))
    (while (binop-p (peek))
      (setq op (token:get-str (peek-and-next)))
      (setq rhs (parse-expr-term))
      (setq expr (list op expr rhs)))
    expr))

(defun parse-return ()
  (consume "return")
  (let ((token (peek)))
    (if (string= (token:get-str token) ";")
        (progn
          (consume ";")
          '("return"))
      (let ((expr (parse-expr)))
        (consume ";")
        (list "return" expr)))))

(defun parse-set ()
  (let ((dest)
        (expr))
    (consume "set")
    (setq dest (token:get-str (peek-and-next)))
    (consume "=")
    (setq expr (parse-expr))
    (consume ";")

    (list "set" dest expr)))

(defun parse-funcall ()
  (let ((fn-name)
        (args))
    (setq fn-name (token:get-str (peek-and-next)))
    (consume "(")
    (setq args (parse-args))
    (consume ")")
    (cons fn-name args)))

(defun parse-call ()
  (let ((funcall))
    (consume "call")
    (setq funcall (parse-funcall))
    (consume ";")
    (list "call" funcall)))

(defun parse-call-set ()
  (let ((var-name)
        (funcall))
    (consume "call_set")
    (setq var-name (token:get-str (peek-and-next)))
    (consume "=")
    (setq funcall (parse-funcall))
    (consume ";")
    (list "call_set" var-name funcall)))

(defun parse-while ()
  (let ((expr) (stmts))
    (consume "while")
    (consume "(")
    (setq expr (parse-expr))
    (consume ")")
    (consume "{")
    (setq stmts (parse-stmts))
    (consume "}")
    (list "while" expr stmts)))

(defun parse-case:when ()
  (let ((when-clause '("when")))
    (consume "when")
    (consume "(")
    (setq when-clause (u:append-one when-clause
                                    (parse-expr)))
    (consume ")")
    (consume "{")
    (setq when-clause (append when-clause
                              (parse-stmts)))
    (consume "}")
    when-clause))

(defun parse-case ()
  (let ((stmt '("case")))
    (consume "case")
    (while (string= (token:get-str (peek)) "when")
      (setq stmt (u:append-one stmt
                               (parse-case:when))))
    stmt))

(defun parse-vmcomment ()
  (let ((comment))
    (consume "_cmt")
    (consume "(")
    (setq comment (token:get-str (peek-and-next)))
    (consume ")")
    (consume ";")
    (list "_cmt" comment)))

;; TODO _debug

(defun parse-stmt ()
  (let ((tstr (token:get-str (peek))))
    (cond
     ((string= tstr "return") (parse-return))
     ((string= tstr "set") (parse-set))
     ((string= tstr "call") (parse-call))
     ((string= tstr "call_set") (parse-call-set))
     ((string= tstr "while") (parse-while))
     ((string= tstr "case") (parse-case))
     ((string= tstr "_cmt") (parse-vmcomment))
     (t (panic (format "unsupported (%s)" tstr))))))

(defun parse-stmts ()
  (let ((stmts '()))
    (while (not (string= (token:get-str (peek))
                         "}"))
      (setq stmts (u:append-one stmts
                                (parse-stmt))))
    stmts))

(defun parse-var ()
  (let ((stmt '("var")))
    (consume "var")
    (let ((var-name (token:get-str (peek-and-next))))
      (setq stmt (u:append-one stmt var-name)))
    (when (string= (token:get-str (peek)) "=")
      (consume "=")
      (setq stmt (u:append-one stmt (parse-expr))))
    (consume ";")
    stmt))

(defun parse-func-def ()
  (let ((fn-name)
        (args)
        (stmts))
    (setq stmts '())

    (consume "func")
    (setq fn-name (token:get-str (peek-and-next)))
    (consume "(")
    (setq args (parse-args))
    (consume ")")
    (consume "{")

    (while (not (string= "}"
                         (token:get-str (peek))))
      (let ((head (token:get-str (peek))))
        (setq stmts
              (u:append-one stmts
                            (cond
                             ((string= head "var") (parse-var))
                             (t (parse-stmt)))))))

    (consume "}")

    (list "func" fn-name args stmts)))

(defun parse ()
  (let ((top-stmts '("top_stmts")))
    (while (string= (token:get-str (peek))
                    "func")
      (let ((fndef (parse-func-def)))
        (setq top-stmts
              (u:append-one top-stmts fndef))))
    top-stmts))

(defun to-tokens (src)
  (let ((lines (seq-remove
                (lambda (line) (= 0 (length line)))
                (split-string src "\n"))))
    (mapcar 'json:parse lines)))

;; --------------------------------

(let ((src (u:read-stdin-all))
      (ast))
  (setq tokens (to-tokens src))
  (setq ast (parse))
  (json:prettyprint ast))
