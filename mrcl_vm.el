(add-to-list 'load-path (expand-file-name "."))
(load "lib/utils.el")
(load "lib/json.el")

(defvar vm:buffer-name "*vm*")
(defvar vm:timer)
(defvar vm:flag-false 0)

(defvar vm:stack-size)
(setq vm:stack-size 100)

(defvar vm:exit-p)
(defvar vm:mem-program)
(defvar vm:mem-stack)
(defvar vm:vram (make-vector 50 0))
(defvar vm:step-run-p t)

;; registers
(defvar vm:pc)
(defvar vm:reg-a)
(defvar vm:reg-a)
(defvar vm:sp)
(defvar vm:bp)
(defvar vm:zf)

(defvar vm:step)
(defvar vm:dump-interval)

(defun vm:reset ()
  (setq vm:mem-stack (make-vector vm:stack-size 0))
  (setq vm:vram (make-vector 50 0))

  (setq vm:reg-a 0)
  (setq vm:reg-b 0)
  (setq vm:pc 0)
  (setq vm:sp (1- vm:stack-size))
  (setq vm:bp vm:sp)
  (setq vm:zf vm:flag-false)

  (setq vm:step 0)
  (setq vm:exit-p nil))

(defun to-vram-index (x y)
  (+ (* y 5) x))

(defun vram-to-string (vm:vram)
  (let ((x 0) (y 0)
        (s "")
        (i))
    (while (< y 5)
      (setq x 0)
      (while (< x 5)
        (setq i (to-vram-index x y))
        (setq s (concat s
                        (if (= (aref vm:vram i) 0) " ." " @")))
        (setq x (1+ x)))
      (setq s (concat s "\n"))      
      (setq y (1+ y)))
    s))

(defun dump-program-insn (insn)
  (let ((head (car insn))
        (line (pp-to-string insn)))
    (cond ((string= head "label")
           line)
          (t (concat "  " line)))))

(defun dump-program ()
  (let ((s "")
        (head)
        (addr)
        (num-insns (length vm:mem-program)))
    (setq addr (- vm:pc 6))
    (while (< addr (+ vm:pc 6))
      (if (and (>= addr 0)
               (< addr num-insns))
          (progn
            (setq head (if (= addr vm:pc)
                           "pc => "
                         "      "))
            (setq s (concat s
                            (format "%s%d %s"
                                    head addr
                                    (dump-program-insn (elt vm:mem-program addr))
                                    ))))
        (progn
          (setq s (concat s ":\n"))))
      (setq addr (1+ addr)))
    s))

(defun dump-stack ()
  (let ((addr)
        (s "")
        (head))
    (setq i (- vm:sp 6))
    (while (<= i (+ vm:sp 6))
      (if (and (>= i 0)
               (< i vm:stack-size))
          ;; ok
          (progn
            (setq head
                  (cond
                   ((= i vm:sp)
                    (if (= vm:sp vm:bp)
                        "sp bp => "
                      "sp    => "))
                   ((= i vm:bp)
                    "   bp => ")
                   (t "         ")))
            (setq s (concat s (format "%s%d %d\n"
                                      head i (aref vm:mem-stack i)))))
        ;; ng
        (progn
          (setq s (concat s ":\n"))))
      (setq i (1+ i)))
    s))

(defun vm:dump ()
  (when (string= (buffer-name) vm:buffer-name)
    (erase-buffer)
    (insert (format "step-run (%s) step (%d)\n"
                    vm:step-run-p vm:step))
    (insert (format "pc (%d) a (%d) b (%d) sp (%d) bp (%d) zf (%d)\n"
                    vm:pc vm:reg-a vm:reg-b vm:sp vm:bp vm:zf))
    (insert "---- memory: program ----\n")
    (insert (dump-program))
    (insert "---- memory: stack ----\n")
    (insert (dump-stack))
    (insert "---- memory: vram ----\n")
    (insert (vram-to-string vm:vram))
    (insert "----\n")
    (insert "s: toggle auto/step run / SPC: step run / q: quit\n")))

(defun vm:set-sp (addr)
  (if (< addr 0)
      (panic "stack overflow")
    (setq vm:sp addr)))

(defun mem-ref-p (arg)
  (u:start-with-p arg "mem:"))

(defun calc-indirect-addr (mem-ref)
  (let ((parts (split-string mem-ref ":"))
        (base-str)
        (disp-str)
        (base))
    (setq base-str (elt parts 1))
    (setq disp-str (elt parts 2))
    (setq base (vm:get-val base-str))
    (+ base (string-to-number disp-str))))

(defun vm:get-val (arg)
  (cond
   ((stringp arg)
    (cond ((string= arg "reg_a") vm:reg-a)
          ((string= arg "reg_b") vm:reg-b)
          ((string= arg "sp"   ) vm:sp   )
          ((string= arg "bp"   ) vm:bp   )
          ((mem-ref-p arg)
           (let ((addr))
             (setq addr (calc-indirect-addr arg))
             (aref vm:mem-stack addr)))
          (t (panic (format "unsupported / arg (%s)" arg)))))
   ((integerp arg)
    arg)
   (t
    (panic "unsupported / arg"))))

(defun vm:set-val (dest val)
  (cond
   ((string= dest "reg_a") (setq vm:reg-a val))
   ((string= dest "reg_b") (setq vm:reg-b val))
   ((string= dest "sp") (setq vm:sp val))
   ((string= dest "bp") (setq vm:bp val))
   ((mem-ref-p dest)
    (let ((addr))
      (setq addr (calc-indirect-addr dest))
      (aset vm:mem-stack addr val)))
   (t (panic (format "unsupported / dest (%s)" dest)))))

;; --------------------------------

(defun insn:mov (insn)
  (let ((dest (elt insn 1))
        (src  (elt insn 2))
        (src-val))
    (setq src-val (vm:get-val src))
    (vm:set-val dest src-val)))

(defun insn:add (insn)
  (let ((dest (elt insn 1))
        (src  (elt insn 2))
        (dest-val)
        (src-val))
    (setq dest-val (vm:get-val dest))
    (setq src-val (vm:get-val src))
    (vm:set-val dest (+ src-val dest-val))))

(defun insn:mul (insn)
  (let ((src  (elt insn 1))
        (src-val))
    (setq src-val (vm:get-val src))
    (setq vm:reg-a  (* vm:reg-a src-val))))

(defun insn:cmp (insn)
  (setq vm:zf (if (= vm:reg-a vm:reg-b)
                  1 0)))

(defun insn:jmp (insn)
  (let ((dest-addr (elt insn 1)))
    (setq vm:pc dest-addr)))

(defun insn:je (insn)
  (let ((dest-addr (elt insn 1)))
    (if (= vm:zf vm:flag-false)
        (vm:pc-increment)
      (setq vm:pc dest-addr))))

(defun insn:call (insn)
  (let ((dest-addr (elt insn 1)))
    (setq vm:sp (1- vm:sp))
    (aset vm:mem-stack vm:sp (1+ vm:pc))
    (setq vm:pc dest-addr)))

(defun insn:ret (insn)
  (let ((ret-addr))
    (setq ret-addr (aref vm:mem-stack vm:sp))
    (setq vm:pc ret-addr)
    (vm:set-sp (1+ vm:sp))))

(defun insn:push (insn)
  (let ((arg (elt insn 1))
        (val))
    (setq val (vm:get-val arg))
    (vm:set-sp (1- vm:sp))
    (aset vm:mem-stack vm:sp val)))

(defun insn:pop (insn)
  (let ((arg (elt insn 1))
        (val))
    (setq val (aref vm:mem-stack vm:sp))
    (vm:set-val arg val)
    (vm:set-sp (1+ vm:sp))))

(defun insn:set-vram (insn)
  (let ((arg-vram (elt insn 1))
        (arg-val  (elt insn 2))
        (src-val))
    (setq src-val (vm:get-val arg-val))
    (cond ((integerp arg-vram)
           (aset vm:vram arg-vram src-val))
          ((mem-ref-p arg-vram)
           (let ((stack-addr)
                 (vram-addr))
             (setq stack-addr (calc-indirect-addr arg-vram))
             (setq vram-addr (aref vm:mem-stack stack-addr))
             (aset vm:vram vram-addr src-val)))
          (t (panic "unsupported arg-vram")))))

(defun insn:get-vram (insn)
  (let ((arg-vram (elt insn 1))
        (arg-dest (elt insn 2))
        (vram-addr)
        (val))
    (setq vram-addr (vm:get-val arg-vram))
    (setq val (aref vm:vram vram-addr))
    (vm:set-val arg-dest val)))

;; _cmt TODO
;; _debug TODO

;; --------------------------------

(defun vm:pc-increment ()
  (setq vm:pc (1+ vm:pc)))

(defun tick ()
  (unless vm:exit-p
    (let ((insn)
          (head))
      ;; fetch
      (setq insn (elt vm:mem-program vm:pc))

      ;; execute
      (setq head (car insn))
      (cond ((eq head 'exit    ) (setq vm:exit-p t))
            ((eq head 'mov     ) (insn:mov      insn) (vm:pc-increment))
            ((eq head 'add     ) (insn:add      insn) (vm:pc-increment))
            ((eq head 'mul     ) (insn:mul      insn) (vm:pc-increment))
            ((eq head 'cmp     ) (insn:cmp      insn) (vm:pc-increment))
            ((eq head 'label   )                      (vm:pc-increment))
            ((eq head 'jmp     ) (insn:jmp      insn))
            ((eq head 'je      ) (insn:je       insn))
            ((eq head 'call    ) (insn:call     insn))
            ((eq head 'ret     ) (insn:ret      insn))
            ((eq head 'push    ) (insn:push     insn) (vm:pc-increment))
            ((eq head 'pop     ) (insn:pop      insn) (vm:pc-increment))
            ((eq head 'set_vram) (insn:set-vram insn) (vm:pc-increment))
            ((eq head 'get_vram) (insn:get-vram insn) (vm:pc-increment))
            ((eq head '_cmt )                  (vm:pc-increment))
            ;; TODO _debug
            (t
             (panic (format "unsupported / insn (%s)" insn))))

      (when (= (% vm:step vm:dump-interval) 0)
        (vm:dump))
      (setq vm:step (1+ vm:step))
      (when vm:exit-p
        (insert "EXIT\n")
        (vm:timer-stop)))))

(defun vm:update-dump-interval ()
  (setq vm:dump-interval
        (if vm:step-run-p 1 200)))

(defun vm:step-run-p-set (val)
  (setq vm:step-run-p val)
  (vm:update-dump-interval))

(defun vm:toggle-step-run ()
  (interactive)
  (vm:step-run-p-set (not vm:step-run-p))
  (vm:dump))

(defun vm:step ()
  (interactive)
  (tick))

(defun vm:load-program (exe-file)
  (let ((src)
        (lines)
        (insns))
    (setq src (u:read-file-all exe-file))
    (setq lines (seq-remove
                 (lambda (line) (= 0 (length line)))
                 (split-string src "\n")))
    (setq insns (mapcar 'json:parse lines))
    (setq insns
          (mapcar (lambda (insn)
                    (let ((head (car insn))
                          (rest (cdr insn)))
                      (cons (intern head) rest)))
                  insns))
    (setq vm:mem-program (make-vector (length insns) nil))
    (u:each-with-index
     insns
     (lambda (insn i)
       (aset vm:mem-program i insn)))))

(defun vm:timer-stop ()
  (cancel-timer vm:timer))

(let ((buf (get-buffer-create vm:buffer-name))
      (exe-file (getenv "EXEFILE"))
      (t2))

  ;; load machine code
  (vm:load-program exe-file)

  (vm:step-run-p-set t)
  ;; (vm:update-dump-interval)

  (vm:reset)

  (switch-to-buffer buf)
  (erase-buffer)

  ;; 初回表示
  (vm:dump)

  (global-set-key (kbd "s") 'vm:toggle-step-run)
  (global-set-key (kbd "SPC") 'vm:step)
  (global-set-key (kbd "q") 'kill-emacs)

  ;; 定期実行
  (setq vm:timer (run-at-time
            t
            0.0001
            (lambda ()
              (unless vm:step-run-p
                ;; ステップ実行モードの場合は勝手に進めない
                (tick)))))

  ;; ;; ずっと動き続けないように一定時間過ぎたら止める
  ;; (setq t2 (run-at-time "600 sec" nil
  ;;                       (lambda ()
  ;;                         (message "cancel!")
  ;;                         (vm:timer-stop))))
  )

;; (eval-buffer)
