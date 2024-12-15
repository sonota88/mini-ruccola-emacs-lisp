(add-to-list 'load-path (expand-file-name "."))
(load "lib/utils.el")
(load "lib/json.el")

(defvar keywords '("func" "return" "var" "set" "call" "call_set"
            "while" "case" "when"
            "_cmt" "_debug"))

(defun print-token-v1 (lineno kind str)
  (princ (format "[%d, \"%s\", \"%s\"]\n"
                 lineno kind str)))

(defun print-token (lineno kind str)
  (json:print (list lineno kind str))
  (princ "\n"))

(defun keyword-p (str) (member str keywords))

;; --------------------------------

(let ((src (u:read-stdin-all))
      (pos 0)
      (lineno 1)
      (rest nil)
      (matched nil))
  (while (< pos (length src))
    (setq rest (substring src pos))
    (cond
     ((string-match "\\`\\( \\)" rest)
      (setq matched (match-string 1 rest))
      (setq pos (+ pos (length matched))))
     ((string-match "\\`\\(\n\\)" rest)
      (setq matched (match-string 1 rest))
      (setq lineno (1+ lineno))
      (setq pos (+ pos (length matched))))
     ((string-match "\\`\\(//.*\\)$" rest)
      (setq matched (match-string 1 rest))
      (setq pos (+ pos (length matched))))
     ((string-match "\\`\\(==\\|!=\\|[(){};,=+*]\\)" rest)
      (setq matched (match-string 1 rest))
      (print-token lineno "sym" matched)
      (setq pos (+ pos (length matched))))
     ((string-match "\\`\\(-?[0-9]+\\)" rest)
      (setq matched (match-string 1 rest))
      (print-token lineno "int" matched)
      (setq pos (+ pos (length matched))))
     ((string-match "\\`\\([a-z_][a-z0-9_]*\\)" rest)
      (setq matched (match-string 1 rest))
      (print-token lineno
                   (if (keyword-p matched) "kw" "ident")
                   matched)
      (setq pos (+ pos (length matched))))
     ((string-match "\\`\"\\(.*?\\)\"" rest)
      (setq matched (match-string 1 rest))
      (print-token lineno "str" matched)
      (setq pos (+ pos (length matched) 2)))
     (t (panic (format "unexpected pattern (%s)" rest))))))
