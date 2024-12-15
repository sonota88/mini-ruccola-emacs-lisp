(add-to-list 'load-path (expand-file-name "."))
(load "lib/utils.el")
(load "lib/json.el")

(let ((json (u:read-stdin-all))
      (data nil))
  (setq data (json:parse json))
  (json:prettyprint data))
