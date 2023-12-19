(defun unintern-symbols-with-prefix (prefix)
  "Unintern all symbols with PREFIX."
  (interactive "sUnintern symbols with prefix: ")
  (let ((prefix-length (length prefix)))
    (mapatoms (lambda (symbol)
                (when (and (symbolp symbol) ; ensure it's a symbol
                           (>= (length (symbol-name symbol)) prefix-length) ; ensure name is long enough
                           (string-prefix-p prefix (symbol-name symbol)))
                  (when (fboundp symbol) ; if symbol has a function binding
                    (fmakunbound symbol)) ; remove the function binding
                  (when (boundp symbol) ; if symbol has a variable binding
                    (makunbound symbol)) ; remove the variable binding
                  (unintern symbol)))))  ; remove the symbol completely
  )

