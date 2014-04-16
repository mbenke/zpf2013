(defun insert-lhs-code ()
  "Insert lhs code block at cursor point."
  (interactive)
  (insert "\\begin{code}\n\n\\end{code}\n")
  (previous-line 2))


(global-set-key "\C-cc" 'insert-lhs-code)


