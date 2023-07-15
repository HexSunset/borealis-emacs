(defun sudo-find-file ()
  "Open the file with sudo through TRAMP"
  (interactive)
    (ido-find-file-in-dir "/sudo::/"))

(defun open-line-below ()
  "Create a newline below the current line"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Create a newline above the current line"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun make-directory-if-not (DIR)
  "Create DIR if it doesn't exist and return it's name"
  (if (not (file-directory-p DIR)) (make-directory DIR))
  DIR)

(provide 'borealis-fn)
