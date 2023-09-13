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

(defun clone-line-below ()
  "Create a copy of the current line below"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (newline)
  (yank))

(defun move-line-up ()
  "Swap the current line and the line above it"
  (interactive)
  (beginning-of-line)
  (kill-line 1)
  (previous-line)
  (yank)
  (backward-char))

(defun move-line-down ()
  "Swap the current line and the line above it"
  (interactive)
  (beginning-of-line)
  (kill-line 1)
  (next-line)
  (yank)
  (backward-char))

(defun make-directory-if-not (DIR)
  "Create DIR if it doesn't exist and return it's name"
  (if (not (file-directory-p DIR)) (make-directory DIR))
  DIR)

(defun find-org-file ()
  "Open a find-file dialog in the org directory"
  (interactive)
  (ido-find-file-in-dir org-directory))

(provide 'borealis-fn)
