;; -*- lexical-binding: t; -*-

(defun borealis-ido-ignore-buffers (name)
  "Ignore all non-user (*starred*) buffers except the ones in borealis-ido-allow-buffers"
  (and (string-match "^\*" name)
       (not (member name borealis-ido-allow-buffers))))

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

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun make-directory-if-not (DIR)
  "Create DIR if it doesn't exist and return it's name"
  (if (not (file-directory-p DIR)) (make-directory DIR))
  DIR)

(defun find-org-file ()
  "Open a find-file dialog in the org directory"
  (interactive)
  (ido-find-file-in-dir org-directory))

(provide 'borealis-fn)
