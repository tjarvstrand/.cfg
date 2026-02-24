(require 'thingatpt)

(defun tmpbuf (buf)
  "open a buffer,
if it doesn't exist, open a new one"
  (interactive "sBuffer name: ")
  (switch-to-buffer
   (get-buffer-create (concat "*" buf "*"))))

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(defun yank-and-inc ()
  (interactive)
  (let ((point (point)))
    (yank)
    (save-excursion
      (goto-char point)
      (re-search-forward "[[:digit:]]+" nil t)
      (increment-integer-at-point))))

(defun kill-trailing-whitespace ()
  (interacive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\s+$" nil t)
      (replace-match "" nil nil))))

(defun insert-filename (filename)
  (interactive "*FInsert filename: ")
  (insert filename))

(defun sudo-edit (&optional arg)
  "Edit file with superuser rights."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  "Edit the current file as root"
  (interactive)
  (let ((point (point)))
    (find-alternate-file
     (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
    (goto-char point)))

(defun symbol-next ()
  "Moves point to next occurrence, if any, of the symbol-at-point, if any"
  (interactive)
  (let ((mark          (point-marker))
        (symbol        (thing-at-point 'symbol))
        (symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (unless symbol
      (error "no symbol at point"))
    (goto-char (1+ (cdr symbol-bounds)))
    (search-symbol-forward symbol)
    (when (and (>= (point) (car symbol-bounds))
               (<= (point) (cdr symbol-bounds)))
      (error "No more ocurrences of %s" symbol))
    (goto-char (match-beginning 0))
    (push-mark mark t)))

(defun search-symbol-forward (symbol)
  (let ((point (point)))
    (or (find-next-symbol-forward symbol (point-max))
        (progn
          (goto-char (point-min))
          (find-next-symbol-forward symbol point)))))

(defun find-next-symbol-forward (symbol bound)
  (let ((res nil)
        (case-fold-search nil))
    (while (and (< (point) bound) (not res))
      (when (and (search-forward symbol bound 'move-point) (not res)
                 (save-match-data (string= (thing-at-point 'symbol) symbol)))
        (setq res (match-end 0))))
    res))

(defun symbol-previous ()
  "Moves point to previous occurrence, if any, of the symbol-at-point, if any"
  (interactive)
  (let ((mark          (point-marker))
        (symbol        (thing-at-point 'symbol))
        (symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (unless symbol
      (error "no symbol at point"))
    (goto-char (1- (car symbol-bounds)))
    (next-symbol-backward symbol)
    (when (and (>= (point) (car symbol-bounds))
               (<= (point) (cdr symbol-bounds)))
      (error "No more ocurrences of %s" symbol))
    (goto-char (match-beginning 0))
    (push-mark mark t)))

(defun next-symbol-backward (symbol)
  (let ((point (point)))
    (or (find-next-symbol-backward symbol (point-min))
        (progn
          (goto-char (point-max))
          (find-next-symbol-backward symbol point)))))

(defun find-next-symbol-backward (symbol bound)
  (let ((res nil)
        (case-fold-search nil))
    (while (and (> (point) bound) (not res))
      (when (and (search-backward symbol bound 'move-point) (not res)
                 (save-match-data (string= (thing-at-point 'symbol) symbol)))
        (setq res (match-end 0))))
    res))
