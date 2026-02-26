(setq x-select-enable-clipboard 1)

(defun split-windows-to-size (size)
  (while (> (window-width) (* 2 size))
    (split-window-horizontally (- (window-width) size)))
  (balance-windows))

(run-with-timer 0.2 nil #'(lambda () (split-windows-to-size 120)))
