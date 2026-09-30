;;; $DOOMDIR/config/text.el --- miscellaneous text editing -*- lexical-binding: t -*-
(defun tao/toggle-text-boolean ()
  "Toggle the boolean string at point (true/false) preserving case."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
          (word (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (when word
      (let ((toggle-map '(("true" . "false") ("false" . "true")
                           ("t" . "nil") ("nil" . "t")))
             (lower-word (downcase word)))
        (when (assoc lower-word toggle-map)
          (let ((new-word (cdr (assoc lower-word toggle-map))))
            (delete-region (car bounds) (cdr bounds))
            (insert (cond
                      ((string-equal word (upcase word)) (upcase new-word))
                      ((string-equal word (capitalize word)) (capitalize new-word))
                      (t new-word)))))))))

(global-set-key (kbd "C-c t") #'tao/toggle-text-boolean)

(defun insert-sum-before-first-number (beg end)
  "Sum all numbers in the region and insert the sum plus a space
before the first number."
  (interactive "r")
  (save-excursion
    (let ((sum 0)
           (first nil))
      (goto-char beg)
      (while (re-search-forward "-?[0-9]+\\(?:\\.[0-9]+\\)?" end t)
        (unless first
          (setq first (match-beginning 0)))
        (setq sum (+ sum (string-to-number (match-string 0)))))
      (if first
        (progn
          (goto-char first)
          (insert (format "%.10g" sum) " "))
        (message "No numbers found in region")))))

(map! :v "g+" #'insert-sum-before-first-number)
