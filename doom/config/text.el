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
