;;; chronomancer.el -- A minor mode for working with time

;;; Commentary:

;;; Code:
(require 'thingatpt)

(defvar chrono/epoch-time-regexp "[0-9]\\{13\\}")

(defun chrono/find-bounds-of-millis ()
  "Defines a new `chrono/millis' type, to be passed to `thing-at-point'.

Note: for the time being, *only* understands 13-digit numbers
with no commas as \"millis\". This is a medium-dubious choice and
will be revisited."
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (if (looking-at chrono/epoch-time-regexp)
        (cons (point) (match-end 0))
      nil)))

;; Load the new boundary definition in to thing-at-points registry
(put 'chrono/millis 'bounds-of-thing-at-point
     'chrono/find-bounds-of-millis)

(defun chrono/millis-to-iso-date-time (millis)
  "Convert MILLIS to an ISO-formatted string."
  (let ((time (seconds-to-time (/ millis 1000))))
    (format-time-string "%Y-%m-%d %T.%3N" time)))

(defun chrono/echo-millis-at-point ()
  "Hi."
  (interactive)
  (let ((millis (string-to-number (thing-at-point 'chrono/millis t))))
    (message (chrono/millis-to-iso-date-time millis))))

(defun chrono/insert-iso-date ()
  "Insert the current date in ISO format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defvar chrono/key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x t i") #'chrono/insert-iso-date)
    (define-key map (kbd "C-x t d") #'chrono/echo-millis-at-point)
    map))

(define-minor-mode chronomancer-mode
  "A minor mode for working with time"
  :lighter " chrono"
  :keymap chrono/key-map)

(provide 'chronomancer)
;;; chronomancer.el ends here
