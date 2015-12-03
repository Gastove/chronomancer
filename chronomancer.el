;;; chronomancer.el -- A minor mode for working with time

;;; Commentary:

;;; Code:
(require 'thingatpt)
(require 'eldoc)

(defvar chrono/epoch-time-regexp "[0-9]\\{13\\}\\b")
(defvar chrono/date-format "%Y-%m-%d")
(defvar chrono/date-time-format "%Y-%m-%d %T.%3N")

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
    (format-time-string chrono/date-time-format time)))

(defun chrono/echo-millis-at-point ()
  "Hi."
  (interactive)
  (let ((millis-str))
    (if (setq millis-str (thing-at-point 'chrono/millis t))
        (message (chrono/millis-to-iso-date-time (string-to-number millis-str)))
      (message "Thing-at-point not recognized as date; see variable chrono/epoch-time-regexp"))))

(defun chrono/insert-iso-date ()
  "Insert the current date in ISO format."
  (interactive)
  (insert (format-time-string chrono/date-format (current-time))))

(defvar chrono/key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x t i") #'chrono/insert-iso-date)
    (define-key map (kbd "C-x t d") #'chrono/echo-millis-at-point)
    map))

;; ElDoc support
;; ...doesn't currently work.
(defun chrono/eldoc-function ()
  "Echo a human-readable date-time representation in the echo ara via ElDoc."
  (let ((millis-str (thing-at-point 'chrono/millis t)))
    (if millis-str
        (chrono/millis-to-iso-date-time (string-to-number millis-str))
      nil)))

(define-minor-mode chronomancer-mode
  "A minor mode for working with time"
  :lighter " chrono"
  :keymap chrono/key-map
  (add-function :before-until (local 'eldoc-documentation-function)
                #'chrono/eldoc-function))

(provide 'chronomancer)
;;; chronomancer.el ends here
