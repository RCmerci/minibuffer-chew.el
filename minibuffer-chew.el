;;; minibuffer-chew.el --- chew things repeatedly      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  rcmerci

;; Author: rcmerci <rcmerci@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  minibuffer is unused most time just an empty line,
;;  so we can display something can always forget, like 'English words'...

;;; Code:

(defcustom minibuffer-chew-org-file "~/minibuffer-chew-things.org"
  "The org file to display on minibuffer"
  :type 'string)

(defcustom minibuffer-chew-wait-secs-after-idle 8
  "Duration to wait after emacs is idle. To make it not so fast to change."
  :type 'integer)

(defvar minibuffer-chew--check-timer nil
  "Timer to check minibuffer is unused or not")

;;;###autoload
(defun minibuffer-chew-start ()
  "Start minibuffer-chew loop, which will display message when it's unused"
  (interactive)
  (if (not (null minibuffer-chew--check-timer))
      (user-error "minibuffer chew has started"))
  (minibuffer-chew--display-when-idle)
  )

(defun minibuffer-chew-stop ()
  "Stop minibuffer-chew loop."
  (interactive)
  (when (not (null minibuffer-chew--check-timer))
    (cancel-timer minibuffer-chew--check-timer)
    (setq minibuffer-chew--check-timer nil))
  (message "minibuffer chew is stopped"))


(defun minibuffer-chew--display-when-idle ()
  (setq minibuffer-chew--check-timer
	(run-with-idle-timer minibuffer-chew-wait-secs-after-idle t
			     (lambda ()
			       (minibuffer-chew--display minibuffer-chew-org-file)))))


(defun minibuffer-chew--display (org-file)
  (when (not (file-exists-p org-file))
    (minibuffer-chew-stop)
    (user-error "%S not exists" org-file))
  (let ((org-buf (find-file-noselect org-file)))
    (with-current-buffer org-buf ;TODO: auto revert
      (let* ((record (minibuffer-chew--get-record-from-org-file))
	     (msg (format "[%S] %s" (plist-get record :todo-type) (plist-get record :raw-value))))
	(message msg))
      )
    )
  )

(defun minibuffer-chew--get-record-from-org-file ()
  "call this function when current-buffer is org `minibuffer-chew-org-file'"
  (let* ((records (mapcar
	   (lambda (a) (let ((value (cadr a)))
			 `(:todo-type ,(plist-get value :todo-type) :raw-value ,(plist-get value :raw-value))))
	   (cddr (org-element-parse-buffer 'headline))))
	 (todo-records (reduce (lambda (r a)
				 (if (eq 'todo (plist-get a :todo-type))
				     (cons a r)
				   r)) records :initial-value '()))
	 (done-records (reduce (lambda (r a)
				 (if (eq 'done (plist-get a :todo-type))
				     (cons a r)
				   r)) records :initial-value '())))
    (if (> (random 100) 20)
	(nth (random (length todo-records)) todo-records)
      (nth (random (length done-records)) done-records)
      )
    )
  )

(provide 'minibuffer-chew)
;;; minibuffer-chew.el ends here
