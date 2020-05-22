;;; parinfer-rust-mode.el --- parinfer-rust-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/parinfer-rust-mode
;; Version: 0.5.0
;; Package-Requires: ((emacs "25"))
;; Keywords: lisps

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:


;; Local State
(defvar debug-mode-state (make-hash-table) "The database of the debug mode tracked state")
(defvar debug-mode-cached-state nil "Cache the current debug-mode-state if we are time traveling")
(defvar-local debug-mode--time-traveling-p nil "Whether we are in the middle of time traveling")
(defvar-local debug-mode--time-point nil "The current point in time (index into state history) we have traveled do.")
(defvar debug-mode-state-history '())
(defvar debug-mode-buffer nil "The current debug mode buffer")
(defvar debug-mode-frame nil "The frame debug-mode-spawned")
(defvar debug-mode-watch-variables '() "Variables currently being watched by debug-mode")
(defvar debug-mode-formatters '() "An alist of variable names and their formatters")

;; Format strings
(defun print-state (buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (maphash (lambda (key val)
               (insert (format "%s %s\n" key val)))
             debug-mode-state)))

(defun debug-mode-watch-variable (symbol newval operation where)
  (add-to-list 'debug-mode-state-history (copy-hash-table debug-mode-state))
  (puthash symbol
           (if newval
               (apply
                (cadr
                 (assoc symbol debug-mode-formatters))
                (list newval))
             newval)
           debug-mode-state)
  (print-state debug-mode-buffer)
  newval)

(defun watch-variables (vars)
  (mapcar (lambda (var-&-formatter)
            (let ((var (car var-&-formatter)))
              (add-to-list 'debug-mode-formatters var-&-formatter )
              (add-to-list 'debug-mode-watch-variables var)
              (add-variable-watcher var
                                    'debug-mode-watch-variable)))
          vars)
  (print-state debug-mode-buffer))

(defun time-travel-max ()
  (length debug-mode-state-history))

(defun time-travel-backwards ()
  "Move back in time as long as time is a positive number. We don't want to start another big bang."
  (interactive)
  (when (not debug-mode--time-traveling-p)
    (setq-local debug-mode--time-traveling-p 't)
    (setq debug-mode-cached-state debug-mode-state)
    (setq-local debug-mode--time-point (1+ (time-travel-max))))
  (if (> debug-mode--time-point 0)
      (progn
        (setq debug-mode--time-point (1- debug-mode--time-point))
        (setq debug-mode-state (nth debug-mode--time-point
                                    debug-mode-state-history))
        (print-state debug-mode-buffer))
    (message "We can't go back any further!")))

(defun unwatch-variables (variables)
  (mapcar (lambda (var)
            (remove-variable-watcher var 'debug-mode-watch-variable))
          variables))

;; Enable and Disable state for debug-mode
(defun spawn-display ()
  (when (not debug-mode-buffer)
    (setq debug-mode-buffer (get-buffer-create "*debug-mode*")))
  (with-current-buffer debug-mode-buffer
    (print-state debug-mode-buffer)
    (setq debug-mode-frame (make-frame))))
;; (spawn-display)
(defun quit-debug-mode ()
  (interactive)
  (let* ((buffer debug-mode-buffer)
         (frame debug-mode-frame))
    (setq debug-mode-buffer nil)
    (setq debug-mode-state (make-hash-table))
    (setq debug-mode-state-history '())
    (setq debug-mode-frame nil)
    (setq debug-mode-cached-state nil)
    (setq debug-mode-formatters nil)
    (unwatch-variables debug-mode-watch-variables)
    (setq debug-mode-watch-variables '())
    (kill-buffer buffer)
    (make-frame-invisible frame)))

(defvar debug-mode-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c b") 'time-travel-backwards)
    m)
  "Keymap for `parinfer-rust-mode'.")
(define-minor-mode debug-mode-mode
  "A simpler way to write lisps"
  :lighter " debug"
  :init-value nil
  :keymap debug-mode-mode-map
  (if debug-mode-buffer
      (quit-debug-mode)
    (spawn-display)))

;; (define-derived-mode debug-mode-mode)
(watch-variables '((parinfer-rust--previous-options parinfer-rust-print-options) (parinfer-rust--current-changes parinfer-rust-print-changes)))
;; (unwatch-variables '(parinfer-rust--current-changes parinfer-rust--current-changes))
(provide 'debug-mode-mode)
;;; debug-mode-mode.el ends here
