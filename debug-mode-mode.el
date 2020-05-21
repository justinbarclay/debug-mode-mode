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
  (puthash symbol (apply (cadr (assoc symbol debug-mode-formatters)) (list newval)) debug-mode-state)
  (print-state debug-mode-buffer))

(defun watch-variables (vars)
  (mapcar (lambda (var-&-formatter)
            (let ((var (car var-&-formatter)))
              (add-to-list 'debug-mode-formatters var-&-formatter )
              (add-to-list 'debug-mode-watch-variables var)
              (add-variable-watcher var
                                    'debug-mode-watch-variable)))
          vars)
  (print-state debug-mode-buffer))

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
    (setq debug-mode-frame nil)
    (kill-buffer debug-mode-buffer)
    (make-frame-invisible debug-mode-frame)))

(define-minor-mode debug-mode-mode
  "A simpler way to write lisps"
  :lighter " debug"
  :init-value nil
  :keymap nil
  (message "Starting debug-mode-mode")
  (if debug-mode-buffer
      (spawn-display)
    (quite-debug-mode)))

(provide 'debug-mode-mode)
;;; debug-mode-mode.el ends here
