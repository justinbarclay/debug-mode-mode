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

;; Modules
;; A hack until we get this fully loaded as a library
(load-file "./debug-mode-time-travel.el")


;; Local State
(defvar debug-mode-state (make-hash-table) "The database of the debug mode tracked state")
(defvar debug-mode-cached-state nil "Cache the current debug-mode-state if we are time traveling")
(defvar debug-mode-state-history '())
(defvar debug-mode-buffer nil "The current debug mode buffer")
(defvar debug-mode-frame nil "The frame debug-mode-spawned")
(defvar debug-mode-variables '() "Variables currently being watched by debug-mode")
(defvar debug-mode-formatters '() "An alist of variable names and their formatters")
(defvar debug-mode--func-name "")
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun debug-mode--reset-state ()
  (setq debug-mode-buffer nil)
  (setq debug-mode--time-traveling-p nil)
  (setq debug-mode-state (make-hash-table))
  (setq debug-mode-state-history '())
  (setq debug-mode-frame nil)
  (setq debug-mode-cached-state nil)
  (setq debug-mode-formatters nil)
  (setq debug-mode-watch-variables '()))

(defun progress-bar (percentage)
  "Percentage is how complete the current progress is."
  (let* ((bar-length (- (window-width) 12))
         (before (round (* bar-length percentage)))
         (after (if (= percentage 1.0)
                    0
                    (- bar-length 1 before))))
    (format "\[%s%s%s\]"
            (make-string before ?#)
            (if (= percentage 1.0)
                "#"
                "-")
            (make-string after ?_))))

;; TODO: Get this down to one insert statement, this will perform much better
;; As per: https://alphapapa.github.io/emacs-package-dev-handbook/#Inserting%20strings
(defun print-state (buffer)
  "Updates BUFFER with the current state."
  (with-current-buffer buffer
    (when buffer-read-only
      (read-only-mode 'toggle))
    ;; Clear state
    (erase-buffer)
    ;; Draw progress bar when time traveling
    (when (> (time-travel-max) 0)
      (insert (progress-bar (/ (or debug-mode--time-point
                                   (time-travel-max))
                               (time-travel-max)
                               1.0))
              "\n"))
    (let ((keys '(variables functions)))
      ;; This is going to be really ugly fixup later
      ;; For each buffer print out all state stored in functions and variables
      (maphash (lambda (buf-or-sym state)
                 (insert
                  (if (symbolp buf-or-sym)
                      (symbol-name buf-or-sym)
                    (buffer-name buf-or-sym))
                  "\n")
                 (maphash (lambda (key debug-state)
                           (insert (make-string 2 ? ) (format "%s" key) "\n")
                           (maphash (lambda (key val)
                                      (insert
                                       (make-string 4 ? )
                                       (format "%s %s\n" key val)))
                                    debug-state))
                         state))
               debug-mode-state))
    (read-only-mode 'toggle)))

(defun __find-or-make-new-table (table key)
  (let ((child (gethash key table (make-hash-table))))
    (if (hash-table-empty-p child)
        (puthash key child table)
      (puthash key (copy-hash-table child) table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Watchers and Unwatchers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun debug-mode-watch-variable (symbol newval operation where)
  (let* ((temp-state (if debug-mode--time-traveling-p
                         (copy-hash-table (or (car (last debug-mode-state-history))
                                              (make-hash-table)))
                       debug-mode-state))
         (current-buf (or where 'global))
         (buffer-state (__find-or-make-new-table temp-state current-buf))
         (variable-state (__find-or-make-new-table buffer-state 'variables)))
    (puthash symbol
             (if newval
                 (apply
                  (cadr
                   (assoc symbol debug-mode-formatters))
                  (list newval))
               newval)
             variable-state)
    (add-to-list 'debug-mode-state-history (copy-hash-table temp-state) 't)
    (print-state debug-mode-buffer)
    newval))

(defun debug-mode-track-function (func &rest args)
  (let* ((temp-state (if debug-mode--time-traveling-p
                         (copy-hash-table (or (car (last debug-mode-state-history))
                                              (make-hash-table)))
                       debug-mode-state))
         (current-buf (or (current-buffer) 'global))
         (buffer-state (__find-or-make-new-table temp-state current-buf))
         (func-state (__find-or-make-new-table buffer-state 'functions))
         (formatted-args (if (and args
                                  (car args))
                             (mapconcat (lambda (element)
                                          (format "%s" element))
                                        args
                                        ", ")
                           args))
         (spacer (make-string 4 ? )))
    (puthash func
             (format
              "Args: %s"
              formatted-args)
             func-state)
    (add-to-list 'debug-mode-state-history (copy-hash-table temp-state) 't)
    (print-state debug-mode-buffer)))

(defmacro watcher-skeleton ()
  "A macro for defining a function that that wraps "
  ;; Todo, this is relying on global state and isn't very nice. This should be refactored so it's simpler
  (let ((func-name (intern (concat "debug-mode-watch-" (symbol-name debug-mode--func-name))))
        (func-sym  debug-mode--func-name))
    `(defun ,func-name
         (&rest args)
         "Wraps debug-mode-watch-function to pass in the current functions name."
       (apply 'debug-mode-track-function
              ',func-sym
              args))))

(defun watch-variables (vars)
  (mapcar (lambda (var-&-formatter)
            (let ((var (car var-&-formatter)))
              (add-to-list 'debug-mode-formatters var-&-formatter )
              (add-to-list 'debug-mode-variables var)
              (add-variable-watcher var
                                    'debug-mode-watch-variable)))
          vars)
  (print-state debug-mode-buffer))

(defun watch-functions (functions)
  (dolist (func functions)
    ;; TODO this is relying on global state and isn't very nice, need to find a better work around
    (setq debug-mode--func-name func)
    (setq new-func (watcher-skeleton))
    (advice-add func :before new-func))
  (setq new-func nil))

(defun unwatch-variables (variables)
  (mapcar (lambda (var)
            (remove-variable-watcher var 'debug-mode-watch-variable))
          variables))

(defun unwatch-functions (functions)
  (mapcar (lambda (func)
            (advice-remove var 'debug-mode-watch-function))
          functions))

;; Open up "debug mode" program
(defun spawn-display ()
  (when (not debug-mode-buffer)
    (setq debug-mode-buffer (get-buffer-create "*debug-mode*")))
  (with-current-buffer debug-mode-buffer
    (print-state debug-mode-buffer)
    (setq debug-mode-frame (make-frame))
    (debug-mode-mode)))

;; Functions intended to be called by users
(defun quit-live-debug ()
  (interactive)
  (let* ((buffer debug-mode-buffer)
         (frame debug-mode-frame))
    (unwatch-variables debug-mode-variables)
    (debug-mode--reset-state)
    (kill-buffer buffer)
    (make-frame-invisible frame)))

(defvar debug-mode-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<") 'time-travel-backwards)
    (define-key m (kbd ">") 'time-travel-forwards)
    (define-key m (kbd "q") 'quit-live-debug)
    m)
  "Keymap for `debug-mode-mode'.")

(defun live-debug ()
  (interactive)
  (spawn-display)
  (watch-functions '(mc/create-cursor-id right-char left-char))
  (watch-variables '((mc--current-cursor-id identity))))

(define-derived-mode debug-mode-mode special-mode
  "Live Debug"
  "Add a live debug mode to your emacs application"
  :group "debug-mode-mode"
  :abbrev-mode nil
  :syntax-table nil
  (use-local-map debug-mode-mode-map))

;; (unwatch-variables '(parinfer-rust--current-changes parinfer-rust--current-changes))
(provide 'debug-mode-mode)
;;; debug-mode-mode.el ends here
