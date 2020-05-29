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
(require 'debug-mode-time-travel)


;; Local State
(defvar debug-mode-state (make-hash-table) "The database of the debug mode tracked state")
(defvar debug-mode-cached-state nil "Cache the current debug-mode-state if we are time traveling")
(defvar debug-mode-state-history '())
(defvar debug-mode-buffer nil "The current debug mode buffer")
(defvar debug-mode-frame nil "The frame debug-mode-spawned")
(defvar debug-mode-variables '() "An alist of variables and formatters to display those variables in the debug-mode window.")
(defvar debug-mode--func-name "")
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun progress-bar (percentage)
  "Create meant take up the width of the frame and be filled a percentage value"
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
                   (assoc symbol debug-mode-variables))
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
  (mapcar (lambda (watch-var)
            (let ((var-&-formatter (if (listp watch-var)
                                       watch-var
                                     (list watch-var 'identity))))
              (add-to-list 'debug-mode-variables var-&-formatter)
              (add-variable-watcher (car var-&-formatter)
                                    'debug-mode-watch-variable)))
          vars)
  (print-state debug-mode-buffer))

(defun watch-functions (functions)
  (dolist (func functions)
    (let ((func-name "1"))
      (defalias (intern (concat "debug-mode-watch-" (symbol-name debug-mode--func-name))) (lambda () 1)))
    ;; TODO this is relying on global state and isn't very nice, need to find a better work around
    (setq debug-mode--func-name func)
    (setq new-func (watcher-skeleton))
    (advice-add func :before new-func))
  (setq new-func nil))

(defun unwatch-variables (variables)
  (mapcar (lambda (var-&-formatter)
            (remove-variable-watcher (car var-&-formatter) 'debug-mode-watch-variable))
          variables))

(defun unwatch-functions (functions)
  (mapcar (lambda (func)
            (advice-remove func (intern (concat "debug-mode-watch-" (symbol-name func)))))
          functions))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set-up mode
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun debug-mode--reset-state ()
  (setq debug-mode-buffer nil)
  (setq debug-mode--time-traveling-p nil)
  (setq debug-mode-state (make-hash-table))
  (setq debug-mode-state-history '())
  (setq debug-mode-frame nil)
  (setq debug-mode-cached-state nil)
  (setq debug-mode-watch-variables '()))

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
    (unwatch-functions debug-mode-watch-functions)
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
  "live-debug starts a live debugging session. It starts tracking the variables stored in debug-mode-watch-variables and
   the functions stored in debug-mode-watch-functions. Then it will create a new frame and opens into the debug-mode buffer."
  (interactive)
  (spawn-display)
  (watch-functions debug-mode-watch-functions)
  (watch-variables debug-mode-watch-variables))

(define-derived-mode debug-mode-mode special-mode
  "Live Debug"
  "Add a live debug mode to your emacs application"
  :group "debug-mode-mode"
  :abbrev-mode nil
  :syntax-table nil
  (use-local-map debug-mode-mode-map))

(setq debug-mode-watch-functions '(multiple-cursors-mode mc/create-cursor-id right-char left-char
                                                         previous-line next-line yank
                                                         time-travel-backwards time-travel-forwards))
(setq debug-mode-watch-variables '(mc--current-cursor-id))

(provide 'debug-mode-mode)
;;; debug-mode-mode.el ends here
