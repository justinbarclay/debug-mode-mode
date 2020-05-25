;;; debug-mode-time-travel.el --- debug-mode-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/debug-mode-mode
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
;;; This is split off from debug-mode-mode file because it's conceptually different and focused enough that _I_ need to be in
;;; a different space when looking at it.

;; Local State

(defvar debug-mode--time-traveling-p nil "Whether we are in the middle of time traveling")
(defvar-local debug-mode--time-point nil "The current point in time (index into state history) we have traveled do.")

;; I think one unanswered question with time travelling is what do we do with state _while_ we are time travelling?
;; even though I am still time traveling, that doesn't stop the Emacs system from running. Async state could still be running for example:
;; an LSP server might still be running and add something to a watched buffer.

;; I think we should keep tracking state while time travelling, that will cause our time travel bar to grow, and the user should still
;; be able to scrub through it, they just won't get live updates. Not until they leave time travelling mode.

(defun time-travel-max ()
  (length debug-mode-state-history))

;; Initiates state so we can go and time travel.
;; Step 1. Don't forget your towel.
;; We are copying the current state here as a lazy way to cache it
;; an alternative approach would be to have an explicit cache variable
;; that we would restore from when we stop time travelling, but that doesn't serve
;; too much purpose.

(defun time-travel-init ()
  "Initiates state so we can play amongst the stars."
  (when (not debug-mode--time-traveling-p)
    (setq debug-mode--time-traveling-p 't)
    (setq-local debug-mode--time-point (time-travel-max))))


;; Leaving time travelling should pop the last element off the list
;; and add it back to the debug-mode-state as well as resetting local state
(defun time-travel-quit ()
  "Reset state once we are done time traveling"
  (progn ;; Clustering operation for restoring state
    (setq debug-mode-state-history (reverse debug-mode-state-history))
    (setq debug-mode-state-history (reverse debug-mode-state-history)))
  (setq-local debug-mode--time-point nil)
  (setq debug-mode--time-traveling-p nil))

;; Moving through time should:
;; Make sure our needed state is initialized
;; Make sure we don't travel outside of bounds (or (< 0) (> (length history)))
;; Move 1 unit of time
;; Warn user if we're at boundary and can't move anymore
;; Reprint state.


;; TODO: Reprinting of _state_ should be handled elsewhere. Probably as a hook onto the
;; variable change for debug-mode-buffer
;;
;; TODO: Enhancement time travel backwards should be able to time travel x number
;; of units and not just an arbitrary unit of one. Interface should be something like `C-X <`
;; where X is the number of units to jump backwards.
(defun time-travel-backwards ()
  "Move back in time as long as time is a positive number. We don't want to start another big bang."
  (interactive)
  (time-travel-init)
  (if (> debug-mode--time-point 0)
      (progn
        (setq debug-mode--time-point (1- debug-mode--time-point))
        (setq debug-mode-state (nth debug-mode--time-point
                                    debug-mode-state-history))
        (print-state debug-mode-buffer))
    (message "We can't go back any further!")))


(defun time-travel-forwards ()
  "Move forward in time. But be careful We don't want to start another big bang."
  (interactive)
  (time-travel-init)
  (if (< debug-mode--time-point
         (1- (time-travel-max)))
      (progn
        (setq debug-mode--time-point (1+ debug-mode--time-point))
        (setq debug-mode-state (nth debug-mode--time-point
                                    debug-mode-state-history))
        (print-state debug-mode-buffer))
    (message "We can't go forward any further!")))

;; debug-mode-time-travel.el ends here
