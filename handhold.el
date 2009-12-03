;;; handhold.el --- GUI for constructing tests

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: extensions, maint, tools, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Holds the user's hand while building a sexp, and puts it on top of
;; the kill ring when built.

;; There are no entry points in this file, but entry points for
;; particular uses can be defined with the macro `handhold-define'.

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'pp)
(eval-when-compile 
  (require 'cl))

;;Supply another completion key for PC-type keyboards.
(define-key widget-field-keymap [ESC TAB] #'widget-complete)

(defvar handhold-root-widget nil "" )


;;;
;;Helper widgets.

(define-widget 'hh-quoted-symbol
   'symbol "Make an env index"

   :value-to-internal 
   (lambda (widget value)
      (if (symbolp value)
	 (symbol-name value)
	 value))
   :value-to-external 
   (lambda (widget value)
      (list 'quote
	 (if (stringp value)
	    (intern value)
	    value))))

;;;
;;Define helpers.

(defun hh-hidden-sym (sym)
   ""
   `(const :format "" :value ,sym))


(defun hh-keyed-field (key field)
   ""
   (list
      (hh-hidden-sym key)
      field))

(defun hh-choice-from-symlist (main-tag default-val args)
   ""
   `
   (choice 
      :tag ,main-tag
      :value ,default-val
      ,@(mapcar
	   #'(lambda (arg)
		(if
		   (symbolp arg)
		   `(const :tag ,(symbol-name arg) :value ,arg)
		   `(const :tag ,(first arg) :value ,(second arg))))
	   args)))


;;;
;;Helper functions

;;Borrowed from the widget docs and encapped.
(defun handhold-fully-erase-buffer () ""
  (let
    ((inhibit-read-only t))
    (erase-buffer))
  (let
    ((all
       (overlay-lists)))
    (mapcar 'delete-overlay
      (car all))
    (mapcar 'delete-overlay
      (cdr all))))

(defun handhold-setup (widget-inserter &optional buf-name)
   ""
   (setq buf-name (or buf-name "*Handhold sexp builder*"))

   (pop-to-buffer buf-name)
   (kill-all-local-variables)
   (make-local-variable 'handhold-root-widget)

   (handhold-fully-erase-buffer)
   (funcall widget-inserter)

   (use-local-map widget-keymap)
   (widget-setup)
   (goto-char (point-min)))


(defun handhold-insert-widgets (widget-data &optional title)
   ""
   (setq title (or title "Expression builder"))
   (check-type title string)
   ;;Start adding the widgets
   (widget-insert title)
   (widget-insert "\n---------------------------------------\n\n")

   (setq handhold-root-widget
      (apply #'widget-create widget-data))
  
   (widget-insert "\n\n---------------------------------------\n\n\n")

   ;;Add buttons to commit/abort
   (widget-create 'push-button
      :notify 
      #'(lambda (&rest ignore) 
	   "Save the root widget's value as a kill entry"
	   (kill-new
	      (pp-to-string
		 (widget-value handhold-root-widget))))
    
      "Save expression to kill-ring")

   (widget-insert "    ")

   (widget-create 'push-button
      :notify 
      (lexical-let
	 ((widget-data widget-data)
	    (title title))
	 #'(lambda (&rest ignore)
	      (handhold-build-sexp widget-data title)))
      "Reset Form")

   (widget-insert "\n"))


(defun handhold-build-sexp (widget-data &optional title)
   "Interactively build a sexp according to WIDGET-DATA
Leave the sexp at the top of the kill ring."

   (handhold-setup 
      #'(lambda ()
	   (handhold-insert-widgets widget-data title))))


(defmacro handhold-define (name exp &optional title)
  "Create a function named NAME to build suitable sexp via widgets.
EXP is an expression suitable to apply create-widget to.
TITLE is the title to display in the widget buffer."
  
  `(defun ,name ()
     "Build a suitable sexp via widgets."
     (interactive)
     (handhold-build-sexp ,exp ,title)))


(provide 'handhold)

;;; handhold.el ends here
