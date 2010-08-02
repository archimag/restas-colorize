;;;; packages.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-module #:restas.colorize
  (:use #:cl #:iter)
  (:export #:*storage*
           #:*finalize-page*
           #:*colorize-user-function*
           #:*max-on-page*
           #:paste
           #:paste-id
           #:paste-title
           #:paste-author
           #:paste-code
           #:paste-date
           #:paste-lang
           ;; storage
           #:storage-count-pastes
           #:storage-list-pastes
           #:storage-get-paste
           #:storage-add-paste
           #:storage-remove-paste))

(in-package #:restas.colorize)

;;;; load templates

(defparameter *colorize-template-path*
  (merge-pathnames "src/drawer.tmpl"
                   (asdf:component-pathname (asdf:find-system '#:restas-colorize))))

(closure-template:compile-template :common-lisp-backend
                                   *colorize-template-path*)


;;;; preferences

(defvar *max-on-page* 10)

(defvar *storage* nil)

(defparameter *colorize-user-function*
  #'(lambda () "anonymous"))

(defun colorize-user ()
  (if *colorize-user-function*
      (funcall *colorize-user-function*)))
