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
           #:count-all-pastes
           #:list-pastes
           #:get-paste
           #:add-paste
           #:remove-paste))

(in-package #:restas.colorize)

(defparameter *resource-dir*
  (asdf:component-pathname (asdf:find-system '#:restas-colorize)))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/colorize.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:restas-colorize))))