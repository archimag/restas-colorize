;;;; packages.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-plugin #:restas.colorize
  (:use #:cl #:iter)
  (:export #:*db*))

(in-package #:restas.colorize)

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/colorize.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:restas-colorize))))