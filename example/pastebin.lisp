;;;; pastebin.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(asdf:operate 'asdf:load-op '#:restas-colorize)

(restas:defhost #:pastebin
  (:use #:cl))

(in-package #:pastebin)

(restas:define-submodule core (#:restas.colorize))

(restas:start-site '#:pastebin :port 8080)
