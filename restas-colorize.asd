;;;; restas-pastebin.asd
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas-colorize
  :depends-on (#:restas #:colorize #:closure-template #:local-time)
  :components ((:module "src"
                        :components ((:file "defmodule")
                                     (:file "storage" :depends-on ("defmodule"))
                                     (:file "colorize" :depends-on ("storage"))))))