;;;; drawer.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric finalize-page (drawer data)
  (:documentation "Finalize page"))

(defgeneric render-route-data (drawer data route )
  (:documentation "Render page for specific route"))

(defgeneric colorize (drawer code lang)
  (:documentation "Make highlight html from code")
  (:method (drawer code lang)
    (colorize::html-colorization (intern lang :keyword) code)))

(defgeneric colorize-langs (drawer)
  (:documentation "List of supported languages")
  (:method (drawer)
    (iter (for (id . title) in (colorize:coloring-types))
          (collect (list :id (symbol-name id)
                         :title title)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; default implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer () ())

(defmethod finalize-page ((drawer drawer) data)
  (restas.colorize.view:finalize-page data))


(defmethod restas:render-object ((drawer drawer) (data list))
  (let ((content (render-route-data drawer
                                    data
                                    (restas:route-symbol restas:*route*)))
        (menu (restas.colorize.view:main-menu
               (list :href-all  (restas:genurl 'list-notes)
                     :href-create (restas:genurl 'create-note)))))
    (finalize-page drawer
                   (list :content content
                         :menu menu
                         :title (getf data :title)))))


(defmethod render-route-data ((drawer drawer) (data list) route)
  (funcall (find-symbol (symbol-name route)
                        '#:restas.colorize.view)
           data))

(defmethod render-route-data ((drawer drawer) (data list) (route (eql 'view-note)))
  (call-next-method drawer
                    (list* :code (colorize drawer
                                           (getf data :code)
                                           (getf data :lang))
                           data)
                    route))

(defmethod render-route-data ((drawer drawer) (data list) (route (eql 'create-note)))
  (call-next-method drawer
                    (list* :langs (colorize-langs drawer)
                           data)
                    route))

(defmethod render-route-data ((drawer drawer) (data list) (route (eql 'preview-note)))
  (call-next-method drawer
                    (list* :langs (colorize-langs drawer)
                           :preview (colorize drawer
                                              (getf data :code)
                                              (getf data :lang))
                           data)
                    route))

(setf *default-render-method* (make-instance 'drawer))
