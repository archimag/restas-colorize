;;;; colorize.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

(defvar *max-on-page* 10)

(defvar *finalize-page* 'restas.colorize.view:default-standalone-frame)

(defun finalize-page (content title)
  (funcall *finalize-page* (list :title title
                                 :content content
                                 :css (list (restas:genurl 'colorize.css)
                                            (restas:genurl 'style.css)))))

(defvar *colorize.css* (concatenate 'string
                           colorize:*coloring-css*
                           ".code {
    background-color: #eee;
    border: 1px solid #d0d0d0;
    margin: 0.2em;
    padding: 0.5em;
    font-family: \"Courier New\", monospace;
    font-size: 90%;
}
"))


(defvar *storage* (make-instance 'memory-storage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paste-html (paste)
  (colorize::html-colorization (or (find-symbol (getf paste :lang) :keyword)
                                   (error "Unknow coloring type: ~A" lang))
                               (getf paste :code)))

(restas:define-route main ("")
  (restas:redirect 'all))

(restas:define-route colorize.css ("colorize.css"
                          :content-type "text/css")
  *colorize.css*)


(restas:define-route all ("all")
  (finalize-page 
   (restas.colorize.view:show-list-pastes 
    (list :pastes (iter (for paste in (list-pastes *storage*
                                                   0
                                                   *max-on-page*))
                        (collect (list* :href (restas:genurl 'view-paste :id (getf paste :id))                                        
                                        paste)))
          :first 0
          :total-count (count-all-pastes *storage*)))
   "All pastes"))

(restas:define-route view-paste (":id")
  (let ((paste (get-paste *storage*
                          (parse-integer id))))
    (finalize-page
     (restas.colorize.view:show-one-paste (list* :code (paste-html paste)
                                                 paste))
     (getf paste :title))))

;;;; create

(defun langs-plist ()
  (iter (for (id . title) in (colorize:coloring-types))
        (collect (list :id (symbol-name id)
                       :title title))))

(restas:define-route create ("create")
  (finalize-page
   (restas.colorize.view:create-paste-form (list :langs (langs-plist)))
   "Создать"))

(restas:define-route create/preview ("create"
                                     :method :post
                                     :requirement #'(lambda () (hunchentoot:post-parameter "preview")))
  (let ((code (hunchentoot:post-parameter "code"))
        (lang (hunchentoot:post-parameter "lang")))
  (finalize-page
   (restas.colorize.view:create-paste-form
    (list :langs (langs-plist)
          :preview (colorize::html-colorization (or (find-symbol lang :keyword)
                                                    (error "Unknow coloring type: ~A" lang))
                                                code)
          :code code
          :lang lang))
   "Предпросмотр")))

(restas:define-route create/save ("create"
                                  :method :post
                                  :requirement #'(lambda () (hunchentoot:post-parameter "save")))
  (let ((paste (add-paste *storage*
                          (list :code (hunchentoot:post-parameter "code")
                                :lang (hunchentoot:post-parameter "lang")
                                :title (hunchentoot:post-parameter "title")))))
    (restas:redirect 'view-paste
                     :id (getf paste :id))))
