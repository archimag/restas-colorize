;;;; colorize.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

(restas:define-default-render-method (obj)
  (closure-template.standard:xhtml-strict-frame
   (list :title (getf obj :title)
         :body (restas.colorize.view:with-main-menu
                   (list :href-all (restas:genurl 'list-pastes)
                         :href-create (restas:genurl 'create-paste)
                         :body (restas:render-object (find-package '#:restas.colorize.view)
                                                     obj)))
         :css (loop for item in '("style.css" "colorize.css")
                 collect (restas:genurl 'css :file item)))))

(defun colorize-user ()
  (if *colorize-user-function*
      (funcall *colorize-user-function*)))

(defun paste-plist/short (paste)
  (list :href (restas:genurl 'view-paste :id (paste-id paste))
        :date (local-time:format-timestring nil (paste-date paste))
        :title (paste-title paste)
        :author (paste-author paste)))

(defun langs-plist ()
  (iter (for (id . title) in (colorize:coloring-types))
        (collect (list :id (symbol-name id)
                       :title title))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route main ("")
  (restas:redirect 'list-pastes))

(restas:define-route css ("css/:file")
  (merge-pathnames (format nil "resources/css/~A" file)
                   *resource-dir*))

(restas:define-route list-pastes ("all")
  (let* ((total-count (storage-count-pastes *storage*))
         (start (min (max (or (ignore-errors (parse-integer (hunchentoot:get-parameter "start")))
                              1)
                          1)
                     total-count)))
    (list :title "All pastes"
          :pastes (iter (for paste in (storage-list-pastes *storage*
                                                   (1- start)
                                                   *max-on-page*))
                        (collect (paste-plist/short paste)))
          :first start
          :total-count total-count
          :href-before (if (< (+ (1- start) *max-on-page*)
                              total-count)
                           (format nil
                                   "~A?start=~A"
                                   (restas:genurl 'list-pastes)
                                   (+ start *max-on-page*)))
          :href-after (if (> start 1)
                          (format nil
                                  "~A?start=~A"
                                  (restas:genurl 'all)
                                  (max (- start *max-on-page*) 1))))))

(restas:define-route view-paste (":id"
                                 :parse-vars (list :id #'parse-integer))
  (let* ((paste (storage-get-paste *storage* id))
         (lang (paste-lang paste)))
    (list* :title (paste-title paste)
           :code (colorize::html-colorization (or (find-symbol lang :keyword)
                                                  (error "Unknow coloring type: ~A" lang))
                                              (paste-code paste))
           :lang (paste-lang paste)
           (paste-plist/short paste))))

(restas:define-route create-paste ("create")
  (list :langs (langs-plist)
        :title "Создать"))

(restas:define-route preview-paste ("create"
                                    :method :post
                                    :requirement #'(lambda () (hunchentoot:post-parameter "preview")))
  (let ((code (hunchentoot:post-parameter "code"))
        (lang (hunchentoot:post-parameter "lang")))
    (list :langs (langs-plist)
          :preview (colorize::html-colorization (or (find-symbol lang :keyword)
                                                    (error "Unknow coloring type: ~A" lang))
                                                code)
          :title (hunchentoot:post-parameter "title")
          :author (colorize-user)
          :code code
          :lang lang)))


(restas:define-route save-paste ("create"
                                 :method :post
                                 :requirement #'(lambda () (hunchentoot:post-parameter "save")))
  (let ((author (colorize-user)))
    (if author
        (restas:redirect 'view-paste
                         :id (paste-id (storage-add-paste *storage*
                                                  (make-instance  'paste
                                                                  :code (hunchentoot:post-parameter "code")
                                                                  :author author
                                                                  :lang (hunchentoot:post-parameter "lang")
                                                                  :title (hunchentoot:post-parameter "title")))))
        hunchentoot:+http-forbidden+)))
