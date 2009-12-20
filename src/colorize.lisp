;;;; colorize.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *max-on-page* 10)

(defvar *finalize-page* 'restas.colorize.view:default-standalone-frame)

(defun finalize-page (content title)
  (funcall *finalize-page* 
           (list :title title
                 :content (restas.colorize.view:show-main-menu (list :href-all (restas:genurl 'all)
                                                                     :href-create (restas:genurl 'create)
                                                                     :body content))
                 :css (loop for item in '("style.css" "colorize.css")
                         collect (restas:genurl 'css :file item)))))

(defvar *colorize-user-function*
  #'(lambda () "anonymous"))

(defun colorize-user ()
  (if *colorize-user-function*
      (funcall *colorize-user-function*)))

(defvar *storage* (make-instance 'memory-storage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route main ("")
  (restas:redirect 'all))

(restas:define-route favicon.ico ("favicon.ico")
  hunchentoot:+http-not-found+)

(restas:define-route css ("css/:file")
  (merge-pathnames (format nil "resources/css/~A" file)
                   *resource-dir*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; show list pastes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paste-plist/short (paste)
  (list :href (restas:genurl 'view-paste :id (paste-id paste))
        :date (local-time:format-http-timestring nil (paste-date paste))
        :title (paste-title paste)
        :author (paste-author paste)))

(restas:define-route all ("all")
  (let* ((total-count (count-all-pastes *storage*))
         (start (min (max (or (ignore-errors (parse-integer (hunchentoot:get-parameter "start")))
                              1)
                          1)
                     total-count)))
    (finalize-page 
     (restas.colorize.view:show-list-pastes 
      (list :pastes (iter (for paste in (list-pastes *storage*
                                                     (1- start)
                                                     *max-on-page*))
                          (collect (paste-plist/short paste)))
            :first start
            :total-count total-count
            :href-before (if (< (+ (1- start) *max-on-page*)
                                total-count)
                           (format nil
                                   "~A?start=~A"
                                   (restas:genurl 'all)
                                   (+ start *max-on-page*)))
            :href-after (if (> start 1)
                           (format nil
                                   "~A?start=~A"
                                   (restas:genurl 'all)
                                   (max (- start *max-on-page*) 1)))))
     "All pastes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; show one paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paste-html (paste)
  (let ((lang (getf paste :lang)))
    (colorize::html-colorization (or (find-symbol lang :keyword)
                                     (error "Unknow coloring type: ~A" lang))
                                 (getf paste :code))))

(defun paste-plist-with-html (paste)
  (let ((lang (paste-lang paste)))
    (list* :code (colorize::html-colorization (or (find-symbol lang :keyword)
                                                  (error "Unknow coloring type: ~A" lang))
                                              (paste-code paste))
           :lang (paste-lang paste)
           (paste-plist/short paste))))
  

(restas:define-route view-paste (":id")
  (let ((paste (get-paste *storage*
                          (parse-integer id))))
    (finalize-page
     (restas.colorize.view:show-one-paste (paste-plist-with-html paste))
     (paste-title paste))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; create paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            :title (hunchentoot:post-parameter "title")
            :author (colorize-user)
            :code code
            :lang lang))
     "Предпросмотр")))

(restas:define-route create/save ("create"
                                  :method :post
                                  :requirement #'(lambda () (hunchentoot:post-parameter "save")))
  (let ((author (colorize-user)))
    (if author
        (restas:redirect 'view-paste
                         :id (paste-id (add-paste *storage*
                                                  (make-instance  'paste
                                                                  :code (hunchentoot:post-parameter "code")
                                                                  :author author
                                                                  :lang (hunchentoot:post-parameter "lang")
                                                                  :title (hunchentoot:post-parameter "title")))))
        hunchentoot:+http-forbidden+)))
