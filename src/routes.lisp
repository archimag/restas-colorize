;;;; colorize.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

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
                                  (restas:genurl 'list-pastes)
                                  (max (- start *max-on-page*) 1))))))

(restas:define-route view-paste (":id"
                                 :parse-vars (list :id #'parse-integer))
  (let* ((paste (storage-get-paste *storage* id)))
    (list* :title (paste-title paste)
           :code (paste-code paste)
           :lang (paste-lang paste)
           (paste-plist/short paste))))

(restas:define-route create-paste ("create")
  (list :title "Создать"))

(restas:define-route preview-paste ("create"
                                    :method :post
                                    :requirement #'(lambda () (hunchentoot:post-parameter "preview")))
    (list :title (hunchentoot:post-parameter "title")
          :author (colorize-user)
          :code (hunchentoot:post-parameter "code")
          :lang (hunchentoot:post-parameter "lang")))


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
