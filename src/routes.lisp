;;;; colorize.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

(defun note-plist/short (note)
  (list :href (restas:genurl 'view-note :id (note-id note))
        :date (local-time:format-timestring nil (note-date note))
        :title (note-title note)
        :author (note-author note)))

(defun note-plist (note)
  (list* :title (note-title note)
         :code (note-code note)
         :lang (note-lang note)
         (note-plist/short note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route main ("")
  (restas:redirect 'list-notes))

(restas:define-route list-notes ("all")
  (let* ((total-count (storage-count-notes *storage*))
         (start (min (max (or (ignore-errors (parse-integer (wsal:get-parameter "start")))
                              1)
                          1)
                     total-count)))
    (list :title "All notes"
          :notes (iter (for note in (storage-list-notes *storage*
                                                        (1- start)
                                                        *max-on-page*))
                       (collect (note-plist/short note)))
          :first start
          :total-count total-count
          :href-before (if (< (+ (1- start) *max-on-page*)
                              total-count)
                           (format nil
                                   "~A?start=~A"
                                   (restas:genurl 'list-notes)
                                   (+ start *max-on-page*)))
          :href-after (if (> start 1)
                          (format nil
                                  "~A?start=~A"
                                  (restas:genurl 'list-notes)
                                  (max (- start *max-on-page*) 1))))))

(restas:define-route view-note (":id"
                                :parse-vars (list :id #'parse-integer))
  (note-plist (storage-get-note *storage* id)))

(restas:define-route create-note ("create")
  (list :title "Создать"))

(restas:define-route preview-note ("create"
                                   :method :post
                                   :requirement #'(lambda () (wsal:post-parameter "preview")))
  (list :title (wsal:post-parameter "title")
        :author (colorize-user)
        :code (wsal:post-parameter "code")
        :lang (wsal:post-parameter "lang")))


(restas:define-route save-note ("create"
                                :method :post
                                :requirement #'(lambda () (wsal:post-parameter "save")))
  (let ((author (colorize-user)))
    (if author
        (restas:redirect 'view-note
                         :id (note-id (storage-add-note *storage*
                                                        (make-instance  'note
                                                                        :code (wsal:post-parameter "code")
                                                                        :author author
                                                                        :lang (wsal:post-parameter "lang")
                                                                        :title (wsal:post-parameter "title")))))
        wsal:+http-forbidden+)))
