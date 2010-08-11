;;;; storage.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

(defclass note ()
  ((id :initarg :id :initform nil :accessor note-id)
   (date :initarg :date :initform nil :accessor note-date)
   (author :initarg :author :initform nil :accessor note-author)
   (title :initarg :title :initform nil :accessor note-title)
   (lang :initarg :lang :initform nil :accessor note-lang)
   (code :initarg :code :initform nil :accessor note-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; implementation storage in memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defclass memory-storage ()
;;   ((notes :initform nil)
;;    (last-id :initform 0)))

;; (defmethod storage-count-notes ((storage memory-storage))
;;   (length (slot-value storage 'notes)))

;; (defmethod storage-list-notes ((storage memory-storage) offset limit)
;;   (let* ((notes (slot-value storage 'notes))
;;          (len (length notes))
;;          (end (+ limit offset)))
;;     (if (and (not (minusp offset))
;;              (> len offset))
;;         (subseq notes
;;                 offset
;;                 (if (and notes (< end len))
;;                     end)))))

;; (defmethod storage-get-note ((storage memory-storage) id)
;;   (find id
;;         (slot-value storage 'notes)
;;         :key #'note-id))

;; (defmethod storage-add-note ((storage memory-storage) note)
;;   (setf (slot-value note 'id)
;;         (incf (slot-value storage 'last-id)))
;;   (setf (slot-value note 'date)
;;         (local-time:now))
;;   (push note
;;         (slot-value storage 'notes))
;;   note)

;; (defmethod storage-remove-note (storage id)
;;   (setf (slot-value storage 'notes)
;;         (remove id
;;                 (slot-value storage 'notes)
;;                 :key #'(lambda (note) (getf note :id)))))

;; ;;;; set default value of *storage*

;; (setf *storage* (make-instance 'memory-storage))