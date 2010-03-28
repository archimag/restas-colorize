;;;; storage.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic storage interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-count-pastes (storage))

(defgeneric storage-list-pastes (storage offset limit))

(defgeneric storage-get-paste (storage id))

(defgeneric storage-add-paste (storage paste))

(defgeneric storage-remove-paste (storage id))

(defclass paste ()
  ((id :initarg :id :initform nil :accessor paste-id)
   (date :initarg :date :initform nil :accessor paste-date)
   (author :initarg :author :initform nil :accessor paste-author)
   (title :initarg :title :initform nil :accessor paste-title)
   (lang :initarg :lang :initform nil :accessor paste-lang)
   (code :initarg :code :initform nil :accessor paste-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; implementation storage in memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass memory-storage ()
  ((pastes :initform nil)
   (last-id :initform 0)))

(defmethod storage-count-pastes ((storage memory-storage))
  (length (slot-value storage 'pastes)))

(defmethod storage-list-pastes ((storage memory-storage) offset limit)
  (let* ((pastes (slot-value storage 'pastes))
         (len (length pastes))
         (end (+ limit offset)))
    (if (and (not (minusp offset))
             (> len offset))
        (subseq pastes
                offset
                (if (and pastes (< end len))
                    end)))))

(defmethod storage-get-paste ((storage memory-storage) id)
  (find id
        (slot-value storage 'pastes)
        :key #'paste-id))

(defmethod storage-add-paste ((storage memory-storage) paste)
  (setf (slot-value paste 'id)
        (incf (slot-value storage 'last-id)))
  (setf (slot-value paste 'date)
        (local-time:now))
  (push paste
        (slot-value storage 'pastes))
  paste)

(defmethod storage-remove-paste (storage id)
  (setf (slot-value storage 'pastes)
        (remove id
                (slot-value storage 'pastes)
                :key #'(lambda (paste) (getf paste :id)))))

(setf *storage*
      (make-instance 'memory-storage))