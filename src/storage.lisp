;;;; storage.lisp
;;;;
;;;; This file is part of the restas-colorize library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.colorize)

;;;; generic interface

(defgeneric count-all-pastes (storage))

(defgeneric list-pastes (storage offset limit))

(defgeneric get-paste (storage id))

(defgeneric add-paste (storage paste))

(defgeneric remove-paste (storage id))

;;;; storage in memory

(defclass memory-storage ()
  ((pastes :initform nil)
   (last-id :initform 0)))

(defmethod list-pastes ((storage memory-storage) offset limit)
  (let* ((pastes (slot-value storage 'pastes))
         (len (length pastes))
         (end (+ limit offset)))
    (if (> len offset)
        (subseq pastes
                offset
                (if (and pastes (< end len))
                    end)))))

(defmethod count-all-pastes ((storage memory-storage))
  (length (slot-value storage 'pastes)))

(defmethod get-paste ((storage memory-storage) id)
  (find id
        (slot-value storage 'pastes)
        :key #'(lambda (paste) (getf paste :id))))

(defmethod add-paste ((storage memory-storage) paste)
  (let ((p (list* :id (incf (slot-value storage 'last-id))
                  :date (get-universal-time)
                  paste)))
    (setf (slot-value storage 'pastes)
          (nconc (slot-value storage 'pastes)
                 (list p)))
    p))

(defmethod remove-paste (storage id)
  (setf (slot-value storage 'pastes)
        (remove id
                (slot-value storage 'pastes)
                :key #'(lambda (paste) (getf paste :id)))))
