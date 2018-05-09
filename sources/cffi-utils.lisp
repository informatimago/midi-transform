;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cffi-utils.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Small CFFI tools.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-04-25 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.CFFI-UTILS"
  (:use "COMMON-LISP" "CFFI")
  (:export "*DUMP-PREFIX*" "DUMP"
           "FOREIGN-VECTOR" "FOREIGN-VECTOR-COPY-FROM" "FOREIGN-VECTOR-COPY-TO"
           "FOREIGN-NULL-TERMINATED-VECTOR-LENGTH"
           "FOREIGN-NULL-TERMINATED-VECTOR"
           "MEMCPY"))
(in-package "COM.INFORMATIMAGO.CFFI-UTILS")


(defun memcpy (destination source byte-count)
  (loop
    :repeat byte-count
    :do (setf (mem-ref destination :uchar) (mem-ref source :uchar))
        (incf-pointer destination)
        (incf-pointer source)
    :finally (return destination)))


(defun foreign-vector-copy-from (pointer ctype size vector)
  "Copies SIZE elements from the lisp VECTOR to the foreign vector of CTYPE at POINTER.
Returns the destination POINTER."
  (loop
    :for i :below size
    :do (setf (mem-aref pointer ctype i) (aref vector i))
    :finally (return pointer)))

(defun foreign-vector-copy-to (pointer ctype size vector &optional (convert (function identity)))
  "Copies SIZE elements from the foreign vector of CTYPE at POINTER to the lisp VECTOR.
Returns the destination VECTOR."
  (loop
    :for i :below size
    :do (setf (aref vector i) (funcall convert (mem-aref pointer ctype i)))
    :finally (return vector)))

(defun foreign-vector (pointer ctype ltype size)
  (foreign-vector-copy-to pointer ctype size (make-array size :element-type ltype)))

(defun foreign-null-terminated-vector-length (pointer ctype)
  (loop
    :for i :from 0
    :until (zerop (mem-aref pointer ctype i))
    :finally (return i)))

(defun foreign-null-terminated-vector (pointer ctype ltype &optional (convert (function identity)))
  (let ((len (foreign-null-terminated-vector-length pointer ctype)))
    (foreign-vector-copy-to pointer ctype len (make-array len :element-type ltype) convert)))


(defvar *dump-prefix* "")
(defun dump (pointer size &key print-characters)
  (let ((*print-circle* nil))
    (loop
      :for i :from 0 :by 16
      :while (< i size)
      :do (format t "~&~A~16,'0X: " *dump-prefix* (+ i (cffi:pointer-address pointer)))
          (loop
            :repeat 16
            :for j :from i
            :if (< j size)
              :do (format t "~2,'0X " (cffi:mem-aref pointer :uint8 j))
            :else
              :do (write-string "   "))
          (when print-characters
           (loop
             :repeat 16
             :for j :from i
             :if (< j size)
               :do  (format t "~C" (let ((code (cffi:mem-aref pointer :uint8 j)))
                                     (if (<= 32 code 126)
                                         (code-char code)
                                         #\.)))
             :else
               :do (write-string " "))))
    :finally (terpri)))


;;;; THE END ;;;;
