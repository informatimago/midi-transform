;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               parameter.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the parameter abstract class.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
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
(defpackage "COM.INFORMATIMAGO.MIDI.PARAMETER"
  (:use "COMMON-LISP")
  (:export  "PARAMETER"
            "PARAMETER-NAME"
            "PARAMETER-MIN"
            "PARAMETER-MAX"
            "PARAMETER-VALUES"
            "UPDATE-PARAMETER"))
(in-package "COM.INFORMATIMAGO.MIDI.PARAMETER")

(defgeneric parameter-name   (parameter))
(defgeneric parameter-min    (parameter))
(defgeneric parameter-max    (parameter))
(defgeneric parameter-value  (parameter))

(defgeneric update-parameter (parameter value)
  (:method ((parameter t) (value t))
    value))


(defclass parameter ()
  ((name    :initarg :name   :reader parameter-name)
   (min     :initarg :min    :reader parameter-min)
   (max     :initarg :max    :reader parameter-max)
   (values  :initarg :values :reader parameter-values)))

;;;; THE END ;;;;
