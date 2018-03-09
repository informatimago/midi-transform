;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               synthesizer.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Abstract synthesizer, program and parameter.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-16 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE")
  (:export
   "PARAMETER"
   "PARAMETER-MAX"
   "PARAMETER-MIN"
   "PARAMETER-NAME"
   "PARAMETER-VALUES"
   "PROGRAM"
   "PROGRAM-NAME"
   "PROGRAM-PARAMETERS"
   "PROGRAM-VALUES"
   "PROGRAM-PARAMETERS"
   "PROGRAM-PARAMETER-BY-NAME"
   "PROGRAM-VALUE-FOR-PARAMETER"
   "SYNTHESIZER"
   "SYNTHESIZER-NAME"
   "SYNTHESIZER-CHANNEL"
   "SYNTHESIZER-SOURCE"
   "SYNTHESIZER-DESTINATION"
   "SYNTHESIZER-CURRENT-PROGRAM"
   "GET-CURRENT-PROGRAM"
   "UPDATE-PARAMETER"
   "SYNTHESIZER-QUEUE"))
(in-package "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER")


;;;---------------------------------------------------------------------

(defgeneric parameter-name         (parameter))
(defgeneric parameter-min          (parameter))
(defgeneric parameter-max          (parameter))
(defgeneric parameter-values       (parameter))

(defclass parameter ()
  ((name    :initarg :name   :reader parameter-name)
   (min     :initarg :min    :reader parameter-min)
   (max     :initarg :max    :reader parameter-max)
   (values  :initarg :values :reader parameter-values)))


;;;---------------------------------------------------------------------

(defclass program ()
  ((name           :reader program-name        :initarg :name)))

(defgeneric program-parameters (program))

(defgeneric program-parameter-by-name (program name)
  (:method ((program program) name)
    (check-type name (or string symbol))
    (find name (program-parameters program) :test (function string=))))

(defgeneric program-value-for-parameter (program parameter)
  (:method ((program program) (parameter string))
    (program-value-for-parameter program (program-parameter-by-name program parameter)))
  (:method ((program program) (parameter symbol))
    (program-value-for-parameter program (program-parameter-by-name program parameter))))


;;;---------------------------------------------------------------------

(defgeneric synthesizer-current-program (synthesizer))
(defgeneric (setf synthesizer-current-program) (new-program synthesizer))

(defgeneric get-current-program (synthesizer))


(defclass synthesizer ()
  ((name                 :initarg :name         :reader   synthesizer-name)
   (model                :initform "Vanilla"    :reader   synthesizer-model)
   (channel              :initarg :channel      :reader   synthesizer-channel)
   (source-endpoint      :initarg :source       :accessor synthesizer-source)
   (destination-endpoint :initarg :destination  :accessor synthesizer-destination)
   (current-program      :initform nil          :accessor synthesizer-current-program)
   (queue                :initform (make-queue) :reader   synthesizer-queue)))

(defgeneric update-parameter (parameter value)
  (:method ((parameter t) (value t))
    value))


;;;; THE END ;;;;
