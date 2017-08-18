;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi-application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the abstract midi-application class.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-18 <PJB> Extracted from convert-cc-dw8000.lisp
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

(defpackage "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION"
  (:use "COMMON-LISP"
        "MIDI"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI.MIDI")
  (:export "*MIDI-APPLICATION*"
           "MIDI-APPLICATION"
           "MIDI-CLIENT"
           "MIDI-INPUT-PORT"
           "MIDI-OUTPUT-PORT"
           "MIDI-SOURCES"
           "CREATE-MIDI-APPLICATION"
           "TERMINATE"
           "CONNECT-SOURCE"))
(in-package "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION")

(defclass midi-application ()
  ((client      :initarg :client      :accessor midi-client)
   (input-port  :initarg :input-port  :accessor midi-input-port)
   (output-port :initarg :output-port :accessor midi-output-port)
   (sources     :initform '()         :accessor midi-sources
                :documentation "Sources connected to our input-port.")))

(defgeneric synthesizer (application))

(defvar *midi-application* nil
   "The current MIDI-APPLICATION instance.")

(defun create-midi-application (class-name client-name client-notify-function port-read-function
                                &rest arguments &key &allow-other-keys)
  (let ((client (client-create client-name client-notify-function)))
    (apply (function make-instance) class-name
           :client client
           :output-port (output-port-create client (format nil "~A Out" client-name))
           :input-port  (input-port-create  client (format nil "~A In"  client-name)
                                            port-read-function)
           arguments)))

(defgeneric terminate (application)
  (:method ((self midi-application))
    (loop
      :with input-port := (midi-input-port self)
      :for (nil . source) :in (midi-sources self)
      :do (port-disconnect-source input-port source))
    (port-dispose (midi-input-port  self))  (slot-makunbound self 'input-port)
    (port-dispose (midi-output-port self))  (slot-makunbound self 'output-port)
    (client-dispose (midi-client self))     (slot-makunbound self 'client)))

(defgeneric connect-source (application source refcon)
  (:method ((self midi-application) source refcon)
    (push (cons refcon source) (midi-sources self))
    (port-connect-source (midi-input-port self) source (cffi:make-pointer refcon))
    source))


;;;; THE END ;;;;


