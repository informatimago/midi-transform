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
  (:export "*MIDI-APPLICATION*" "CONNECT-SOURCE"
           "CREATE-MIDI-APPLICATION" "MIDI-APPLICATION"
           "MIDI-APPLICATION" "MIDI-CLIENT" "MIDI-CLIENT"
           "MIDI-CLIENT-NAME" "MIDI-CLIENT-NOTIFY-FUNCTION"
           "MIDI-INITIALIZE" "MIDI-INPUT-PORT" "MIDI-INPUT-PORT"
           "MIDI-OUTPUT-PORT" "MIDI-OUTPUT-PORT"
           "MIDI-PORT-READ-FUNCTION" "MIDI-SOURCES" "MIDI-SOURCES"
           "TERMINATE")
  (:export "CLIENT" "INPUT-PORT" "OUTPUT-PORT" "SOURCES"
           "CLIENT-NAME" "CLIENT-NOTIFY-FUNCTION" "PORT-READ-FUNCTION"))
(in-package "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION")

(defclass midi-application ()
  ((client      :reader midi-client)
   (input-port  :reader midi-input-port)
   (output-port :reader midi-output-port)
   (sources     :accessor midi-sources
                :initform '()
                :documentation "Sources connected to our input-port.")
   (client-name            :initarg :client-name            :reader midi-client-name)
   (client-notify-function :initarg :client-notify-function :reader midi-client-notify-function)
   (port-read-function     :initarg :port-read-function     :reader midi-port-read-function)))

(defgeneric synthesizer (application))

(defgeneric midi-initialize (application)
  (:method ((application midi-application))
    (with-slots (client input-port output-port
                 client-name client-notify-function port-read-function) application
      (setf client (client-create client-name client-notify-function)
            output-port (output-port-create client (format nil "~A Out" client-name))
            input-port  (input-port-create  client (format nil "~A In"  client-name)
                                            port-read-function))
      application)))

(defun create-midi-application (class-name client-name client-notify-function port-read-function
                                &rest arguments &key &allow-other-keys)

  (let ((application (apply (function make-instance) class-name
                            :client-name client-name
                            :client-notify-function client-notify-function
                            :port-read-function port-read-function
                            arguments)))
    (midi-initialize application)
    application))

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

(defvar *midi-application* nil
  "The current MIDI-APPLICATION instance.")

;;;; THE END ;;;;
