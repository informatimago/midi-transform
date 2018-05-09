;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi.transform.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Transforms MIDI CC into DW-8000 parameter-changes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-03 <PJB> Created.
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

(setf *readtable* (copy-readtable nil))

(asdf:defsystem "com.informatimago.midi.transform"
  :description "Transforms MIDI CC into DW-8000 parameter-changes."
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on ("cffi"
               "midi"
               "trivial-timers"
               "trivial-main-thread"
               "com.informatimago.macosx.coremidi"
               "com.informatimago.macosx.coremidi.midi"
               "com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.interactive")
  :components ((:file "synthesizer")
               (:file "midi-application")
               (:file "parameter-map-compiler" :depends-on ("synthesizer"))
               (:file "korg"                   :depends-on ("synthesizer"))
               (:file "korg-dw-8000"           :depends-on ("synthesizer"
                                                            "korg"
                                                            "midi-application"
                                                            "parameter-map-compiler"))
               (:file "korg-dss-1"             :depends-on ("synthesizer"
                                                            "korg"))
               (:file "convert-cc-dw-8000"     :depends-on ("midi-application"
                                                            "parameter-map-compiler"
                                                            "korg-dw-8000"
                                                            "korg-dss-1")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

;;;; THE END ;;;;
