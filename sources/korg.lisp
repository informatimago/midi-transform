;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               korg.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Gathers definitions common to all Korg synthesizers.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-15 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.MIDI.KORG"
  (:use "COMMON-LISP"
        "MIDI"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER")
  (:export "MIDI-DATA"
           "CHANNEL"
           "+SYSEX+"
           "+EOX+"
           "+KORG-ID+"
           "+KORG-DW-8000+"
           "+KORG-EX-8000+"
           "+KORG-DSS-1+"
           "+KORG-DSM-1+"
           "+DEVICE-ID+"
           "+GENERAL-REQUEST+"
           "+DEVICE-ID-REQUEST+"
           "+PROGRAM-PARAMETER-REQUEST+"
           "+PROGRAM-WRITE-REQUEST+"
           "+MODE-REQUEST+"
           "+PCM-DATA-REQUEST+"
           "+MULTISOUND-PARAMETER-REQUEST+"
           "+MULTISOUND-LIST-REQUEST+"
           "+COMBINATION-PROGRAM-LIST-REQUEST+"
           "+COMBINATION-PARAMETER-REQUEST+"
           "+COMBINATION-WRITE-REQUEST+"
           "+DATA-PROCESSING-REQUEST+"
           "+WRITE-COMPLETED-STATUS+"
           "+WRITE-ERROR-STATUS+"
           "+DATA-LOAD-COMPLETED+"
           "+DATA-LOAD-ERROR+"
           "+DATA-PROCESSING-COMPLETED+"
           "+PROGRAM-PARAMETER-DUMP+"
           "+PROGRAM-PARAMETER-CHANGE+"
           "+MODE-DATA+"
           "+PCM-DATA-DUMP+"
           "+MULTISOUND-PARAMETER-DUMP+"
           "+MULTISOUND-LIST+"
           "+COMBINATION-PROGRAM-LIST+"
           "+COMBINATION-PARAMETER-DUMP+"
           "DEVICE-ID-REQUEST"
           "SYSEX"
           "WRITE-DATA-BYTE-TO-SYSEX"
           "WRITE-STRING-TO-SYSEX"
           "WRITE-INTEGER-TO-SYSEX"
           "WRITE-PCM-SAMPLE-TO-SYSEX"
           "SYSEX-BUFFER"))
(in-package "COM.INFORMATIMAGO.MIDI.KORG")

(deftype midi-data        () '(integer 0 127))
(deftype channel          () '(integer 0 15))


(defconstant +sysex+                             #xf0)
(defconstant +eox+                               #xf7)

(defconstant +korg-id+                           #x42)

(defconstant +korg-dw-8000+                      #x03)
(defconstant +korg-ex-8000+                      #x03)
(defconstant +korg-dss-1+                        #x15)
(defconstant +korg-dsm-1+                        #x15)


(defconstant +device-id+                         3)

(defconstant +general-request+                   #x30)
(defconstant +device-id-request+                 #x40)


(defconstant +program-parameter-request+         #x10) ; dw-8000[r.]  dss-1[r.]  ; called data-dump-request in dw-8000
(defconstant +program-write-request+             #x11) ; dw-8000[r.]             ; called write-request in dw-8000
(defconstant +mode-request+                      #x12) ;              dss-1[r.]
(defconstant +pcm-data-request+                  #x14) ;              dss-1[r.]
(defconstant +multisound-parameter-request+      #x15) ;              dss-1[r.]
(defconstant +multisound-list-request+           #x16) ;              dss-1[r.]
(defconstant +combination-program-list-request+  #x17) ;              dss-1[r.]
(defconstant +combination-parameter-request+     #x19) ;              dss-1[r.]
(defconstant +combination-write-request+         #x1A) ;              dss-1[r.]
(defconstant +data-processing-request+           #x1E) ;              dss-1[r.]


(defconstant +write-completed-status+            #x21) ; dw-8000[.t]  dss-1[.t]
(defconstant +write-error-status+                #x22) ; dw-8000[.t]  dss-1[.t]
(defconstant +data-load-completed+               #x23) ;              dss-1[.t]
(defconstant +data-load-error+                   #x24) ;              dss-1[.t]
(defconstant +data-processing-completed+         #x25) ;              dss-1[.t]


(defconstant +program-parameter-dump+            #x40) ; dw-8000[rt]  dss-1[rt] ; called data-dump in dw-8000
(defconstant +program-parameter-change+          #x41) ; dw-8000[rt]  dss-1[rt] ; called parameter-change in dw-8000
(defconstant +mode-data+                         #x42) ;              dss-1[rt]
(defconstant +pcm-data-dump+                     #x43) ;              dss-1[rt]
(defconstant +multisound-parameter-dump+         #x44) ;              dss-1[rt]
(defconstant +multisound-list+                   #x45) ;              dss-1[rt]
(defconstant +combination-program-list+          #x46) ;              dss-1[.t]
(defconstant +combination-parameter-dump+        #x49) ;              dss-1[rt]


(defun korg-char-code (ch)
  (let ((code (char-code ch)))
    (if (or (find code #(#x22 #x2a #x3f))
            (< code #x20)
            (< #x7f code))
        #x21 ; #\!
        code)))


(defmacro sysex (&body expressions)
  (let ((i -1)
        (vvar (gensym)))
    `(let ((,vvar (make-array ,(+ 2 (length expressions)) :element-type '(unsigned-byte 8))))
       (labels ((write-data-byte-to-sysex (data-byte)
                  (check-type data-byte midi-data)
                  (setf (aref ,vvar ,(incf i)) data-byte))
                (write-string-to-sysex (string length)
                  (loop :for ch :across (subseq (format nil "~VA" length string) 0 length)
                        :for code := (korg-char-code ch)
                        :do (write-data-byte-to-sysex code)))
                (write-integer-to-sysex (value length)
                  (loop
                    :repeat length
                    :for p :from 0 :by 7
                    :do (write-data-byte-to-sysex (ldb (byte 7 p) value))))
                (write-pcm-sample-to-sysex (value)
                  (write-data-byte-to-sysex (ash (ldb (byte 5 0) value) 2))
                  (write-data-byte-to-sysex (ldb (byte 7 5) value)))
                (sysex-buffer () ,vvar))
         (declare (inline write-data-byte-to-sysex
                          write-string-to-sysex
                          write-integer-to-sysex
                          write-pcm-sample-to-sysex))
         (setf (aref ,vvar ,(incf i)) +sysex+)
         ,@(loop
             :while expressions
             :do (let ((e (pop expressions)))
                   (if (eq e :eval)
                       (progn
                         (unless expressions
                           (error "Missing lisp expressions after :eval in SYSEX form."))
                         (pop expressions))
                       `(setf (aref ,vvar ,(incf i)) ,e))))
         (setf (aref ,vvar ,(incf i)) +eox+)
         ,vvar))))

(defun device-id-request (channel)
  (check-type channel channel)
  (sysex
    +korg-id+
    (logior +device-id-request+ channel)))


;;;; THE END ;;;;
