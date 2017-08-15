;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dw8000.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines sysex messages of the Korg DW-8000 / EX-8000.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-08 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.MIDI.KORG.DW-8000"
  (:use "COMMON-LISP"
        "MIDI"
        "COM.INFORMATIMAGO.MIDI.PARAMETER")
  (:export
   "DW8000-PARAMETER"
   "PARAMETERS"
   "CHANNEL"
   "PROGRAM-NUMBER"
   "PARAMETER-OFFSET"
   "PARAMETER-VALUE"
   "+SYSEX+"
   "+EOX+"
   "+KORG-ID+"
   "+KORG-DW-8000+"
   "+KORG-EX-8000+"
   "+DEVICE-ID+"
   "+GENERAL-REQUEST+"
   "+DEVICE-ID-REQUEST+"
   "+DATA-SAVE-REQUEST+"
   "+DATA-DUMP+"
   "+WRITE-REQUEST+"
   "+WRITE-COMPLETED-STATUS+"
   "+WRITE-ERROR-STATUS+"
   "+PARAMETER-CHANGE-REQUEST+"
   "DEVICE-ID"
   "DATA-DUMP"
   "WRITE-COMPLETED-STATUS"
   "WRITE-ERROR-STATUS"
   "PARSE-SYSTEM-EXCLUSIVE-MESSAGE"
   "SYSEX"
   "DEVICE-ID-REQUEST"
   "DATA-DUMP-REQUEST"
   "WRITE-REQUEST"
   "PARAMETER-CHANGE-REQUEST"
   "DATA-DUMP"
   "PARAMETER-NAME"
   "PARAMETER-OFFSET"
   "PARAMETER-MIN"
   "PARAMETER-MAX"
   "PARAMETER-VALUES"
   "FIND-PARAMETER"
   "OSC1-OCTAVE"
   "OSC1-WAVEFORM"
   "OSC1-LEVEL"
   "AUTO-BEND-SELECT"
   "AUTO-BEND-MODE"
   "AUTO-BEND-TIME"
   "AUTO-BEND-INTENSITY"
   "OSC2-OCTAVE"
   "OSC2-WAVEFORM"
   "OSC2-LEVEL"
   "OSC2-INTERVAL"
   "OSC2-DETUNE"
   "NOISE-LEVEL"
   "ASSIGN-MODE"
   "PARAMETER-NO-MEMORY"
   "CUTOFF"
   "RESONANCE"
   "KEYBOARD-TRACK"
   "POLARITY"
   "VCF-EG-INTENSITY"
   "VCF-ATTACK"
   "VCF-DECAY"
   "VCF-BREAK-POINT"
   "VCF-SLOPE"
   "VCF-SUSTAIN"
   "VCF-RELEASE"
   "VCF-VELOCITY-SENSITIVITY"
   "VCA-ATTACK"
   "VCA-DECAY"
   "VCA-BREAK-POINT"
   "VCA-SLOPE"
   "VCA-SUSTAIN"
   "VCA-RELEASE"
   "VCA-VELOCITY-SENSITIVITY"
   "MG-WAVE-FORM"
   "MG-FREQUENCY"
   "MG-DELAY"
   "MG-OSC"
   "MG-VCF"
   "BEND-OSC"
   "BEND-VCF"
   "DELAY-TIME"
   "DELAY-FACTOR"
   "DELAY-FEEDBACK"
   "DELAY-FREQUENCY"
   "DELAY-INTENSITY"
   "DELAY-EFFECT-LEVEL"
   "PORTAMENTO"
   "AFTERTOUCH-OSC-MG"
   "AFTERTOUCH-VCF"
   "AFTERTOUCH-VCA"))
(in-package "COM.INFORMATIMAGO.MIDI.KORG.DW-8000")

(deftype channel          () '(integer 0 15))
(deftype program-number   () '(integer 0 63))
(deftype parameter-offset () '(integer 0 63))
(deftype parameter-value  () '(integer 0 63))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defconstant +sysex+                        #xf0)
 (defconstant +eox+                          #xf7)

 (defconstant +korg-id+                      #x42)

 (defconstant +korg-dw-8000+                 #x03)
 (defconstant +korg-ex-8000+                 #x03)

 (defconstant +device-id+                    3)

 (defconstant +general-request+              #x30)
 (defconstant +device-id-request+            #x40)

 (defconstant +data-save-request+            #x10)
 (defconstant +data-dump+                    #x40)
 (defconstant +write-request+                #x11)
 (defconstant +write-completed-status+       #x21)
 (defconstant +write-error-status+           #x22)
 (defconstant +parameter-change-request+     #x41))

(defun device-id (channel device-id)
  `(device-id ,channel ,device-id))
(defun data-dump (channel device-id parameters)
  `(data-dump ,channel ,device-id ,parameters))
(defun write-completed-status (channel device-id)
  `(write-completed-status ,channel ,device-id))
(defun write-error-status     (channel device-id)
  `(write-error-status ,channel ,device-id))

(defun parse-system-exclusive-message (bytes)
  (let ((s 0)
        channel device-id)
    (flet ((eat (code)
             (if (= code (aref bytes s))
                 (incf s)
                 (error "Unexpected byte in sysex at position ~D, got ~2,'0X, expected ~2,'0X."
                        s (aref bytes s) code))))
      (eat +sysex+)
      (unless (= +korg-id+ (aref bytes s))
        (return-from parse-system-exclusive-message nil))
      (eat +korg-id+)
      (setf channel (ldb (byte 4 0) (aref bytes s)))
      (let ((format (ldb (byte 4 4) (aref bytes s))))
        (case format
          ((#.+device-id+)
           (incf s)
           (setf device-id (aref bytes s))
           (unless (= +korg-dw-8000+ device-id)
             (return-from parse-system-exclusive-message nil))
           (incf s)
           (if (= +eox+ (aref bytes s))
               (progn
                 (eat +eox+)
                 (device-id channel device-id))
               (case (aref bytes s)
                 ((#.+data-dump+)
                  (incf s)
                  (let ((parameters (loop
                                      :with parameters := '()
                                      :while (and (< s (- (length bytes) 2))
                                                  (< (aref bytes s) 128)
                                                  (< (aref bytes (1+ s)) 128))
                                      :for p := (aref bytes s)
                                      :for v := (aref bytes (1+ s))
                                      :for parameter := (find-parameter p)
                                      :do (unless parameter
                                            (error "Unkown parameter offset ~D in data dump." p))
                                          (if (<= (parameter-min parameter) v (parameter-max parameter))
                                              (push (list (parameter-name parameter) v) parameters)
                                              (error "Value ~D of parameter ~A is out of expected range [~D,~D] in data dump."
                                                     v
                                                     (parameter-name parameter)
                                                     (parameter-min parameter)
                                                     (parameter-max parameter)))
                                          (incf s 2)
                                      :finally (return parameters))))
                    (eat +eox+)
                    (data-dump channel device-id parameters)))
                 ((#.+write-completed-status+)
                  (write-completed-status channel device-id))
                 ((#.+write-error-status+)
                  (write-error-status     channel device-id))
                 (otherwise
                  (error "Unexpected sysex from DW-8000/EX-8000.")))))

          (otherwise
           (error "Unexpected format code in sysex at position ~D, got ~1,'0X."
                  s format)))))))


(defmacro sysex (&body expressions)
  (let ((i -1)
        (vvar (gensym)))
    `(let ((,vvar (make-array ,(+ 2 (length expressions)) :element-type '(unsigned-byte 8))))
       (setf (aref ,vvar ,(incf i)) +sysex+)
       (setf ,@(mapcan (lambda (e) `((aref ,vvar ,(incf i)) ,e))
                       expressions))
       (setf (aref ,vvar ,(incf i)) +eox+)
       ,vvar)))

(defun device-id-request (channel)
  (check-type channel channel)
  (sysex
    +korg-id+
    (logior +device-id-request+ channel)))

(defun data-dump-request (channel)
  (check-type channel channel)
  (sysex
    +korg-id+
    (logior +general-request+ channel)
    +korg-dw-8000+
    +data-save-request+))

(defun write-request (channel program-number)
  (check-type channel channel)
  (check-type program-number program-number)
  (sysex
    +korg-id+
    (logior +general-request+ channel)
    +korg-dw-8000+
    +write-request+
    program-number))

(defun parameter-change-request (channel parameter-offset parameter-value)
  (check-type channel           channel)
  (check-type parameter-offset  (or symbol parameter-offset))
  (check-type parameter-value   parameter-value)
  (let ((parameter (find-parameter parameter-offset)))
    (assert (<= (parameter-min parameter) parameter-value (parameter-max parameter))
            (parameter-value)
            "Invalid value ~D for parameter ~A, expected range [~D,~D]"
            parameter-value
            (parameter-name parameter)
            (parameter-min parameter)
            (parameter-max parameter))
    (sysex
      +korg-id+
      (logior +general-request+ channel)
      +korg-dw-8000+
      +parameter-change-request+
      (parameter-offset parameter)
      parameter-value)))

(defun data-dump (channel parameters)
  (check-type channel channel)
  (let ((parameters (mapcar (lambda (parameter)
                              (check-type parameter list)
                              (destructuring-bind (name-or-offset value) parameter
                                (let ((parameter (find-parameter name-or-offset)))
                                  (if parameter
                                      (if (<= (parameter-min parameter) value (parameter-max parameter))
                                          (list (parameter-offset parameter) value)
                                          (error "Invalid value ~D for parameter ~A, expected range [~D,~D]"
                                                 value
                                                 (parameter-name parameter)
                                                 (parameter-min parameter)
                                                 (parameter-max parameter)))
                                      (error "No such parameter ~A" name-or-offset)))))
                            parameters)))
    (let ((v (make-array (+ 5 (* 2 (length parameters)))
                         :element-type '(unsigned-byte 8)))
          (i -1))
      (setf (aref v (incf i)) +sysex+
            (aref v (incf i)) +korg-id+
            (aref v (incf i)) (logior +general-request+ channel)
            (aref v (incf i)) +korg-dw-8000+)
      (loop
        :for (off val) :in (sort parameters (function <) :key (function first))
        :do (setf (aref v (incf i)) off
                  (aref v (incf i)) val))
      (setf (aref v (incf i)) +eox+)
      v)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun scat (&rest sds)
    (intern (apply (function concatenate) 'string (mapcar (function string) sds))))
  (defvar *parameters* (make-hash-table :test (function eql))))


(defclass dw8000-parameter (parameter)
  ((offset :initarg :offset :reader parameter-offset)))

(defmethod print-object ((self dw8000-parameter) stream)
  (format stream "~S" (list 'dw8000-parameter
                            (parameter-name self)
                            (parameter-offset self)
                            (parameter-min self)
                            (parameter-max self)
                            (parameter-values self)))
  self)


(defmacro define-parameter (name offset (min max) &optional values)
  `(progn
     (setf (gethash ',name *parameters*)
           (setf (gethash ,offset *parameters*)
                 (make-instance 'dw8000-parameter :name ',name
                                                  :offset ,offset
                                                  :min ,min
                                                  :max ,max
                                                  :values ',values)))
     (defconstant ,(scat '+ name '+) ,offset)
     (deftype ,name () '(integer ,min ,max))
     ',name))


(defun find-parameter (offset) (gethash offset *parameters*))

(define-parameter osc1-octave                0 (0 3)     ("16'" "8'" "4'" inhibit))
(define-parameter osc1-waveform              1 (0 15)
  (brass-string
   clarinet
   acoustic-piano
   electric-piano
   electric-piano-hard
   clavi
   organ
   brass
   saxophone
   violin
   acoustic-guitar
   guitar
   electric-bass
   digital-bass
   bell
   whistle))
(define-parameter osc1-level                 2 (0 31))
(define-parameter auto-bend-select           3 (0 3)     (off osc1 osc2 both))
(define-parameter auto-bend-mode             4 (0 1)     (up down))
(define-parameter auto-bend-time             5 (0 31))
(define-parameter auto-bend-intensity        6 (0 31))
(define-parameter osc2-octave                7 (0 3)     ("16'" "8'" "4'" inhibit))
(define-parameter osc2-waveform              8 (0 15)
  (brass-string
   clarinet
   acoustic-piano
   electric-piano
   electric-piano-hard
   clavi
   organ
   brass
   saxophone
   violin
   acoustic-guitar
   guitar
   electric-bass
   digital-bass
   bell
   whistle))
(define-parameter osc2-level                 9 (0 31))
(define-parameter osc2-interval             10 (0 7)    (1 -3 3 4 5 inhibit inhibit inhibit))
(define-parameter osc2-detune               11 (0 7)    (0 1 2 3 4 5 6 inhibit))
(define-parameter noise-level               12 (0 31))

(define-parameter assign-mode               13 (0 3)    (poly1 poly2 unison1 unison2))
(define-parameter parameter-no-memory       14 (0 63))

(define-parameter cutoff                    15 (0 63))
(define-parameter resonance                 16 (0 31))
(define-parameter keyboard-track            17 (0 3)    ("0" "1/4" "1/2" "1"))
(define-parameter polarity                  18 (0 1)    (positive negative))

(define-parameter vcf-eg-intensity          19 (0 31))
(define-parameter vcf-attack                20 (0 31))
(define-parameter vcf-decay                 21 (0 31))
(define-parameter vcf-break-point           22 (0 31))
(define-parameter vcf-slope                 23 (0 31))
(define-parameter vcf-sustain               24 (0 31))
(define-parameter vcf-release               25 (0 31))
(define-parameter vcf-velocity-sensitivity  26 (0 7))

(define-parameter vca-attack                27 (0 31))
(define-parameter vca-decay                 28 (0 31))
(define-parameter vca-break-point           29 (0 31))
(define-parameter vca-slope                 30 (0 31))
(define-parameter vca-sustain               31 (0 31))
(define-parameter vca-release               32 (0 31))
(define-parameter vca-velocity-sensitivity  33 (0 7))

(define-parameter mg-wave-form              34 (0 3)    (triangle saw ramp square))
(define-parameter mg-frequency              35 (0 31))
(define-parameter mg-delay                  36 (0 31))
(define-parameter mg-osc                    37 (0 31))
(define-parameter mg-vcf                    38 (0 31))

(define-parameter bend-osc                  39 (0 15) (0 1 2 3 4 5 6 7 8 9 10 11 12 inhibit inhibit inhibit))
(define-parameter bend-vcf                  40 (0 1)  (off on))
(define-parameter delay-time                41 (0 7))
(define-parameter delay-factor              42 (0 15))
(define-parameter delay-feedback            43 (0 15))
(define-parameter delay-frequency           44 (0 31))
(define-parameter delay-intensity           45 (0 31))
(define-parameter delay-effect-level        46 (0 15))
(define-parameter portamento                47 (0 31))
(define-parameter aftertouch-osc-mg         48 (0 3))
(define-parameter aftertouch-vcf            49 (0 3))
(define-parameter aftertouch-vca            50 (0 3))

(defun parameters ()
  (let ((result '()))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v result))
             *parameters*)
    (delete-duplicates
     (sort result (function <) :key (function parameter-offset)))))

#|

(device-id-request 10)
#(240 66 74 247)

(data-dump-request 10)
#(240 66 58 3 16 247)

(write-request 10 0)
#(240 66 58 3 17 0 247)

(parameter-change-request 10 +cutoff+ 32)
#(240 66 58 3 65 15 32 247)

(data-dump 10
            '((osc1-octave 3)
              (osc1-waveform 15)
              (osc1-level 31)
              (auto-bend-select 3)
              (auto-bend-mode 1)
              (auto-bend-time 31)
              (auto-bend-intensity 31)
              (osc2-octave 3)
              (osc2-waveform 15)
              (osc2-level 31)
              (osc2-interval 7)
              (osc2-detune 7)
              (noise-level 31)
              (assign-mode 3)
              (parameter-no-memory 63)
              (cutoff 63)
              (resonance 31)
              (keyboard-track 3)
              (polarity 1)
              (vcf-eg-intensity 31)
              (vcf-attack 31)
              (vcf-decay 31)
              (vcf-break-point 31)
              (vcf-slope 31)
              (vcf-sustain 31)
              (vcf-release 31)
              (vcf-velocity-sensitivity 7)
              (vca-attack 31)
              (vca-decay 31)
              (vca-break-point 31)
              (vca-slope 31)
              (vca-sustain 31)
              (vca-release 31)
              (vca-velocity-sensitivity 7)
              (mg-wave-form 3)
              (mg-frequency 31)
              (mg-delay 31)
              (mg-osc 31)
              (mg-vcf 31)
              (bend-osc 15)
              (bend-vcf 1)
              (delay-time 7)
              (delay-factor 15)
              (delay-feedback 15)
              (delay-frequency 31)
              (delay-intensity 31)
              (delay-effect-level 15)
              (portamento 31)
              (aftertouch-osc-mg 3)
              (aftertouch-vcf 3)
              (aftertouch-vca 3)))
|#

;;;; THE END ;;;;
