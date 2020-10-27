;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dw-8000.lisp
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
        "TRIVIAL-TIMERS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION"
        "COM.INFORMATIMAGO.MIDI.KORG"
        "COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER")
  (:import-from "COM.INFORMATIMAGO.MACOSX.COREMIDI"
                "SEND-SYSEX" "SYSEX-REQUEST"
                "SEND" "CURRENT-HOST-TIME")
  (:import-from "COM.INFORMATIMAGO.MACOSX.COREMIDI.MIDI"
                "PACKET-LIST-FROM-MESSAGES")
  (:export
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
   "+PROGRAM-PARAMETER-REQUEST+"
   "+PROGRAM-PARAMETER-DUMP+"
   "+PROGRAM-WRITE-REQUEST+"
   "+WRITE-COMPLETED-STATUS+"
   "+WRITE-ERROR-STATUS+"
   "+PROGRAM-PARAMETER-CHANGE+"
   "DEVICE-ID"
   "DATA-DUMP"
   "WRITE-COMPLETED-STATUS"
   "WRITE-ERROR-STATUS"
   "PARSE-SYSTEM-EXCLUSIVE-MESSAGE"
   "SYSEX"
   "DEVICE-ID-REQUEST"
   "DATA-SAVE-REQUEST"
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
   "AFTERTOUCH-VCA"
   ;; internal parameters:
   "PAGE"
   "BANK"
   "PROGRAM-CHANGE"
   "PROGRAM-UP"
   "PROGRAM-DOWN"


   "DW-8000-PARAMETER" "PARAMETER-OFFSET"
   "DW-8000-PROGRAM"
   "PROGRAM-VALUES"
   "PROGRAM-PARAMETERS"
   "PROGRAM-VALUE-FOR-PARAMETER"

   "DW-8000-SYNTHESIZER" "GET-CURRENT-PROGRAM"
   "UPDATE-PARAMETER"
   "DW-8000-PROGRAM-PARAMETER"
   "META-PARAMETER"
   "PARAMETER-PROGRAM"
   "PARAMETER-VALUE"
   "PROGRAM-PARAMETER-VALUES"
   "SYNTHESIZER-PARAMETERS"
   "RECEIVE-SYSEX-MESSAGE"
   ))
(in-package "COM.INFORMATIMAGO.MIDI.KORG.DW-8000")

(deftype channel          () '(integer 0 15))
(deftype program-number   () '(integer 0 63))
(deftype parameter-offset () '(integer 0 63))
(deftype parameter-value  () '(integer 0 63))


(defun device-id (channel device-id)
  (print `(device-id ,channel ,device-id)))
(defun received-data-dump (channel device-id parameters)
  (print `(data-dump ,channel ,device-id ,parameters)))
(defun write-completed-status (channel device-id)
  (print `(write-completed-status ,channel ,device-id)))
(defun write-error-status     (channel device-id)
  (print `(write-error-status ,channel ,device-id)))

(defun parse-system-exclusive-message (bytes)
  (let ((s 0)
        channel device-id)
    (flet ((eat (code)
             (if (= code (aref bytes s))
                 (incf s)
                 (progn
                   (cerror "Skip until expected ~2*~2,'0X byte is found."
                           "Unexpected byte in sysex at position ~D, got ~2,'0X, expected ~2,'0X."
                           s (aref bytes s) code)
                   (loop :while (and (< s (length bytes))
                                     (/= code (aref bytes s)))
                         :do (incf s))))))
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
                 ((#.+program-parameter-dump+)
                  (incf s)
                  (let ((parameters (loop
                                      :with parameters := '()
                                      :while (and (< s (- (length bytes) 2))
                                                  (< (aref bytes s) 128)
                                                  (< (aref bytes (1+ s)) 128))
                                      :for p := (aref bytes s)
                                      :for v := (aref bytes (1+ s))
                                      :for parameter := (find-parameter p)
                                      ;; :do (if parameter
                                      ;;         (print (list p '/ (parameter-name parameter) (parameter-min parameter) '<= v '<= (parameter-max parameter)))
                                      ;;         (print `(unknown parameter ,p value ,v)))
                                      :do (if parameter
                                              (if (<= (parameter-min parameter) v (parameter-max parameter))
                                                  (push (list (parameter-offset parameter) v) parameters)
                                                  (progn
                                                    (cerror "Set parameter ~@1*~A to minimum value ~D"
                                                            "Value ~D of parameter ~A is out of expected range [~D,~D] in data dump."
                                                            v
                                                            (parameter-name parameter)
                                                            (parameter-min parameter)
                                                            (parameter-max parameter))
                                                    (push (list (parameter-offset parameter) (parameter-min parameter)) parameters)))
                                              (cerror "Ignore unknown parameter ~D"
                                                      "Unknown parameter offset ~D in data dump." p))
                                          (incf s 2)
                                      :finally (return parameters))))
                    (eat +eox+)
                    (received-data-dump channel device-id parameters)))
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

(defun data-save-request (channel)
  (check-type channel channel)
  (sysex
    +korg-id+
    (logior +general-request+ channel)
    +korg-dw-8000+
    +program-parameter-request+))


(defun write-request (channel program-number)
  (check-type channel channel)
  (check-type program-number program-number)
  (sysex
    +korg-id+
    (logior +general-request+ channel)
    +korg-dw-8000+
    +program-write-request+
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
      +program-parameter-change+
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



;;;---------------------------------------------------------------------

(defclass dw-8000-parameter (parameter)
  ((offset :initarg :offset :reader parameter-offset)))

(defmethod print-object ((self dw-8000-parameter) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~S" (list :name   (parameter-name self)
                              :offset (parameter-offset self)
                              :min    (parameter-min self)
                              :max    (parameter-max self)
                              :values (parameter-values self))))
  self)

;;;---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun scat (&rest sds)
    (intern (apply (function concatenate) 'string (mapcar (function string) sds))))
  (defvar *parameters* (make-hash-table :test (function eql))))

(defmacro define-parameter (name offset (min max) &optional values)
  `(progn
     (setf (gethash ',name *parameters*)
           (setf (gethash ,offset *parameters*)
                 (make-instance 'dw-8000-parameter :name ',name
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

;;;---------------------------------------------------------------------


(defclass dw-8000-program (program)
  ((parameters :allocation :class)
   (values     :reader program-values)))

(defgeneric program-values (program)
  (:documentation "The values of the parameters of the program."))

(defgeneric program-parameter-values (program)
  (:documentation "The dw-8000-program-parameter indirect values of the program."))

(defconstant +dw-8000-value-vector-size+ 51)

(defun make-dw-8000-parameter-vector ()
  (let ((program  (make-array +dw-8000-value-vector-size+
                              :element-type '(or null dw-8000-parameter)
                              :initial-element nil)))
    (maphash (lambda (k parameter)
               (declare (ignore k))
               (setf (aref program (parameter-offset parameter)) parameter))
             *parameters*)
    program))

(defun make-dw-8000-value-vector (parameters)
  (map-into (make-array +dw-8000-value-vector-size+
                        :element-type '(unsigned-byte 8))
            (function parameter-min) parameters))

(defmethod initialize-instance :after ((program dw-8000-program) &key &allow-other-keys)
  (setf (slot-value program 'values) (make-dw-8000-value-vector (program-parameters program))))


(defmethod program-parameters ((program dw-8000-program))
  (unless (slot-boundp program 'parameters)
    (setf (slot-value program 'parameters) (make-dw-8000-parameter-vector)))
  (slot-value program 'parameters))

(defmethod program-value-for-parameter ((program dw-8000-program) (parameter dw-8000-parameter))
  (aref (program-values program) (parameter-offset parameter)))

(defmethod (setf program-values) ((values vector) (program dw-8000-program))
  (replace (program-values program) values))

(defmethod (setf program-values) ((values cons) (program dw-8000-program))
  (loop
    :with program-values := (program-values program)
    :for (name value) :in values
    :for parameter := (find-parameter name)
    :for offset    := (when parameter (parameter-offset parameter))
    :when offset
      :do (setf (aref program-values offset) value)
    :finally (return program-values)))

;;;---------------------------------------------------------------------

(defclass dw-8000-synthesizer (synthesizer)
  ((model              :initform "Korg DW-8000 or EX-8000" :reader synthesizer-model)

   (program-parameters :reader synthesizer-program-parameters
                       :reader synthesizer-parameters)
   (parameter-page     :initform 0   :accessor synthesizer-parameter-page
                       :documentation "Internal program-parameters page.")
   (program-bank       :initform 0   :accessor synthesizer-program-bank
                       :documentation "Internal program-parameters program bank.")
   (program-bank-msb   :initform 0   :accessor synthesizer-program-bank-msb
                       :documentation "MIDI Program Bank MSB (CC #0)")
   (program-bank-lsb   :initform 0   :accessor synthesizer-program-bank-lsb
                       :documentation "MIDI Program Bank MSB (CC #20)")
   (program-number     :initform 0   :accessor synthesizer-program-number
                       :documentation "MIDI Program Change number.")

   (timer              :initform nil :accessor synthesizer-timer)
   (state              :initform nil :accessor synthesizer-state)))

(defun caller ()
  (loop :with system-packages := (list (find-package "CL") #+ccl (find-package "CCL"))
        :for frame :in #+ccl (ccl::backtrace-as-list) #-ccl '()
        :for fun := (first frame)
        :while (or (member (symbol-package fun) system-packages)
                   (member fun '(caller)))
        :finally (return frame)))

(defmethod (setf synthesizer-state) :before (new-state (synthesizer dw-8000-synthesizer))
  (format *trace-output*
          "~&Old State = ~S  ---(~A)--->  New State = ~S~%"
          (synthesizer-state synthesizer)
          (caller)
          new-state))

(defmethod initialize-instance :after ((self dw-8000-synthesizer) &key &allow-other-keys)
  (setf (synthesizer-current-program self) (make-instance 'dw-8000-program :name "Default")))


(defmethod bad-device-id ((synthesizer dw-8000-synthesizer) device-id)
  (if (eql :timeout device-id)
      (format *error-output* "Timeout on device-id request with synthesizer at channel ~A.~%"
              (synthesizer-channel synthesizer))
      (format *error-output* "Synthesizer at channel ~A is not a ~A, but has a device ID: ~D.~%"
              (synthesizer-channel synthesizer)
              (synthesizer-model synthesizer)
              device-id)))

(defmethod cannot-fetch-program-parameters ((synthesizer dw-8000-synthesizer) &optional timeout)
  (format *error-output* "~:[Error~;Timeout~] on fetching program parameters from ~A synthesizer at channel ~A"
          timeout
          (synthesizer-model synthesizer)
          (synthesizer-channel synthesizer)))

(defmethod write-error ((synthesizer dw-8000-synthesizer) &optional timeout)
  (format *error-output* "~:[Error~;Timeout~] while writing program on ~A synthesizer at channel ~A"
          timeout
          (synthesizer-model synthesizer)
          (synthesizer-channel synthesizer)))


(defmethod set-timeout ((synthesizer dw-8000-synthesizer) timeout)
  (flet ((timeout () (timeout synthesizer)))
    (let ((timer (synthesizer-timer synthesizer)))
      (unless timer
        (setf timer (make-timer (function timeout)
                                :name "DW-8000-synthesizer timer"))
        (setf (synthesizer-timer synthesizer) timer))
      (schedule-timer timer timeout :absolute-p nil))))

(defmethod cancel-timeout ((synthesizer dw-8000-synthesizer))
  (let ((timer (synthesizer-timer synthesizer)))
    (when timer
      (unschedule-timer timer))))


;;;---------------------------------------------------------------------

(defgeneric parameter-tracking (parameter))
(defgeneric (setf parameter-tracking) (new-value parameter)
  (:method (new-value (parameter t))
    new-value))

(defclass dw-8000-program-parameter ()
  ((program   :initarg  :program   :accessor parameter-program)
   (offset    :initarg  :offset    :accessor parameter-offset)
   (tracking  :initform nil        :accessor parameter-tracking)))

(defmethod parameter-name ((parameter dw-8000-program-parameter))
  (parameter-name (aref (program-parameters (parameter-program parameter))
                        (parameter-offset parameter))))

(defmethod parameter-min ((parameter null))
  0)

(defmethod parameter-min ((parameter dw-8000-program-parameter))
  (parameter-min (aref (program-parameters (parameter-program parameter))
                       (parameter-offset parameter))))

(defmethod parameter-max ((parameter dw-8000-program-parameter))
  (parameter-max (aref (program-parameters (parameter-program parameter))
                       (parameter-offset parameter))))

(defmethod parameter-value ((parameter dw-8000-program-parameter))
  (aref (program-values (parameter-program parameter))
        (parameter-offset parameter)))

(defmethod (setf parameter-value) (new-value (parameter dw-8000-program-parameter))
  (setf (aref (program-values (parameter-program parameter))
              (parameter-offset parameter))
        new-value))

(defmethod update-parameter ((parameter dw-8000-program-parameter) value)
  (let ((current-value (parameter-value parameter))
        (synthesizer   (synthesizer *midi-application*)))
    (flet ((update ()
             (setf (parameter-tracking parameter) t
                   (parameter-value parameter) value)
             (when (/= value current-value)
               (format t "~&CC: (PAGE ~A) UPDATE PARAMETER ~A TO ~A~%"
                       (synthesizer-parameter-page synthesizer)
                       (parameter-name parameter) value)
               (send-parameter-change-request synthesizer parameter value)))
           (not-tracking (next)
             (setf (parameter-tracking parameter) next)
             (format t "~&CC: (PAGE ~A) PARAMETER ~A NOT PASSED THROUGH ~A YET (KNOB AT ~A)~%"
                     (synthesizer-parameter-page synthesizer)
                     (parameter-name parameter)
                     current-value
                     value)))
      (case (parameter-tracking parameter)
        ((nil)
         ;; We need current-value ± 1 for the case where the know is
         ;; at the current value, and moves out.  In that case we want
         ;; to update immediately.
         (cond ((< value (- current-value 1))   (not-tracking :less))
               ((> value (+ current-value 1))   (not-tracking :more))
               (t                               (update))))
        ;; We won't move until we reach the current-value
        ((:less)
         (when (>= value current-value)         (update)))
        ((:more)
         (when (<= value current-value)         (update)))
        ;; Once we've already updated, we can go on.
        (t                                      (update))))))


(defmethod program-parameter-values ((program dw-8000-program))
  (map 'vector (lambda (parameter)
                 (make-instance 'dw-8000-program-parameter
                                :program program
                                :offset (parameter-offset parameter)))
    (program-parameters program)))


;;----------------------------------------------------------------------

;; send-device-id-request  -> expecting-device-id -> device-id
;; write-request -> expecting-write-status -> write-status
;; data-save-request -> expecting-data-dump -> data-dump
;; data-load-request ->|
;; parameter-change ->|
;; program-change ->|

(defmethod timeout ((synthesizer dw-8000-synthesizer))
  (case (synthesizer-state synthesizer)
    (:expecting-device-id    (bad-device-id                   synthesizer :timeout))
    (:expecting-data-dump    (cannot-fetch-program-parameters synthesizer :timeout))
    (:expecting-write-status (write-error                     synthesizer :timeout))
    (otherwise                #|spurious timeout|#))
  (setf (synthesizer-state synthesizer) nil))

(defmethod receive-sysex-message ((synthesizer dw-8000-synthesizer) message)
  (cancel-timeout synthesizer)
  (let ((parsed (handler-bind ((error (lambda (err)
                                        (let ((restart (find-restart 'continue err)))
                                          (when restart
                                            (format *error-output* "~&RS: ~A -- continued.~%" err)
                                            (invoke-restart restart))))))
                  (parse-system-exclusive-message (message-data message)))))
    (print parsed)
    ;; let's ignore the channel.
    (case (first parsed)
      ((device-id)
       (let ((device-id (third parsed)))
         (unless (eql device-id +KORG-DW-8000+)
           (bad-device-id synthesizer device-id)))
       (case (synthesizer-state synthesizer)
         ((nil :expecting-device-id) (enter-idle-state synthesizer))))
      ((data-dump)
       ;;- fill the current-program with the received parameters.
       (assert (eql 3 (third parsed)))
       (setf (program-values (synthesizer-current-program synthesizer)) (fourth parsed))
       (case (synthesizer-state synthesizer)
         ((nil :expecting-data-dump) (enter-idle-state synthesizer))))
      ((write-error-status)
       (write-error synthesizer)
       (case (synthesizer-state synthesizer)
         ((nil :expecting-write-status) (enter-idle-state synthesizer))))
      ((write-completed-status)
       (case (synthesizer-state synthesizer)
         ((nil :expecting-write-status) (enter-idle-state synthesizer))))
      (otherwise
       (case (synthesizer-state synthesizer)
         ((nil)  (enter-idle-state synthesizer)))))))

(defmethod check-state ((synthesizer dw-8000-synthesizer))
  (unless (null (synthesizer-state synthesizer))
    (if (or (null (synthesizer-timer synthesizer))
            (not (timer-scheduled-p (synthesizer-timer synthesizer))))
        (setf (synthesizer-state synthesizer) nil)
        (progn (warn "Invalid synthesizer state ~A" (synthesizer-state synthesizer))
               (setf (synthesizer-state synthesizer) nil)))))

(defmethod send-device-id-request ((synthesizer dw-8000-synthesizer))
  (check-state synthesizer)
  (send-sysex (sysex-request
               (synthesizer-destination synthesizer)
               (device-id-request (synthesizer-channel synthesizer))
               nil nil))
  (setf (synthesizer-state synthesizer) :expecting-device-id)
  (set-timeout synthesizer 5))

(defmethod send-data-save-request ((synthesizer dw-8000-synthesizer))
  (check-state synthesizer)
  (send-sysex (sysex-request
               (synthesizer-destination synthesizer)
               (data-save-request (synthesizer-channel synthesizer))
               nil nil))
  (setf (synthesizer-state synthesizer) :expecting-data-dump)
  (set-timeout synthesizer 15))

(defmethod send-write-request ((synthesizer dw-8000-synthesizer) program-number)
  (check-type program-number program-number)
  (check-state synthesizer)
  (send-sysex (sysex-request
               (synthesizer-destination synthesizer)
               (write-request (synthesizer-channel synthesizer) program-number)
               nil nil))
  (setf (synthesizer-state synthesizer) :expecting-write-status)
  (set-timeout synthesizer 15))

(defmethod send-data-load-request ((synthesizer dw-8000-synthesizer) parameters)
  (check-state synthesizer)
  (send-sysex (sysex-request
               (synthesizer-destination synthesizer)
               (data-dump (synthesizer-channel synthesizer) parameters)
               nil nil))
  (enter-idle-state synthesizer))

(defmethod send-program-change ((synthesizer dw-8000-synthesizer) bank-msb bank-lsb program-number)
  (declare (ignore bank-msb bank-lsb))
  (check-type program-number program-number)
  (check-state synthesizer)
  (send (midi-output-port *midi-application*)
        (synthesizer-destination synthesizer)
        (packet-list-from-messages
         (list (make-instance 'program-change-message :program program-number
                                                      :time (current-host-time)
                                                      :channel (synthesizer-channel synthesizer)))))
  (enter-idle-state synthesizer))

(defmethod send-parameter-change-request ((synthesizer dw-8000-synthesizer) (parameter dw-8000-program-parameter) value)
  (check-state synthesizer)
  (send-sysex (sysex-request
               (synthesizer-destination synthesizer)
               (parameter-change-request (synthesizer-channel synthesizer)
                                         (parameter-offset parameter)
                                         value)
               nil nil))
  (enter-idle-state synthesizer))

;;----------------------------------------------------------------------

(defmethod enter-idle-state ((synthesizer dw-8000-synthesizer))
  (let ((queue (synthesizer-queue synthesizer)))
    (unless (queue-empty-p queue)
      (destructuring-bind (qsyn . thunk) (queue-dequeue queue)
        (assert (eql qsyn synthesizer))
        (funcall thunk)))))

(defmethod enqueue* (synthesizer thunk)
  ;; TODO: add lock.
  (let ((queue (synthesizer-queue synthesizer)))
    (if (queue-empty-p queue)
        (funcall thunk)
        (queue-enqueue queue (list :call synthesizer thunk)))))

(defmacro enqueue (synthesizer &body body)
  `(enqueue* ,synthesizer (lambda () ,@body)))

(defmethod get-current-program ((synthesizer dw-8000-synthesizer))
  (enqueue synthesizer (send-data-save-request synthesizer)))


;;----------------------------------------------------------------------

(defclass internal-parameter (parameter)
  ((update-function :initarg :update-function :reader parameter-update-function))
  (:default-initargs :min 0 :max 1 :values '()))

(defmethod update-parameter ((parameter internal-parameter) value)
  (funcall (parameter-update-function parameter) parameter value))


(defmethod (setf synthesizer-current-program) :after (new-program (synthesizer dw-8000-synthesizer))
  (setf (slot-value synthesizer 'program-parameters)
        (concatenate
         'vector
         (program-parameter-values new-program)
         (vector

          (make-instance 'internal-parameter
                         :name 'page
                         :max 2
                         :update-function (lambda (parameter value)
                                            (declare (ignore parameter))
                                            (format t "~&    Selected control page ~A~%" value)
                                            (setf (synthesizer-parameter-page synthesizer) value)
                                            (loop :for parameter
                                                    :across (synthesizer-program-parameters synthesizer)
                                                  :do (setf (parameter-tracking parameter) nil))))
          (make-instance 'internal-parameter
                         :name 'bank
                         :max 2
                         :update-function (lambda (parameter value)
                                            (declare (ignore parameter))
                                            (format t "~&    Selected bank ~A~%" value)
                                            (setf (synthesizer-program-bank synthesizer) value)))


          (make-instance 'internal-parameter
                         :name 'program-bank-msb
                         :update-function (lambda (parameter value)
                                            (declare (ignore parameter))
                                            (format t "~&    Program bank MSB = ~A~%" value)
                                            (setf (synthesizer-program-bank-msb synthesizer) value)))
          (make-instance 'internal-parameter
                         :name 'program-bank-lsb
                         :update-function (lambda (parameter value)
                                            (declare (ignore parameter))
                                            (format t "~&    Program bank LSB = ~A~%" value)
                                            (setf (synthesizer-program-bank-lsb synthesizer) value)))

          (make-instance 'internal-parameter
                         :name 'program-change
                         :update-function (lambda (parameter value)
                                            ;; (declare (ignore parameter))
                                            (format t "~&    Program change parameter = ~A  value = ~A~%" parameter value)
                                            (setf (synthesizer-program-number synthesizer) (mod value 64))
                                            (enqueue synthesizer
                                              (send-program-change synthesizer
                                                                   (synthesizer-program-bank-msb synthesizer)
                                                                   (synthesizer-program-bank-lsb synthesizer)
                                                                   (synthesizer-program-number   synthesizer)))
                                            (enqueue synthesizer (get-current-program synthesizer))))

          (make-instance 'internal-parameter
                         :name 'program-up
                         :update-function (lambda (parameter value)
                                            (declare (ignore parameter))
                                            (format t "~&    Program up ~A~%" value)
                                            (enqueue synthesizer
                                              (setf (synthesizer-program-number synthesizer)
                                                    (mod (1+ (synthesizer-program-number synthesizer))
                                                         64))
                                              (send-program-change synthesizer
                                                                   (synthesizer-program-bank-msb synthesizer)
                                                                   (synthesizer-program-bank-lsb synthesizer)
                                                                   (synthesizer-program-number   synthesizer)))
                                            (enqueue synthesizer (get-current-program synthesizer))))

          (make-instance 'internal-parameter
                         :name 'program-down
                         :update-function (lambda (parameter value)
                                            (declare (ignore parameter))
                                            (format t "~&    Program down ~A~%" value)
                                            (enqueue synthesizer
                                              (setf (synthesizer-program-number synthesizer)
                                                    (mod (1- (synthesizer-program-number synthesizer))
                                                         64))
                                              (send-program-change synthesizer
                                                                   (synthesizer-program-bank-msb synthesizer)
                                                                   (synthesizer-program-bank-lsb synthesizer)
                                                                   (synthesizer-program-number   synthesizer)))
                                            (enqueue synthesizer (get-current-program synthesizer))))))))




;;;; THE END ;;;;
#-(and)
(block try
  (handler-bind ((error (lambda (err)
                                (format t "~&~A~%" err)
                                (let ((r (find-restart 'continue err)))
                                  (if r
                                      (invoke-restart r)
                                      (return-from try))))))
    (parse-system-exclusive-message #(240 66 58 3 64 1 0 15 3 0 0 31 0 11
                                      15 0 4 0 2 62 24 4 1 0 25 0 9 31 19
                                      17 24 0 0 20 17 20 16 18 0 0 21 0 0
                                      0 2 0 5 0 0 4 22 14 0 3 0 0 247))))
