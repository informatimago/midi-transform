;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               convert-cc-dw-8000.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Transforms MIDI CC into DW-8000 parameter-changes.
;;;;
;;;;    See README.rst.
;;;;
;;;;    - add support for other synthesizers (eg. Korg DSS-1).
;;;;
;;;;    - add support for other controllers (eg. Korg KRONOS, or Korg MS2000/R).
;;;;
;;;;    - transform the modulation wheel into some Sysex parameter
;;;;      (eg. map it to cutoff or resonance, etc).  It doesn't sound like
;;;;      the Modulation Wheel is currently taken into account by the DW-8000.
;;;;      DSS-1
;;;;
;;;;      DW-8000/EX-8000, DSS-1/DSM-1 receive MIDI OSC Modulation,
;;;;      MIDI VCF Modulation, MIDI Volume, MIDI Pitch Bender Change,
;;;;      that could be mapped from the Modulation Wheel (transmitted
;;;;      as a MIDI CC from the VI61), or from some other control.
;;;;
;;;;      We could also map the Damper Pedal, Portamento, All Note
;;;;      OFF, from toggles or momentary controls.
;;;;
;;;;
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

(defpackage "COM.INFORMATIMAGO.MIDI.TRANSFORM"
  (:nicknames "CCDW" "CCEX")
  (:use "COMMON-LISP"
        "MIDI"
        "TRIVIAL-MAIN-THREAD"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI.MIDI"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
        "COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER"
        "COM.INFORMATIMAGO.MIDI.KORG.DW-8000")
  (:shadowing-import-from "COREMIDI" "DEVICE-ID")
  (:shadow "INITIALIZE")
  (:export "INITIALIZE" "RUN" "PRINT-MIDI-DEVICES" "MAIN"))
(in-package "COM.INFORMATIMAGO.MIDI.TRANSFORM")

(defvar *rc-filename* ".midi-transform.lisp")
(defvar *version* "1.1.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defparameter *allow-print-backtrace* t)
(defun print-backtrace (&optional (output *error-output*))
  (when *allow-print-backtrace*
   #+ccl (format output "~&~80,,,'-<~>~&~{~A~%~}~80,,,'-<~>~&"
                 (ccl::backtrace-as-list))))


(defvar *effects* '())
(defun call-effects (&rest arguments)
  (mapc (lambda (effect)
          (block effect
            (handler-bind
                ((error (lambda (err)
                          (terpri *error-output*)
                          (print-backtrace *error-output*)
                          (format *error-output* "~&EE: effect ~A error: ~A~%" effect err)
                          (setf *effects* (delete effect *effects*))
                          (return-from effect))))
              (apply effect arguments))))
        (copy-list *effects*))
  *effects*)


(defvar *midi-log* *error-output*)
(defvar *midi-application* nil)
(defvar *midi-verbose* nil)

(defun client-notify (message)
  (format *midi-log* "CN: ~A~%" message)
  (force-output *midi-log*))

(defvar *bad-source-connection-refcon* '())
(defun midi-port-read (packet-list source-connection-refcon)
  (handler-bind
      ((error (lambda (err)
                (terpri *error-output*)
                (print-backtrace)
                (format *error-output* "~&EE: ~A: ~A~%"
                        (cffi:pointer-address source-connection-refcon)
                        err)
                (force-output *error-output*)
                (return-from midi-port-read))))

    (let ((*standard-output* *midi-log*)
          ;; (source-connection-refcon (cffi:pointer-address source-connection-refcon))
          (output-list '()))

      ;; (format *error-output* "PR: source-connection-refcon = ~S ~A ~A~%"
      ;;         source-connection-refcon
      ;;         (dw-8000-refcon-p *midi-application* source-connection-refcon)
      ;;         (controller-refcon-p *midi-application* source-connection-refcon))
      ;; (finish-output *error-output*)

      (cond
        ((null *midi-application*) #| not initialized yet|#)

        ((dw-8000-refcon-p *midi-application* source-connection-refcon)

         (let ((message-list (packet-list-to-messages packet-list)))
           (call-effects :start-packet-list message-list source-connection-refcon)
           (dolist (message message-list)
             (when *midi-verbose*
               (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
                 (format t "~&RD: ~A: ~A~%" source-connection-refcon message)))
             (call-effects :message message source-connection-refcon))
           (force-output)
           (call-effects :end-packet-list message-list source-connection-refcon)))

        ((controller-refcon-p *midi-application* source-connection-refcon)

         (let ((message-list (packet-list-to-messages packet-list)))
           (dolist (message message-list)
             (when (and (typep message 'channel-message)
                        (= (message-channel message) (controller-channel *midi-application*)))
               (typecase message
                 (program-change-message
                  (let ((program  (message-program message)))
                    (format t "~&RC: ~S~%" message)
                    (format t "~&RC: ~A: PC ~A~%" source-connection-refcon program)
                    (unless (= (message-channel message)
                               (dw-8000-channel *midi-application*))
                      (setf (message-channel message) (dw-8000-channel *midi-application*)))
                    (push message output-list)))
                 (control-change-message
                  (let ((controller (message-controller message))
                        (value      (message-value      message)))
                    (when *midi-verbose*
                      (format t "~&RC: ~S~%" message)
                      (format t "~&RC: ~A: CC ~A ~A~%" source-connection-refcon controller value))
                    (if (configuringp *midi-application*)
                        (configure *midi-application* controller value)
                        (map-controller-to-sysex-request *midi-application* controller value))))
                 (t
                  (when *midi-verbose*
                    (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
                      (format t "~&RC: ~A: ~A~%" source-connection-refcon message)))
                  (unless (= (message-channel message)
                             (dw-8000-channel *midi-application*))
                    (setf (message-channel message) (dw-8000-channel *midi-application*)))
                  (push message output-list)))))))

        (t
         (unless (member source-connection-refcon *bad-source-connection-refcon*)
           (push source-connection-refcon *bad-source-connection-refcon*)
           (format *error-output* "~&RR: ~A: unexpected refcon.~%" source-connection-refcon))))

      (when output-list
        (when *midi-verbose*
          (format t "~&RO: output-list ~S~%" output-list))
        (send (midi-output-port *midi-application*)
              (dw-8000-destination *midi-application*)
              (packet-list-from-messages (nreverse output-list))))
      (when *midi-verbose*
        (force-output)))))



(defun test/send (output-port destination &key (channel 0))
  (let ((ti (current-host-time))
        (1s 1000000000))
    (flet ((in (n)
             (+ ti (* n 1s))))
      (send output-port destination
            (packet-list-from-messages
             (list  (make-instance 'midi::note-on-message :time (in 1) :status #x90 :channel channel :key 80 :velocity 70)
                    (make-instance 'midi::note-on-message :time (in 1) :status #x90 :channel channel :key 64 :velocity 70)
                    (make-instance 'midi::note-on-message :time (in 2) :status #x90 :channel channel :key 68 :velocity 40)
                    (make-instance 'midi::note-on-message :time (in 3) :status #x90 :channel channel :key 87 :velocity 80)
                    (make-instance 'midi::note-on-message :time (in 3) :status #x90 :channel channel :key 80 :velocity 80)
                    (make-instance 'midi::all-notes-off-message :time (in 5) :status #xb0 :channel channel)))))))


(defun print-midi-devices ()
  (let ((*print-circle* nil))
    (flet ((endpoint-and-connected-device (endpoint)
             (list (name endpoint)
                   (mapcar (function name) (connected-devices endpoint)))))
      (dolist (device (append (devices)
                              (external-devices)))
        (let ((entities      (device-entities device)))
          (format t "~30A ~%"
                  (name device))
          (dolist (entity entities)
            (format t "          - ~A~@[ <- ~{~S~^, ~}~]~@[ -> ~{~S~^, ~}~]~%"
                    (name entity)
                    (mapcar (function endpoint-and-connected-device)
                            (entity-sources entity))
                    (mapcar (function endpoint-and-connected-device)
                            (entity-destinations entity))))
          (terpri))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun find-endpoint-for-external-device (external-device &key (direction :source))
  "Finds a source or destination endpoint belonging to a device, that
is linked to some endpoint of this EXTERNAL-DEVICE."
  (check-type external-device device)
  (check-type direction (member :source :input :destination :output))
  (flet ((not-connected (device)
           (error "Device named ~S doesn't seem to be connected" (name device))))
    (let* ((inp (case direction
                  ((:source :input) t)
                  (otherwise        nil)))
           (entity-endpoints (if inp
                                 (function entity-sources)
                                 (function entity-destinations))))
      (with-functions (entity-endpoints)
        (first (mapcan (lambda (device)
                         (mapcan (lambda (entity)
                                   (let ((endpoints (find external-device (entity-endpoints entity)
                                                          :key (function connected-devices)
                                                          :test (function member))))
                                     (when endpoints (list endpoints))))
                                 (device-entities device)))
                       (delete-duplicates
                        (mapcan (lambda (x)
                                  (mapcan (function connected-devices)
                                          (append (entity-sources x)
                                                  (entity-destinations x))))
                                (device-entities external-device)))))))))

(defun find-endpoint-for-device (device &key (direction :source))
  "Finds a source or destination endpoint belonging to a device, that
is linked to some endpoint of this EXTERNAL-DEVICE."
  (check-type device device)
  (check-type direction (member :source :input :destination :output))
  (flet ((not-connected (device)
           (error "Device named ~S doesn't seem to be connected" (name device))))
    (let* ((inp (case direction
                  ((:source :input) t)
                  (otherwise        nil)))
           (entity-endpoints (if inp
                                 (function entity-sources)
                                 (function entity-destinations))))
      (with-functions (entity-endpoints)
        (first (entity-endpoints
                (find-if (lambda (entity)
                           (and (entity-sources      entity)
                                (entity-destinations entity)))
                         (device-entities device))))))))

(defun find-source-endpoint-for-device-named (name)
  (let ((device (find-external-device-named name)))
    (if device
        (find-endpoint-for-external-device device :direction :source)
        (let ((device (find-device-named name)))
          (when device
            (find-endpoint-for-device device :direction :source))))))

(defun find-destination-endpoint-for-device-named (name)
  (let ((device (find-external-device-named name)))
    (if device
        (find-endpoint-for-external-device device :direction :destination)
        (let ((device (find-device-named name)))
          (when device
            (find-endpoint-for-device device :direction :destination))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defclass convert-cc-dw-8000-application (midi-application)
  ((dw-8000-device-name    :reader dw-8000-device-name    :initarg :dw-8000-device-name)
   (dw-8000-channel        :reader dw-8000-channel        :initarg :dw-8000-channel)
   (dw-8000-destination    :reader dw-8000-destination)
   (dw-8000-source         :reader dw-8000-source)
   (dw-8000-refcon         :reader dw-8000-refcon         :initform (generate-refcon))
   (controller-device-name :reader controller-device-name :initarg :controller-device-name)
   (controller-channel     :reader controller-channel     :initarg :controller-channel)
   (controller-destination :reader controller-destination)
   (controller-source      :reader controller-source)
   (controller-refcon      :reader controller-refcon      :initform (generate-refcon))
   (configuring-controller :reader configuringp           :initform nil
                           :accessor configure-controller)
   (cc-map                 :reader cc-map                 :initarg :cc-map)
   (synthesizer            :reader synthesizer            :initarg :synthesizer)))


(defmethod midi-initialize ((application convert-cc-dw-8000-application))
  (call-next-method)
  (let ((synthesizer (synthesizer application)))
    (with-slots (input-port output-port
                 dw-8000-destination dw-8000-source
                 controller-destination controller-source
                 dw-8000-device-name controller-device-name
                 dw-8000-refcon controller-refcon) application
      (setf dw-8000-destination    (find-destination-endpoint-for-device-named dw-8000-device-name)
            dw-8000-source         (find-source-endpoint-for-device-named      dw-8000-device-name)
            controller-destination (find-destination-endpoint-for-device-named controller-device-name)
            controller-source      (find-source-endpoint-for-device-named      controller-device-name)
            (synthesizer-destination synthesizer)     dw-8000-destination
            (synthesizer-source      synthesizer)     dw-8000-source)

      (port-connect-source input-port dw-8000-source dw-8000-refcon)
      (port-connect-source input-port controller-source controller-refcon)
      #-(and) (progn
                (format *midi-log* "controller source       = ~60A (~A)~%" controller-source      controller-device-name)
                (format *midi-log* "controller destination  = ~60A (~A)~%" controller-destination controller-device-name)
                (format *midi-log* "device     source       = ~60A (~A)~%" dw-8000-source         dw-8000-device-name)
                (format *midi-log* "device     destination  = ~60A (~A)~%" dw-8000-destination    dw-8000-device-name))
      application)))

(defgeneric (setf controller-state) (new-state application controller)
  (:method (new-state (application convert-cc-dw-8000-application) controller)
    (check-type controller (integer 0 127))
    (send (midi-output-port application)
          (controller-destination application)
          (packet-list-from-messages
           (list (make-instance 'control-change-message :time (current-host-time)
                                                        :channel (controller-channel application)
                                                        :controller controller
                                                        :value (if new-state 127 0)))))))

(defgeneric controller-refcon-p (application refcon)
  (:method ((self convert-cc-dw-8000-application) refcon)
    (eql refcon (controller-refcon self))))

(defgeneric dw-8000-refcon-p (application refcon)
  (:method ((self convert-cc-dw-8000-application) refcon)
    (eql refcon (dw-8000-refcon self))))

(defgeneric map-controller-to-sysex-request (application controller value)
  (:method ((self convert-cc-dw-8000-application) controller value)
    (let ((map (cc-map self))
          (*midi-application* self))
      (dispatch map controller value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defgeneric default-map-cc (synthesizer))

(defmethod default-map-cc ((synthesizer dw-8000-synthesizer))
  (let ((parameters (synthesizer-parameters synthesizer)))

    ;;  (parameter-no-memory 63)             ; nope
    ;;
    ;;  20        (osc1-octave 3)                    (osc2-octave 3)                 (aftertouch-vcf 3)
    ;;  21        (osc1-waveform 15)                 (osc2-waveform 15)              (aftertouch-vca 3)
    ;;  22        (osc1-level 31)                    (osc2-level 31)                 (auto-bend-time 31)
    ;;  23        (aftertouch-osc-mg 3)              (osc2-interval 7)               (auto-bend-intensity 31)
    ;;  24        (keyboard-track 3)                 (osc2-detune 7)                 (bend-osc 15)
    ;;  25        (noise-level 31)                   (noise-level 31)                (portamento 31)
    ;;  26        (cutoff 63)                        (cutoff 63)                     (mg-frequency 31)
    ;;  27        (resonance 31)                     (resonance 31)                  (mg-delay 31)
    ;;  28        (vcf-eg-intensity 31)              (mg-wave-form 3)                (mg-osc 31)
    ;;  29        (vcf-attack 31)                    (vca-attack 31)                 (mg-vcf 31)
    ;;  30        (vcf-decay 31)                     (vca-decay 31)                  (delay-time 7)
    ;;  31        (vcf-break-point 31)               (vca-break-point 31)            (delay-factor 15)
    ;;  32        (vcf-slope 31)                     (vca-slope 31)                  (delay-feedback 15)
    ;;  33        (vcf-sustain 31)                   (vca-sustain 31)                (delay-frequency 31)
    ;;  34        (vcf-release 31)                   (vca-release 31)                (delay-intensity 31)
    ;;  35        (vcf-velocity-sensitivity 7)       (vca-velocity-sensitivity 7)    (delay-effect-level 15)
    ;;
    ;;                                       ; select           -> 3 buttons                   ; momentary 63 79 95
    ;;  (assign-mode 3) ;     poly1 poly2 unison1 unison2 -> 2 button (poly/unison 1/2)  ; toggle 62 78
    ;;  (auto-bend-select 3) ; off osc1 osc2 osc1+osc2    -> 2 button (osc1 osc2)        ; toggle 61 77
    ;;
    ;;  (auto-bend-mode 1) ;               up down        -> 1 button                      ; toggle 60
    ;;  (polarity 1) ;                     /\  \/         -> 1 button                      ; toggle 59
    ;;  (bend-vcf 1) ;                     off on         -> 1 button                      ; toggle 58
    ;;
    ;; PAGE, BANK, PROGRAM-CHANGE, PROGRAM-UP and PROGRAM-DOWN are virtual internal-parameters.
    ;; They are implemented by the SYNTHESIZER subclass.

    (compile-map
     '(

       (select bank (selection
                     ((toggle 76) 1))

         ;; pc 64-127 should not occur; instead we could map them to
         ;; MEX-800, or implement our own extended bank.

         (group
          (map (program-change  0 31)  (program-change 0 31))
          ;; program-change 32-63 should not occur, but in case:
          (map (program-change 32 63)  (program-change 32 63)))

         (group
          (map (program-change 32 63)  (program-change 0  31))
          ;; program-change 32-63 should not occur, but in case:
          (map (program-change  0 31)  (program-change 32 63))))

       (map program-up                          ((momentary 74) 1))
       (map program-down                        ((momentary 75) 1))

       (select page (selection
                     ((momentary 95) 0)
                     ((momentary 79) 1)
                     ((momentary 63) 2))

         (group
          (map (osc1-octave              0 3)     ((continuous 20) 0 127))
          (map (osc1-waveform            0 15)    ((continuous 21) 0 127))
          (map (osc1-level               0 31)    ((continuous 22) 0 127))
          (map (aftertouch-osc-mg        0 3)     ((continuous 23) 0 127))
          (map (keyboard-track           0 3)     ((continuous 24) 0 127))
          (map (noise-level              0 31)    ((continuous 25) 0 127))
          (map (cutoff                   0 63)    ((continuous 26) 0 127))
          (map (resonance                0 31)    ((continuous 27) 0 127))
          (map (vcf-eg-intensity         0 31)    ((continuous 28) 0 127))
          (map (vcf-attack               0 31)    ((continuous 29) 0 127))
          (map (vcf-decay                0 31)    ((continuous 30) 0 127))
          (map (vcf-break-point          0 31)    ((continuous 31) 0 127))
          (map (vcf-slope                0 31)    ((continuous 32) 0 127))
          (map (vcf-sustain              0 31)    ((continuous 33) 0 127))
          (map (vcf-release              0 31)    ((continuous 34) 0 127))
          (map (vcf-velocity-sensitivity 0 7)     ((continuous 35) 0 127)))

         (group
          (map (osc2-octave              0 3)     ((continuous 20) 0 127))
          (map (osc2-waveform            0 15)    ((continuous 21) 0 127))
          (map (osc2-level               0 31)    ((continuous 22) 0 127))
          (map (osc2-interval            0 7)     ((continuous 23) 0 127))
          (map (osc2-detune              0 7)     ((continuous 24) 0 127))
          (map (noise-level              0 31)    ((continuous 25) 0 127))
          (map (cutoff                   0 63)    ((continuous 26) 0 127))
          (map (resonance                0 31)    ((continuous 27) 0 127))
          (map (mg-wave-form             0 3)     ((continuous 28) 0 127))
          (map (vca-attack               0 31)    ((continuous 29) 0 127))
          (map (vca-decay                0 31)    ((continuous 30) 0 127))
          (map (vca-break-point          0 31)    ((continuous 31) 0 127))
          (map (vca-slope                0 31)    ((continuous 32) 0 127))
          (map (vca-sustain              0 31)    ((continuous 33) 0 127))
          (map (vca-release              0 31)    ((continuous 34) 0 127))
          (map (vca-velocity-sensitivity 0 7)     ((continuous 35) 0 127)))

         (group
          (map (aftertouch-vcf           0 3)     ((continuous 20) 0 127))
          (map (aftertouch-vca           0 3)     ((continuous 21) 0 127))
          (map (auto-bend-time           0 31)    ((continuous 22) 0 127))
          (map (auto-bend-intensity      0 31)    ((continuous 23) 0 127))
          (map (bend-osc                 0 15)    ((continuous 24) 0 127))
          (map (portamento               0 31)    ((continuous 25) 0 127))
          (map (mg-frequency             0 31)    ((continuous 26) 0 127))
          (map (mg-delay                 0 31)    ((continuous 27) 0 127))
          (map (mg-osc                   0 31)    ((continuous 28) 0 127))
          (map (mg-vcf                   0 31)    ((continuous 29) 0 127))
          (map (delay-time               0 7)     ((continuous 30) 0 127))
          (map (delay-factor             0 15)    ((continuous 31) 0 127))
          (map (delay-feedback           0 15)    ((continuous 32) 0 127))
          (map (delay-frequency          0 31)    ((continuous 33) 0 127))
          (map (delay-intensity          0 31)    ((continuous 34) 0 127))
          (map (delay-effect-level       0 15)    ((continuous 35) 0 127))))

       (map assign-mode                         (combination ((toggle 78) 2) ((toggle 62) 1)))
       (map auto-bend-select                    (combination ((toggle 77) 1) ((toggle 61) 2)))
       (map auto-bend-mode                      ((toggle 60) 1))
       (map polarity                            ((toggle 59) 1))
       (map bend-vcf                            ((toggle 58) 1)))

     parameters)))


(defmethod vmini-map-cc ((synthesizer dw-8000-synthesizer))
  (let ((parameters (synthesizer-parameters synthesizer)))

    (compile-map
     '(

       (map program-up                          ((momentary 15) 1))
       (map program-down                        ((momentary 16) 1))

       (select page         (selection (combination ((toggle 13) 2) ((toggle 12) 1)))

         (group
          (map (cutoff                   0 63)    ((continuous 20) 0 127))
          (map (resonance                0 31)    ((continuous 21) 0 127))
          (map (resonance                0 31)    ((continuous 22) 0 127))
          (map (vcf-eg-intensity         0 31)    ((continuous 23) 0 127)))

         (group
          (map (vca-attack               0 31)    ((continuous 20) 0 127))
          (map (vca-decay                0 31)    ((continuous 31) 0 127))
          (map (vca-sustain              0 31)    ((continuous 32) 0 127))
          (map (vca-release              0 31)    ((continuous 33) 0 127)))

         (group
          (map (mg-frequency             0 31)    ((continuous 20) 0 127))
          (map (mg-wave-form             0 3)     ((continuous 21) 0 127))
          (map (mg-osc                   0 31)    ((continuous 22) 0 127))
          (map (mg-vcf                   0 31)    ((continuous 23) 0 127)))

         (group
          (map (delay-time               0 7)     ((continuous 20) 0 127))
          (map (delay-frequency          0 31)    ((continuous 21) 0 127))
          (map (delay-intensity          0 31)    ((continuous 22) 0 127))
          (map (delay-feedback           0 15)    ((continuous 23) 0 127)))))

     parameters)))


(defun configure (midi-application controller value)
  (declare (ignore midi-application controller value))
  (warn "~S not implemented yet." 'configure))



;; Two threads:

;; 1- a REPL thread, using the terminal.

(define-symbol-macro quit          (repl-exit))
(define-symbol-macro cl-user::quit (repl-exit))

(defun convert-repl ()
  (#-ccl progn #+ccl objc:with-autorelease-pool
   #+ (and) (progn
              (format t "~&Use quit to exit.~%")
              (repl))
   #-(and) (loop
             :for command := (string-trim " "
                                          (progn (format t "> ")
                                                 (finish-output)
                                                 (read-line)))
             :do (cond
                   ((string-equal command "quit"))
                   ((string-equal command "help")
                    (format t "~&Help: ~:{~%  ~8A ~A~}~%"
                            '(("help" "Displays this help.")
                              ("quit" "Stops this midi application.")
                              ("" "otherwise, evaluate lisp expressions."))))
                   (t
                    (rep :line command)))
             :until (string-equal command "quit"))))

(defvar *repl-done* nil)
(defun run-repl-thread ()
  (bt:make-thread (lambda ()
                    (setf *repl-done* nil)
                    (unwind-protect
                         (convert-repl)
                      (setf *repl-done* t)))
                  :name "convert-repl"
                  :initial-bindings `((*standard-input*  . ,*standard-input*)
                                      (*standard-output* . ,*standard-output*)
                                      (*terminal-io*     . ,*terminal-io*))))



;; 2- the main thread, running an event loop to process midi events.

(defun run-loop-process-midi-events ()
  #+ccl (objc:with-autorelease-pool
          (#_CFRunLoopRunInMode #$kCFRunLoopDefaultMode 0.1d0 1)
          #-(and) (list #$kCFRunLoopRunFinished
                        #$kCFRunLoopRunStopped
                        #$kCFRunLoopRunTimedOut
                        #$kCFRunLoopRunHandledSource)))

(defun run-main-event-loop ()
  (loop
    :do #+swank (with-body-in-main-thread (:blocking t)
                  (run-loop-process-midi-events))
        #-swank (run-loop-process-midi-events)
    :until *repl-done*))


;; The main function.

(defun run (&key
              (dw-8000-device-name "Korg DW-8000")
              (dw-8000-channel 10)
              (controller-device-name "VI61")
              (controller-channel 10))
  (initialize)
  (let* ((synthesizer (make-instance 'dw-8000-synthesizer
                                     :name dw-8000-device-name
                                     :channel dw-8000-channel))
         (application (create-midi-application
                       'convert-cc-dw-8000-application
                       "Transform CC -- DW-8000 Parameter"
                       'client-notify 'midi-port-read
                       :dw-8000-device-name dw-8000-device-name
                       :dw-8000-channel dw-8000-channel
                       :controller-device-name controller-device-name
                       :controller-channel controller-channel
                       :synthesizer synthesizer
                       :cc-map (default-map-cc synthesizer))))
    (push (lambda (selector message source-connection-refcon)
            (declare (ignorable source-connection-refcon))
            (block effect
              (when (and (eq :message selector)
                         (typep message 'midi:system-exclusive-message))
                (handler-bind
                    ((error (lambda (err)
                              (finish-output)
                              (terpri *error-output*)
                              (print-backtrace)
                              (format *error-output* "~&EE: ~A~%" err)
                              (finish-output *error-output*)
                              (return-from effect))))
                  (receive-sysex-message synthesizer message)))))
          *effects*)
    (unwind-protect
         (progn
           (setf *midi-application* application)
           (run-repl-thread)
           (run-main-event-loop))
      (terminate *midi-application*)
      (setf *midi-application* nil))))

(defun load-rc-file ()
  (with-open-file (rc (merge-pathnames *rc-filename* (user-homedir-pathname))
                      :if-does-not-exist nil)
    (when rc
      (handler-case (load rc)
        (error (err)
          (finish-output)
          (terpri *error-output*)
          (print-backtrace)
          (format *error-output* "~%ERROR: ~A~%" err)
          (finish-output *error-output*))))))

(defvar *initialized* nil)
(defun initialize ()
  #-(and) (trace midi-initialize
                 midi-port-read
                 client-create
                 output-port-create
                 input-port-create
                 port-connect-source
                 find-destination-endpoint-for-device-named
                 find-source-endpoint-for-device-named)
  (unless *initialized*
    (#-ccl progn #+ccl objc:with-autorelease-pool
     (com.informatimago.common-lisp.interactive.interactive:initialize)
     (coreaudio-framework)
     (coremidi-framework)
     (coremidi:restart)
     (load-rc-file))
    (setf *initialized* t)))


;;----------------------------------------------------------------------
;; main
;;----------------------------------------------------------------------

(defun check-bounds (val min max title)
  (assert (<= min val max)
          (val) "~A should be between ~A and ~A"
          title min max)
  val)

(defun parse-arguments (arguments)
  (loop
    :with result := '()
    :while arguments
    :for arg := (pop arguments)
    :do (cond
          ((member arg '("-h" "--help") :test (function string=))
           (setf result (list* :help t result)))
          ((member arg '("-V" "--version") :test (function string=))
           (setf result (list* :version t result)))
          ((member arg '("-v" "--verbose") :test (function string=))
           (setf result (list* :verbose t result)))
          ((member arg '("-l" "--list-devices") :test (function string=))
           (setf result (list* :list-devices t result)))
          ((member arg '("-dd" "--dw-8000-device-name"
                         "-ed" "--ex-8000-device-name") :test (function string=))
           (let ((value (if arguments
                            (pop arguments)
                            (error "Missing a DW-8000/EX-8000 MIDI device name after ~S" arg))))
             (setf result (list* :dw-8000-device-name value result))))
          ((member arg '("-dc" "--dw-8000-channel"
                         "-ec" "--ex-8000-channel") :test (function string=))
           (let ((value (if arguments
                            (1- (check-bounds (parse-integer (pop arguments)) 1 16 "MIDI Channel"))
                            (error "Missing a MIDI controller device name after ~S" arg))))
             (setf result (list* :dw-8000-channel value result))))
          ((member arg '("-cd" "--controller-device-name") :test (function string=))
           (let ((value (if arguments
                            (pop arguments)
                            (error "Missing a MIDI controller device name after ~S" arg))))
             (setf result (list* :controller-device-name value result))))
          ((member arg '("-cc" "--controller-channel") :test (function string=))
           (let ((value (if arguments
                            (1- (check-bounds (parse-integer (pop arguments)) 1 16 "MIDI Channel"))
                            (error "Missing a MIDI controller device name after ~S" arg))))
             (setf result (list* :controller-channel value result))))
          ((member arg '("--print-backtrace-on-error") :test (function string=))
           (setf result (list* :print-backtrace-on-error t result))))
    :finally (return result)))

(defun print-help (pname)
  (format t "~2%~A usage:" pname)
  (format t "~2%    ~A [-h|--help] [-V|--version] [-l|--list-devices]" pname)
  (format t " \\~%    ~VA [-dd|--dw-8000-device-name|-ed|--ex-8000-device-name  name]" (length pname) "")
  (format t " \\~%    ~VA [-dc|--dw-8000-channel|-ec|--ex-8000-channel  midi-channel]" (length pname) "")
  (format t " \\~%    ~VA [-cd|--controller-device-name  name]"                        (length pname) "")
  (format t " \\~%    ~VA [-cc|--controller-channel  midi-channel]"                    (length pname) "")
  (format t " \\~%    ~VA [-v|--verbose] [--print-backtrace-on-error]"                 (length pname) "")
  (format t "~2%  names can be found with --list-devices,")
  (format t "~%  midi-channel go from 1 to 16.")
  (format t "~%  Defaults are: -dd \"Korg DW-8000\" -dc 11 -cd \"VI61\" -cc 11")
  (format t "~2%")
  (finish-output))

(defun print-version (pname)
  (format t "~A version: ~A~%" pname *version*))

(defun main (program-path arguments)
  (let ((print-backtrace-on-error nil))
    (handler-bind
        ((error (lambda (condition)
                  (finish-output *standard-output*)
                  (when print-backtrace-on-error
                    (terpri *error-output*)
                    (print-backtrace *error-output*))
                  (format *error-output* "~%ERROR: ~A~%" condition)
                  (finish-output *error-output*)
                  (ccl:quit 1))))
      (let ((options (parse-arguments arguments))
            (pname   (file-namestring program-path)))
        (setf print-backtrace-on-error (getf options :print-backtrace-on-error))
        (setf *midi-verbose*           (getf options :verbose))
        (cond
          ((getf options :help)
           (print-help pname))
          ((getf options :version)
           (print-version pname))
          ((getf options :list-devices)
           (initialize)
           (print-midi-devices))
          (t
           (initialize)
           (run :controller-device-name (getf options :controller-device-name "VI61")
                :controller-channel     (getf options :controller-channel     10)
                :dw-8000-device-name    (getf options :dw-8000-device-name    "Korg DW-8000")
                :dw-8000-channel        (getf options :dw-8000-channel        10)))))))
  0)

;;;; THE END ;;;;
