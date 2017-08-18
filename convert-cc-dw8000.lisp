;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               convert-cc-dw8000.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Transforms MIDI CC into DW-8000 parameter-changes.
;;;;
;;;;
;;;;    Main Features:
;;;;
;;;;    - Receives MIDI messages from the controller, and
;;;;
;;;;    - for normal MIDI messages (note, etc), forwards them to the DW-8000 /
;;;;      EX-8000 synthesizer (updating the channel if needed).
;;;;
;;;;    - CC messages are transformed into parameter-change sysex (updating
;;;;      the channel if needed).
;;;;
;;;;
;;;;    Secondary Features (implemented):
;;;;
;;;;    - pass through mode for knobs (continuous controls).
;;;;
;;;;
;;;;    Secondary Features (not implemented yet):
;;;;
;;;;    - configure the CC mapping interactively:
;;;;       + select in the user interface a DW-8000 parameter.
;;;;       + receive a CC message from the controller.
;;;;       + establish the mappin between that CC message and the parameter.
;;;;
;;;;    - load and save programs.
;;;;
;;;;    - load and save whole program banks.
;;;;
;;;;    - currently bank MSB/LSB are ignored for program changes; they
;;;;      could be taken into account, automatically downloading new
;;;;      banks.
;;;;
;;;;    - add some graphical (or ascii art) features such as drawing
;;;;      the envelopes when modifying them…
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
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI.MIDI"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
        "COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER"
        "COM.INFORMATIMAGO.MIDI.KORG.DW-8000")
  (:shadowing-import-from "COREMIDI" "DEVICE-ID")
  (:export "INITIALIZE" "RUN" "PRINT-MIDI-DEVICES"))
(in-package "COM.INFORMATIMAGO.MIDI.TRANSFORM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *effects* '())
(defun call-effects (&rest arguments)
  (mapc (lambda (effect)
          (handler-case (apply effect arguments)
            (error (err)
              (format t "~&EE: effect ~A error: ~A~%" effect err)
              (setf *effects* (delete effect *effects*)))))
        (copy-list *effects*))
  *effects*)


(defvar *midi-log* *trace-output*)
(defvar *midi-application* nil)


(defparameter *allow-print-backtrace* t)
(defun print-backtrace (&optional (output *error-output*))
  (when *allow-print-backtrace*
   #+ccl (format output "~&~80,,,'-<~>~&~{~A~%~}~80,,,'-<~>~&"
                 (ccl::backtrace-as-list))))

(defun client-notify (message)
  (format *midi-log* "MM: ~A~%" message)
  (force-output *midi-log*))


(defun midi-port-read (packet-list source-connection-refcon)
  (let ((*standard-output* *midi-log*)
        (source-connection-refcon (cffi:pointer-address source-connection-refcon))
        (output-list '()))
    (cond
      ((dw-8000-refcon-p *midi-application* source-connection-refcon)

       (let ((message-list (packet-list-to-messages packet-list)))
         (call-effects :start-packet-list message-list source-connection-refcon)
         (handler-case
             (handler-bind
                 ((error (lambda (condition)
                           (terpri *error-output*)
                           (print-backtrace)
                           (format *error-output* "~%ERROR: ~A~%" condition)
                           (signal condition))))
               (dolist (message message-list)
                 ;; (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
                 ;;   (format t "~&RD: ~A: ~A~%" source-connection-refcon message))
                 (call-effects :message message source-connection-refcon)))
           (error (err)
             (format t "~&RD: ~A: ~A~%" source-connection-refcon  err)))
         (force-output)
         (call-effects :end-packet-list message-list source-connection-refcon)))

      ((controller-refcon-p *midi-application* source-connection-refcon)

       (let ((message-list (packet-list-to-messages packet-list)))
         (handler-case
             (handler-bind
                 ((error (lambda (condition)
                           (terpri *error-output*)
                           (print-backtrace)
                           (format *error-output* "~%ERROR: ~A~%" condition)
                           (finish-output)
                           (signal condition))))

               (dolist (message message-list)
                 ;; (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
                 ;;   (format t "~&RC: ~A: ~A~%" source-connection-refcon message))
                 (typecase message
                   (program-change-message
                    (let ((program  (message-program message)))
                      (format t "~&RC: ~A: PC ~A~%" source-connection-refcon program)
                      (unless (= (message-channel message)
                                 (dw-8000-channel *midi-application*))
                        (setf (message-channel message) (dw-8000-channel *midi-application*)))
                      (push message output-list)))
                   (control-change-message
                    (let ((controller (message-controller message))
                          (value      (message-value      message)))
                      ;; (format t "~&RC: ~A: CC ~A ~A~%" source-connection-refcon controller value)
                      (if (configuringp *midi-application*)
                          (configure *midi-application* controller value)
                          (map-controller-to-sysex-request *midi-application* controller value))))
                   (t
                    ;; (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
                    ;;   (format t "~&RC: ~A: ~A~%" source-connection-refcon message))
                    (unless (= (message-channel message)
                               (dw-8000-channel *midi-application*))
                      (setf (message-channel message) (dw-8000-channel *midi-application*)))
                    (push message output-list)))))
           (error (err)
             (format t "~&RC: ~A: ~A~%" source-connection-refcon  err)))))

      (t
       (format t "~&RR: ~A: unexpected refcon.~%" source-connection-refcon)))
    (when output-list
      (send (midi-output-port *midi-application*)
            (dw-8000-destination *midi-application*)
            (packet-list-from-messages (nreverse output-list))))
    (force-output)))




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
        (first (mapcan (lambda (x)
                         (mapcan (lambda (device)
                                   (let ((endpoints (find external-device (entity-endpoints device)
                                                          :key (function connected-devices)
                                                          :test (function member))))
                                     (when endpoints (list endpoints))))
                                 (device-entities x)))
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

(defclass convert-cc-dw8000-application (midi-application)
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


(defgeneric (setf controller-state) (new-state application controller)
  (:method (new-state (application convert-cc-dw8000-application) controller)
    (check-type controller (integer 0 127))
    (send (midi-output-port application)
          (controller-destination application)
          (packet-list-from-messages
           (list (make-instance 'control-change-message :time (current-host-time)
                                                        :channel (controller-channel application)
                                                        :controller controller
                                                        :value (if new-state 127 0)))))))



(defmethod initialize-instance :after ((self convert-cc-dw8000-application) &key &allow-other-keys)
  (let ((synthesizer (synthesizer self)))
    (setf (slot-value self 'dw-8000-destination)    (find-destination-endpoint-for-device-named (dw-8000-device-name self))
          (slot-value self 'dw-8000-source)         (find-source-endpoint-for-device-named      (dw-8000-device-name self))
          (synthesizer-destination synthesizer)     (slot-value self 'dw-8000-destination)
          (synthesizer-source      synthesizer)     (slot-value self 'dw-8000-source)
          (slot-value self 'controller-destination) (find-destination-endpoint-for-device-named (controller-device-name self))
          (slot-value self 'controller-source)      (find-source-endpoint-for-device-named      (controller-device-name self))))
  (connect-source self (slot-value self 'dw-8000-source)     (dw-8000-refcon self))
  (connect-source self (slot-value self 'controller-source)  (controller-refcon self)))


(defgeneric controller-refcon-p (application refcon)
  (:method ((self convert-cc-dw8000-application) refcon)
    (= refcon (controller-refcon self))))

(defgeneric dw-8000-refcon-p (application refcon)
  (:method ((self convert-cc-dw8000-application) refcon)
    (= refcon (dw-8000-refcon self))))

(defgeneric map-controller-to-sysex-request (application controller value)
  (:method ((self convert-cc-dw8000-application) controller value)
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

    (compile-map
     '(
       (select page (selection
                     ((momentary 95) 0)
                     ((momentary 79) 1)
                     ((momentary 63) 2))

         (group
          (map-cc (osc1-octave              0 3)     ((continuous 20) 0 127))
          (map-cc (osc1-waveform            0 15)    ((continuous 21) 0 127))
          (map-cc (osc1-level               0 31)    ((continuous 22) 0 127))
          (map-cc (aftertouch-osc-mg        0 3)     ((continuous 23) 0 127))
          (map-cc (keyboard-track           0 3)     ((continuous 24) 0 127))
          (map-cc (noise-level              0 31)    ((continuous 25) 0 127))
          (map-cc (cutoff                   0 63)    ((continuous 26) 0 127))
          (map-cc (resonance                0 31)    ((continuous 27) 0 127))
          (map-cc (vcf-eg-intensity         0 31)    ((continuous 28) 0 127))
          (map-cc (vcf-attack               0 31)    ((continuous 29) 0 127))
          (map-cc (vcf-decay                0 31)    ((continuous 30) 0 127))
          (map-cc (vcf-break-point          0 31)    ((continuous 31) 0 127))
          (map-cc (vcf-slope                0 31)    ((continuous 32) 0 127))
          (map-cc (vcf-sustain              0 31)    ((continuous 33) 0 127))
          (map-cc (vcf-release              0 31)    ((continuous 34) 0 127))
          (map-cc (vcf-velocity-sensitivity 0 7)     ((continuous 35) 0 127)))

         (group
          (map-cc (osc2-octave              0 3)     ((continuous 20) 0 127))
          (map-cc (osc2-waveform            0 15)    ((continuous 21) 0 127))
          (map-cc (osc2-level               0 31)    ((continuous 22) 0 127))
          (map-cc (osc2-interval            0 7)     ((continuous 23) 0 127))
          (map-cc (osc2-detune              0 7)     ((continuous 24) 0 127))
          (map-cc (noise-level              0 31)    ((continuous 25) 0 127))
          (map-cc (cutoff                   0 63)    ((continuous 26) 0 127))
          (map-cc (resonance                0 31)    ((continuous 27) 0 127))
          (map-cc (mg-wave-form             0 3)     ((continuous 28) 0 127))
          (map-cc (vca-attack               0 31)    ((continuous 29) 0 127))
          (map-cc (vca-decay                0 31)    ((continuous 30) 0 127))
          (map-cc (vca-break-point          0 31)    ((continuous 31) 0 127))
          (map-cc (vca-slope                0 31)    ((continuous 32) 0 127))
          (map-cc (vca-sustain              0 31)    ((continuous 33) 0 127))
          (map-cc (vca-release              0 31)    ((continuous 34) 0 127))
          (map-cc (vca-velocity-sensitivity 0 7)     ((continuous 35) 0 127)))

         (group
          (map-cc (aftertouch-vcf           0 3)     ((continuous 20) 0 127))
          (map-cc (aftertouch-vca           0 3)     ((continuous 21) 0 127))
          (map-cc (auto-bend-time           0 31)    ((continuous 22) 0 127))
          (map-cc (auto-bend-intensity      0 31)    ((continuous 23) 0 127))
          (map-cc (bend-osc                 0 15)    ((continuous 24) 0 127))
          (map-cc (portamento               0 31)    ((continuous 25) 0 127))
          (map-cc (mg-frequency             0 31)    ((continuous 26) 0 127))
          (map-cc (mg-delay                 0 31)    ((continuous 27) 0 127))
          (map-cc (mg-osc                   0 31)    ((continuous 28) 0 127))
          (map-cc (mg-vcf                   0 31)    ((continuous 29) 0 127))
          (map-cc (delay-time               0 7)     ((continuous 30) 0 127))
          (map-cc (delay-factor             0 15)    ((continuous 31) 0 127))
          (map-cc (delay-feedback           0 15)    ((continuous 32) 0 127))
          (map-cc (delay-frequency          0 31)    ((continuous 33) 0 127))
          (map-cc (delay-intensity          0 31)    ((continuous 34) 0 127))
          (map-cc (delay-effect-level       0 15)    ((continuous 35) 0 127))))

       (map-cc assign-mode                         (combination ((toggle 78) 2) ((toggle 62) 1)))
       (map-cc auto-bend-select                    (combination ((toggle 77) 1) ((toggle 61) 2)))
       (map-cc auto-bend-mode                      ((toggle 60) 1))
       (map-cc polarity                            ((toggle 59) 1))
       (map-cc bend-vcf                            ((toggle 58) 1)))

     parameters)))



(defun run (&key
              (dw-8000-device-name "Korg DW-8000")
              (dw-8000-channel 10)
              (controller-device-name "VI61")
              (controller-channel 10))
  (setf *midi-log* *terminal-io*)
  (let* ((synthesizer (make-instance 'dw-8000-synthesizer
                                     :name dw-8000-device-name
                                     :channel dw-8000-channel))
         (application
           (create-midi-application 'convert-cc-dw8000-application
                                    "Transform CC -> DW-8000 Parameter"
                                    'client-notify 'midi-port-read
                                    :dw-8000-device-name dw-8000-device-name
                                    :dw-8000-channel dw-8000-channel
                                    :controller-device-name controller-device-name
                                    :controller-channel controller-channel
                                    :synthesizer synthesizer
                                    :cc-map (default-map-cc synthesizer))))
    (unwind-protect
         (progn
           (setf *midi-application* application)
           (loop
             :for command := (string-trim " "
                                          (progn (format t "> ")
                                                 (finish-output)
                                                 (read-line)))
             :do (cond
                   ((string-equal command "quit"))
                   ((string-equal command "help")
                    (format t "~&Help: ~:{~%  ~8A ~A~}~%"
                            '(("help" "Displays this help.")
                              ("quit" "Stops this midi application."))))
                   (t
                    (format t "~&Error: Unknown command ~S~%" command)))
             :until (string-equal command "quit")))
      (terminate *midi-application*)
      (setf *midi-application* nil))))



(defun initialize ()
  (coremidi-framework))

(defun print-midi-devices ()
  (dolist (device (append (devices)
                          (external-devices)))
    (let ((entities      (device-entities device)))
      (format t "~30A ~%"
              (name device))
      (dolist (entity entities)
        (format t "          - ~A~@[ <- ~{~A~^, ~}~]~@[ -> ~{~A~^, ~}~]~%"
                (name entity)
                (mapcar (function name) (entity-sources entity))
                (mapcar (function name) (entity-destinations entity))))
      (terpri)))
  (finish-output))

;;;; THE END ;;;;


