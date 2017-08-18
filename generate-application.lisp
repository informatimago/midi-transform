;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This script generates the midi-transform program on CCL on MacOSX.
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

(in-package "COMMON-LISP-USER")

(eval-when (:compile-toplevel :load-toplevel)
  (error "This file should be loaded as source, not compiled"))

;;; --------------------------------------------------------------------

(defun say (fmt &rest args)
  (format t "~&;;; ~?~%" fmt args)
  (finish-output))

(defun local-file (name &optional type)
  (make-pathname :name name
                 :type type
                 :version nil
                 :defaults #.(or *compile-file-truename* *load-truename*)))

;;; --------------------------------------------------------------------

(say "Loading quicklisp.")
(load #P"~/quicklisp/setup.lisp")
(setf quicklisp-client:*quickload-verbose* t)

;;; --------------------------------------------------------------------

(say "Load loader.")
(load (local-file "loader"))

;;; --------------------------------------------------------------------

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
  (format t "~2%    ~A [-h|--help] [-l|--list-devices]" pname)
  (format t " \\~%    ~VA [-dd|--dw-8000-device-name|-ed|--ex-8000-device-name  name]" (length pname) "")
  (format t " \\~%    ~VA [-dc|--dw-8000-channel|-ec|--ex-8000-channel  midi-channel]" (length pname) "")
  (format t " \\~%    ~VA [-cd|--controller-device-name  name]"                        (length pname) "")
  (format t " \\~%    ~VA [-cc|--controller-channel  midi-channel]"                    (length pname) "")
  (format t "~2%  names can be found with --list-devices,")
  (format t "~%  midi-channel go from 1 to 16.")
  (format t "~%  Defaults are: -dd \"Korg DW-8000\" -dc 11 -cd \"VI61\" -cc 11")
  (format t "~2%")
  (finish-output))


(defun main ()
  (let ((print-backtrace-on-error nil))
    (handler-bind
        ((error (lambda (condition)
                  (when print-backtrace-on-error
                    (terpri *error-output*)
                    (com.informatimago.midi.transform::print-backtrace *error-output*))
                  (format *error-output* "~%ERROR: ~A~%" condition)
                  (ccl:quit 1))))
      (let ((options (parse-arguments #+ccl (rest (ccl::command-line-arguments)))))
        (setf print-backtrace-on-error (getf options :print-backtrace-on-error))
        (cond
          ((getf options :list-devices)
           (com.informatimago.midi.transform:initialize)
           (com.informatimago.midi.transform:print-midi-devices))
          ((getf options :help)
           (print-help (file-namestring (first (ccl::command-line-arguments)))))
          (t
           (com.informatimago.midi.transform:initialize)
           (com.informatimago.midi.transform:run
            :dw-8000-device-name    (getf options :dw-8000-device-name    "Korg DW-8000")
            :dw-8000-channel        (getf options :dw-8000-channel        10)
            :controller-device-name (getf options :controller-device-name "VI61")
            :controller-channel     (getf options :controller-channel     10)))))))
  (ccl:quit 0))

#+ccl (ccl:save-application "midi-transform"
                            :toplevel-function (function main)
                            :init-file nil
                            :error-handler nil
                            :application-class nil
                            :clear-clos-caches nil
                            :purify t
                            :mode #o755
                            :prepend-kernel t)

;;;; THE END ;;;;
