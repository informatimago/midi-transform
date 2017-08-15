;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               parameter-map-compiler-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Tests parameter-map-compiler.lisp
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-13 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER")

(defclass test-parameter ()
  ((name :initarg :name :reader parameter-name)
   (min  :initarg :min  :reader parameter-min)
   (max  :initarg :max  :reader parameter-max)))

(defmethod update-parameter ((parameter test-parameter) value)
  (format t "Parameter ~A changed to ~A~%"
          (parameter-name parameter) value))

(defun test/controllers ()
  ;; (map-cc (cutoff 0 63)   ((continuous 26) 0 127))
  ;; (map-cc polarity        ((toggle 59) 1))
  ;; (map-cc mode            (combination ((toggle 78) 2) ((toggle 62) 1)))
  ;; (map-cc wave            (selection   ((momentary 95) 0) ((momentary 79) 1) ((momentary 63) 2)))
  (let* ((parameters (list
                      (make-instance 'test-parameter :name 'cutoff   :min 0 :max 31) ; continuous
                      (make-instance 'test-parameter :name 'wave     :min 0 :max 2) ; selection of momentary w0 w1 w2
                      (make-instance 'test-parameter :name 'polarity :min 0 :max 1) ; toggle positive/negative
                      (make-instance 'test-parameter :name 'mode     :min 0 :max 3) ; combination of toggle poly/unison, toggle 1/2
                      ))
         (arguments (mapcar (lambda (p) (make-instance 'argument
                                                       :parameter p
                                                       :input-value (parameter-min p)))
                            parameters)))

    (flet ((find-argument (name)
             (find name arguments :key (lambda (a) (parameter-name (argument-parameter a))))))
      (let* ((cutoff-controller
               (make-instance 'continuous-controller
                              :code 26
                              :cc-min 0
                              :cc-max 127
                              :pr-min 0
                              :pr-max 31
                              :output-value 0))
             (polarity-controller
               (make-instance 'toggle-controller
                              :code 59
                              :off 0
                              :on 1
                              :output-value 0))
             (mode-controller
               (make-instance 'combination
                              :output-value 0))
             (mode-toggle-2
               (make-instance 'toggle-controller
                              :code 78
                              :off 0
                              :on 2
                              :output-value 0))
             (mode-toggle-1
               (make-instance 'toggle-controller
                              :code 62
                              :off 0
                              :on 1
                              :output-value 0))
             (wave-controller
               (make-instance 'selection
                              :output-value 0))
             (wave-momentary-0
               (make-instance 'momentary-controller
                              :code 95
                              :on 0
                              :output-value 0))
             (wave-momentary-1
               (make-instance 'momentary-controller
                              :code 79
                              :on 1
                              :output-value 0))
             (wave-momentary-2
               (make-instance 'momentary-controller
                              :code 63
                              :on 2
                              :output-value 0))
             (controllers (make-hash-table)))

        (link-cells cutoff-controller   (find-argument 'cutoff))
        (link-cells polarity-controller (find-argument 'polarity))
        (link-cells mode-toggle-1       mode-controller)
        (link-cells mode-toggle-2       mode-controller)
        (link-cells mode-controller     (find-argument 'mode))
        (link-cells wave-momentary-0    wave-controller)
        (link-cells wave-momentary-1    wave-controller)
        (link-cells wave-momentary-2    wave-controller)
        (link-cells wave-controller     (find-argument 'wave))

        (dolist (controller (list cutoff-controller
                                  polarity-controller
                                  mode-toggle-1
                                  mode-toggle-2
                                  wave-momentary-0
                                  wave-momentary-1
                                  wave-momentary-2))
          (setf (gethash (controller-code controller) controllers) controller))

        (flet ((find-controller (code) (gethash code controllers)))

          ;; (trace cell-output-changed (setf cell-input) (setf cell-output))

          ;; cutoff
          (setf (cell-input (find-controller 26)) 33)
          (setf (cell-input (find-controller 26)) 66)
          (setf (cell-input (find-controller 26)) 127)

          ;; polarity
          (setf (cell-input (find-controller 59)) 127)
          (setf (cell-input (find-controller 59)) 0)
          (setf (cell-input (find-controller 59)) 127)

          ;; mode
          (setf (cell-input (find-controller 78)) 127
                (cell-input (find-controller 62)) 127
                (cell-input (find-controller 78)) 0
                (cell-input (find-controller 62)) 0)

          ;; wave
          (setf (cell-input (find-controller 95)) 127
                (cell-input (find-controller 95)) 0)
          (setf (cell-input (find-controller 79)) 127
                (cell-input (find-controller 79)) 0)
          (setf (cell-input (find-controller 63)) 127
                (cell-input (find-controller 63)) 0)
          (setf (cell-input (find-controller 95)) 127
                (cell-input (find-controller 95)) 0)))))
  :success)



(defun test/all ()
  (test/controllers))

;;;; THE END ;;;;
