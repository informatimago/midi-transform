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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc (lambda (package) (unuse-package package "COMMON-LISP-USER"))
        (remove  (find-package "COMMON-LISP")  (copy-seq (package-use-list "COMMON-LISP-USER"))))
  (unintern 'quit))

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

(say "Loading loader.")
(load (local-file "loader"))

;;; --------------------------------------------------------------------

#+ccl (ccl:save-application
       "midi-transform"
       :toplevel-function (lambda ()
                            (handler-bind
                                ((error (lambda (condition)
                                          (finish-output *standard-output*)
                                          (terpri *error-output*)
                                          (com.informatimago.midi.transform::print-backtrace *error-output*)
                                          (format *error-output* "~%ERROR: ~A~%" condition)
                                          (finish-output *error-output*)
                                          (ccl:quit 1))))
                              (let ((result (com.informatimago.midi.transform:main
                                             (first (ccl::command-line-arguments))
                                             (rest  (ccl::command-line-arguments)))))
                                (if (typep result '(integer 0 255))
                                    (ccl:quit result)
                                    (ccl:quit 0)))))
       :init-file nil
       :error-handler :quit
       :application-class nil
       :clear-clos-caches nil
       :purify t
       :mode #o755
       :prepend-kernel t)

;;;; THE END ;;;;
