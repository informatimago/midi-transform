;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file loads the midi-transform programs and its dependencies.
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

;;;----------------------------------------

(defmacro with-directory (directory-expression &body body)
  (let ((vsaved (gensym)))
    `(let ((,vsaved (uiop:getcwd)))
       (unwind-protect (progn
                         (uiop:chdir ,directory-expression)
                         (locally ,@body))
         (uiop:chdir (namestring ,vsaved))))))

(defvar *base-directory*   nil
  "Base directory, where this source directory is, and where we store the dependencies.")

(defvar *source-directory* nil
  "This source directory.")

(defun clone-repository (repository test-file)
  (unless (probe-file (merge-pathnames test-file *base-directory* nil))
    (with-directory (namestring *base-directory*)
      (uiop:run-program (list "git" "clone" repository)))))

;;;----------------------------------------

(setf *source-directory* (make-pathname :defaults *load-truename*
                                        :name nil :type nil :version nil)
      *base-directory*   (merge-pathnames (make-pathname :directory '(:relative :up "dependencies"))
                                          *source-directory*
                                          nil))

(clone-repository "git@framagit.org:patchwork/CoreMIDI.git"                       "../dependencies/CoreMIDI/coremidi.lisp")
(clone-repository "git@git.framasoft.org:abnotation/midi.git"                     "../dependencies/midi/midi.lisp")
(clone-repository "git@git.framasoft.org:com-informatimago/com-informatimago.git" "../dependencies/com-informatimago/com.informatimago.asd")

(dolist (path (list (merge-pathnames "midi/"              *base-directory*)
                    (merge-pathnames "CoreMIDI/"          *base-directory*)
                    (merge-pathnames "com-informatimago/" *base-directory*)
                    *source-directory*))
  (pushnew (truename path) asdf:*central-registry* :test (function equalp)))

(require "OBJC-SUPPORT")
(ql:quickload :com.informatimago.midi.transform)
(load "cffi-utils")

(in-package "COMMON-LISP-USER")

(defun foreign-raw-string-to-lisp (pointer)
  "Copy at most COUNT bytes from POINTER plus OFFSET encoded in
ENCODING into a Lisp string and return it.  If POINTER is a null
pointer, NIL is returned."
  (unless (cffi:null-pointer-p pointer)
    (com.informatimago.cffi-utils:foreign-null-terminated-vector pointer :uchar 'character #'code-char)))


(defun vmini->schmidt ()
  (com.informatimago.midi.transform:run
   :controller-device-name "VMini"
   :controller-channel 8
   :dw-8000-device-name "SCHMIDT SYNTH"
   :dw-8000-channel 7))

(defun vmini->ms2000r ()
  (com.informatimago.midi.transform:run
   :controller-device-name "VMini"
   :controller-channel 8
   :dw-8000-device-name "Korg MS2000R"
   :dw-8000-channel 14))

(print '(com.informatimago.midi.transform:run))
(print '(vmini->schmidt))
(print '(vmini->ms2000r))
;;;; THE END ;;;;
