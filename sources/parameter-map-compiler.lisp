;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               parameter-map-compiler.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Compiles a cc-parameter map.
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
;; (delete-package "COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER")
(defpackage "COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER")
  (:export "SELECT" "GROUP" "MAP"
           "COMBINATION" "SELECTION"
           "CONTINUOUS" "MOMENTARY"
           "TOGGLE" "PROGRAM-CHANGE"
           "COMPILE-MAP"  "DISPATCH"

           ;; internal parameter names:
           "PAGE" "BANK"
           "PROGRAM-CHANGE"
           "PROGRAM-UP"
           "PROGRAM-DOWN"))
(in-package "COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER")


(defun bare-parameter (parameter)
  (if (listp parameter)
      (first parameter)
      parameter))

(defun parameter-arguments (parameter)
  (if (listp parameter)
      (rest parameter)
      '()))


#|
(select <group-name> <controller-spec>…)
<controller-spec> ::= ((momentary <cc>) <value>)
momentary -> ignore 0 ; action on 127: (<group-name> <value>)
|#

#|

- receive  cc-number cc-value
- find cc-number in compiled-map and selected groups.
- apply action.


actions for select:
  the computed value is used to select the group of same name and value.

actions for map:
  - for continuous controllers, compute (truncate (- (clip cc-min cc-value cc-max) cc-min)
                                                  (- p-max p-min -1))

         p-min                   p-max
        |-----|-----|-----|-----|-----|
     cc-min                         cc-max

    and set the specified parameter to that value.

  - for toggle controllers, set parameter to (if cc-value = 0 then 0 else p-value)

  - for momentary controllers, when cc-value /= 0 then set parameter to p-value

  - for combine, we nee to keep a list of the current value of sub-controllers,
    and when one value change, compute the sum before setting the parameter.

  - for range controller such as program-change, maps the range.

|#
#|

((continuous cc) cc-min cc-max)  -> direct value, 3-rule

                                  (= (/ (- cc-val cc-min) (- cc-max cc-min))
                                     (/ (- pr-val pr-min) (- pr-max pr-min)))

                                  (setf pr-val
(+ (/ (* (- pr-max pr-min) (- (clip cc-min cc-val cc-max) cc-min))
                                              (- cc-max cc-min))
                                           pr-min))

((toggle cc) on)
((toggle cc) off on)             -> 0/on or off/on

((momentary cc) val)             -> trigger to val when cc ≠ 0

(combine …)                      -> add sub-controller values and trigger when one of them changes.

|#




;; (setf (controller-input c1) cc-value)
;; --> (setf (controller-output self) (computed-output (controller-input self)))
;;     (cell-changed ci self)
;; ==>>
;; (cell-changed argument c2)
;; --> (setf (argument-value argument) (controller-output c2))


(defgeneric cell-input (cell))
(defgeneric (setf cell-input) (new-value cell))

(defgeneric cell-output (cell))
(defgeneric (setf cell-output) (new-value cell))
(defgeneric cell-output-changed (downstream-cell changed-cell))

(defgeneric downstream-cells        (cell)          (:method ((cell t))          '()))
(defgeneric (setf downstream-cells) (new-list cell) (:method (new-list (cell t)) new-list))
(defgeneric upstream-cells          (cell)          (:method ((cell t))          '()))
(defgeneric (setf upstream-cells)   (new-list cell) (:method (new-list (cell t)) new-list))

(defgeneric dispatch (object cc value))

;;; ----------------------------------------
;;;
(defclass input ()
  ((input-value :initarg :input-value :reader cell-input)))

(defmethod (setf cell-input) (new-value (self input))
  (setf (slot-value self 'input-value) new-value))


;;; ----------------------------------------
;;;
(defclass output ()
  ((output-value :initarg :output-value :initform 0 :reader cell-output)))

(defmethod (setf cell-output) (new-value (self output))
  (setf (slot-value self 'output-value) new-value))

(defmethod (setf cell-output) :after (new-value (self output))
  (declare (ignore new-value))
  (dolist (downstream-cell (downstream-cells self))
    (cell-output-changed downstream-cell self)))


(defgeneric link-cells (output input)
  (:method ((output t) (input t))
    (assert output (output) "Output cannot be NIL.")
    (assert input  (input)  "Input cannot be NIL.")
    (push output (upstream-cells   input))
    (push input  (downstream-cells output))
    (values output input)))

(defgeneric unlink-cells (output input)
  (:method ((output t) (input t))
    (setf  (upstream-cells   input)  (delete output (upstream-cells   input))
           (downstream-cells output) (delete input  (downstream-cells output)))
    (values output input)))


;;; ----------------------------------------
;;;
(defclass argument (input)
  ((parameter  :reader argument-parameter :initarg :parameter)))

(defmethod print-object ((self argument) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~@{~S~^ ~}"
            :parameter-name (parameter-name (argument-parameter self))
            :cell-input (cell-input self)))
  self)

(defmethod (setf cell-input) :after (new-value (self argument))
  (declare (ignore new-value))
  (update-parameter (argument-parameter self)
                    (cell-input self)))

(defmethod cell-output-changed ((self argument) (changed output))
  (setf (cell-input self) (cell-output changed)))


;;; ----------------------------------------
;;;
(defclass cell (output)
  ((downstream-cells :accessor downstream-cells
                     :initform '() :initarg :downstream-cells)))

(defmethod print-object ((self cell) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~@{~S~^ ~}"
            :downstream-cells-count (length (downstream-cells self))
            :cell-output (cell-output self)))
  self)



;;; ----------------------------------------
;;;
(defclass controller (cell)
  ((code      :initarg :code      :reader controller-code)))

(defmethod controllers ((self controller))
  (list self))

(defmethod add-controller ((self cell) (controller controller))
  (link-cells controller self))


;;; ----------------------------------------
;;;
(defclass combination (cell)
  ((operator :initform '+  :initarg :operator :reader combination-operator)
   (cells    :initform '() :initarg :cells    :reader combination-cells
             :accessor upstream-cells)))


(defmethod cell-output-changed ((self combination) (changed output))
  (declare (ignore changed))
  (setf (cell-output self) (reduce (combination-operator self)
                                  (combination-cells self)
                                  :key (function cell-output))))

(defmethod controllers ((self combination))
  (mapcan (function controllers) (upstream-cells self)))

(defmethod add-controller ((self combination) (controller cell))
  (link-cells controller self))

(defmethod dispatch ((self combination) cc value)
  (dolist (cell (combination-cells self))
    (dispatch cell cc value)))


;;; ----------------------------------------
;;;
(defclass selection (cell)
  ((selectors :initarg :selectors :initform '() :reader selection-selectors)))

(defmethod cell-output-changed ((self selection) (changed output))
  (setf (cell-output self) (cell-output changed)))

(defmethod controllers ((self selection))
  (mapcan (function controllers) (upstream-cells self)))

(defmethod add-controller ((self selection) (controller controller))
  (push controller (slot-value self 'selectors))
  (link-cells controller self))

(defmethod upstream-cells ((self selection))
  (selection-selectors self))

(defmethod dispatch ((self selection) cc value)
  (dolist (selector (selection-selectors self))
    (dispatch selector cc value)))


;;; ----------------------------------------
;;;
(defclass continuous-controller (controller)
  ((cc-min :initarg :cc-min :reader controller-min)
   (cc-max :initarg :cc-max :reader controller-max)
   (pr-min :initarg :pr-min :reader controller-parameter-min)
   (pr-max :initarg :pr-max :reader controller-parameter-max)))

;; Note: (<= cc-min cc-val cc-max) (because of CLIP),
;;   but (< pr-max pr-min) is possible to invert the slope.

(defmethod print-object ((self continuous-controller) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (code cc-min cc-max pr-min pr-max) self
      (format stream "~@{~S~^ ~}"
              :downstream-cells-count (length (downstream-cells self))
              :cell-output (cell-output self)
              :code code
              :cc-min cc-min :cc-max cc-max
              :pr-min pr-min :pr-max pr-max)))
  self)

(defun clip (min val max)
  (cond
    ((< val min) min)
    ((< max val) max)
    (t           val)))

(defmethod (setf cell-input) (cc-val (self continuous-controller))
  (symbol-macrolet ((pr-val (cell-output self)))
    ;; TODO: move pr-min pr-max to the parameter instances
    (with-slots (cc-min cc-max pr-min pr-max) self
      (setf pr-val
            (round (+ (/ (* (- pr-max pr-min) (- (clip cc-min cc-val cc-max) cc-min))
                         (- cc-max cc-min))
                      pr-min))))))


;;; ----------------------------------------
;;;
(defclass toggle-controller (controller)
  ((on  :initarg :on  :reader controller-parameter-on)
   (off :initarg :off :reader controller-parameter-off :initform 0)))

(defmethod print-object ((self toggle-controller) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (code on off) self
      (format stream "~@{~S~^ ~}"
              :downstream-cells-count (length (downstream-cells self))
              :cell-output (cell-output self)
              :code code
              :on on
              :off off)))
  self)

(defmethod (setf cell-input) (cc-val (self toggle-controller))
  (symbol-macrolet ((pr-val (cell-output self)))
    (with-slots (on off) self
      (setf pr-val (if (zerop cc-val) off on)))))



;;; ----------------------------------------
;;;
(defclass momentary-controller (controller)
  ((on  :initarg :on  :reader controller-parameter-on)))

(defmethod print-object ((self momentary-controller) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (code on) self
      (format stream "~@{~S~^ ~}"
              :downstream-cells-count (length (downstream-cells self))
              :cell-output (cell-output self)
              :code code
              :on on)))
  self)

(defmethod (setf cell-input) (cc-val (self momentary-controller))
  (symbol-macrolet ((pr-val (cell-output self)))
    (with-slots (on) self
      (unless (zerop cc-val) (setf pr-val on)))))


;;; ----------------------------------------
;;;
(defclass program-change-controller (controller)
  ((program-min :initarg :program-min :reader controller-program-min)
   (program-max :initarg :program-max :reader controller-program-max)
   (pr-min :initarg :pr-min :reader controller-parameter-min)
   (pr-max :initarg :pr-max :reader controller-parameter-max))
  (:default-initargs :code 0))

(defmethod print-object ((self program-change-controller) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (program-min program-max) self
      (format stream "~@{~S~^ ~}"
              :downstream-cells-count (length (downstream-cells self))
              :cell-output (cell-output self)
              :program-min program-min :program-max program-max)))
  self)


(defmethod (setf cell-input) (program-val (self program-change-controller))
  (symbol-macrolet ((pr-val (cell-output self)))
    ;; TODO: move pr-min pr-max to the parameter instances
    (with-slots (program-min program-max pr-min pr-max) self
      (setf pr-val
            (round (+ (/ (* (- pr-max pr-min) (- (clip program-min program-val program-max)
                                                 program-min))
                         (- program-max program-min))
                      pr-min))))))



#|

Compiler
========================================

Dispatching CC numbers:
----------------------------------------


A given CC number can have several instances (possibly even of
different classes) and thus may drive several parameters at the
same time.

The SELET operator let the use select a GROUP of CC inside a given
category that is active at one time. The other groups in the same
category are not active.  Thus a given CC number  may also have
several instances but drive several parameters at different times.



Grammar:
----------------------------------------

map            ::= <item>… .
item           ::= <select> | <map>  .
select         ::= (SELECT <controller> <group>…) .
map            ::= (MAP <parameter> <controller>) .
parameter      ::= (<parameter-name> <pr-min> <pr-max>) | <parameter-name> .
controller     ::= <pc> | <toggle> | <momentary> | <continuous> | <selection> | <combination> .
pc             ::= (PROGRAM-CHANGE <program-min> [<program-max>])
toggle         ::= ((TOGGLE     <cc>) [<pr-off-val] <pr-on-val>) .
momentary      ::= ((MOMENTARY  <cc>) <pr-on-val>) .
continuous     ::= ((CONTINUOUS <cc>) <cc-min> <cc-max>) .
selection      ::= (SELECTION   <controller>…) .
combination    ::= (COMBINATION <controller>…) .
group          ::= (GROUP <item>…) .
parameter-name ::= symbol .
pr-on-val      ::= integer .
pr-off-val     ::= integer .
pr-val         ::= integer .
pr-min         ::= integer .
pr-max         ::= integer .
cc             ::= integer .
cc-min         ::= integer .
cc-max         ::= integer .
program-min    ::= integer .
program-max    ::= integer .



Compilation:
----------------------------------------

All the parameter-names in the source must be collected first to build
a table of interned argument instances.

All the MAP in a given <item>… list are collected into a single dispatch.

Each select will generate an additionnal set of dispatch (one per
group), one of them being active, along with the select controller.


|#

(defmethod dispatch ((self controller) cc value)
  (when (= cc (controller-code self))
    (setf (cell-input self) value)))



;;; ----------------------------------------
;;;
(defclass compiled-map ()
  ((cc-map  :initform (make-hash-table)
            :reader map-cc-table
            :documentation "Maps a CC number to a list of controllers.")
   (pc-map  :initform (make-hash-table)
            :reader map-pc-table
            :documentation "Maps a PC number to a list of controllers.")
   (selects :initform '()
            :reader map-selects
            :documentation "A list of SELECT instances.")))

;;; ----------------------------------------
;;;
(defclass select (cell input)
  ((selector       :initarg :selector
                   :reader select-selector)
   (groups         :initform #()
                   :initarg :groups
                   :reader select-groups)
   (selected-group :accessor select-selected-group)))

(defgeneric select-group (select group-number)
  (:method ((self select) group-number)
    (setf (select-selected-group self) (aref (select-groups self) group-number))))

(defmethod (setf cell-input) :after (new-value (self select))
  (select-group self (cell-input self))
  (setf (cell-output self) new-value))

(defmethod cell-output-changed ((self select) (changed output))
  (setf (cell-input self) (cell-output changed)))

(defmethod add-controller ((self select) (selector cell))
  (when (slot-boundp self 'selector)
    (unlink-cells selector self))
  (setf (slot-value self 'selector) selector)
  (link-cells selector self))

(defmethod add-group ((self select) (group compiled-map))
  (setf (slot-value self 'groups) (concatenate 'vector (select-groups self) (list group)))
  (unless (slot-boundp self 'selected-group)
    (select-group self 0)))

(defmethod dispatch ((self select) cc value)
  (dispatch (select-selector       self) cc value)
  (dispatch (select-selected-group self) cc value))



;;; ----------------------------------------
;;; compiled-map

(defmethod add-controller ((self compiled-map) (cell cell))
  (dolist (controller (controllers cell))
    (push controller (gethash (controller-code controller) (map-cc-table self) '()))))

(defmethod add-select ((self compiled-map) (select select))
  (push select (slot-value self 'selects)))

(defmethod dispatch ((self compiled-map) cc value)
  (dolist (controller (gethash cc (map-cc-table self)))
    (dispatch controller cc value))
  (dolist (select (map-selects self))
    (dispatch select cc value)))


;;; ----------------------------------------
;;;

(defun collect-parameters (map)
  (loop
    :with parameters := '()
    :for expression :in map
    :for op := (first expression)
    :do (case op
          ((select)
           (destructuring-bind (parameter-spec controller &rest groups) (rest expression)
             (declare (ignore controller))
             (setf parameters
                   (cons (bare-parameter parameter-spec)
                         (nconc parameters
                                (mapcan (lambda (group)
                                          (unless (eq 'group (first group))
                                            (error "Invalid group ~S" group))
                                          (collect-parameters (rest group)))
                                        groups))))))
          ((map)
           (push (bare-parameter (second expression))
                 parameters))
          (otherwise (error "Invalid operator in map: ~S" op)))
    :finally (return parameters)))

(defun build-argument-table (map parameters)
  (let ((present-parameters (delete-duplicates (collect-parameters map)))
        (arguments          (make-hash-table)))
    (dolist (name present-parameters)
      (let ((parameter (find name parameters :key (function parameter-name))))
        (if parameter
            (setf (gethash name arguments) (make-instance 'argument
                                                          :parameter parameter
                                                          :input-value (parameter-min parameter)))
            (error "Unknown parameter name ~S" name))))
    arguments))

(defun find-argument (name arguments)
  (let ((argument (gethash name arguments)))
    (unless argument
      (error "No argument named ~S could be found in given arguments table"
             name))
    argument))

(defun compile-controller-expression (expression arguments &optional pr-min pr-max)
  (let ((op1 (first expression)))
    (if (symbolp op1)
        (case op1
          ((selection combination)
           ;; selection      ::= (SELECTION   <controller>…) .
           ;; combination    ::= (COMBINATION <controller>…) .
           (let ((controller (make-instance op1)))
             (dolist (subexpression (rest expression) controller)
               (add-controller controller (compile-controller-expression subexpression arguments)))))
          ((program-change)
           ;; pc             ::= (PROGRAM-CHANGE <program-min> [<program-max>])
           (destructuring-bind (op program-min program-max) expression
             (declare (ignore op))
             (check-type program-min (integer 0 127))
             (check-type program-max (integer 0 127))
             (make-instance 'program-change-controller
                            :program-min program-min
                            :program-max program-max
                            :pr-min pr-min
                            :pr-max pr-max)))
          (otherwise
           (error "Unexpected token ~S in controller expression ~S" op1 expression)))
        (destructuring-bind (op2 cc) op1
          (case op2
            (toggle
             ;; toggle         ::= ((TOGGLE     <cc>) [<pr-off-val] <pr-on-val>) .
             (let ((pr-on-val (second expression))
                   (pr-off-val 0))
               (when (cddr expression)
                 (setf pr-off-val pr-on-val
                       pr-on-val (third expression)))
               (make-instance 'toggle-controller :code cc :off pr-off-val :on pr-on-val)))
            (momentary
             ;; momentary      ::= ((MOMENTARY  <cc>) <pr-on-val>) .
             (make-instance 'momentary-controller :code cc :on (second expression)))
            (continuous
             ;; continuous     ::= ((CONTINUOUS <cc>) <cc-min> <cc-max>) .
             (destructuring-bind (ignored cc-min cc-max) expression
               (declare (ignore ignored))
               (make-instance 'continuous-controller :code cc
                                                     :cc-min cc-min :cc-max cc-max
                                                     :pr-min pr-min :pr-max pr-max)))
            (otherwise
             (error "Unexpected token ~S in controller expression ~S" op2 expression)))))))

(defun compile-select-expression (expression arguments)
  (destructuring-bind (op parameter-spec controller &rest groups) expression
    (assert (eq 'select op))
    ;; for now, we only take atomic parameter-spec (we expect the controller to be discrete).
    (let* ((argument (find-argument parameter-spec arguments))
           (select   (make-instance 'select)))
      (link-cells select argument)
      (add-controller select (compile-controller-expression controller arguments))
      (dolist (group groups)
        (destructuring-bind (op &rest items) group
          (assert (eq 'group op))
          (add-group select (compile-items items arguments))))
      select)))

(defun compile-map-expression (expression arguments)
  (destructuring-bind (op parameter-spec controller-expression) expression
    (assert (eq 'map op))
    (if (listp parameter-spec)
        (destructuring-bind (pr-name pr-min pr-max) parameter-spec
          ;; TODO: Move back pr-min pr-max to the parameter, and let the parameter instances compute their new value.
          (let ((controller (compile-controller-expression controller-expression arguments pr-min pr-max))
                (argument   (find-argument pr-name arguments)))
            (link-cells controller argument)
            controller))
        (let ((controller (compile-controller-expression controller-expression arguments))
              (argument   (find-argument parameter-spec arguments)))
          (link-cells controller argument)
          controller))))

(defun compile-items (items arguments)
  (loop
    :with map := (make-instance 'compiled-map)
    :for expression :in items
    :for op := (first expression)
    :do (case op
          (select (add-select     map (compile-select-expression expression arguments)))
          (map    (add-controller map (compile-map-expression    expression arguments)))
          (otherwise (error "Invalid operator in map: ~S" op)))
    :finally (return map)))

(defun compile-map (items parameters)
  (check-type items list)
  (check-type parameters sequence)
  (compile-items items (build-argument-table items parameters)))


;; (untrace cell-output-changed select-group (setf cell-input) controller-code dispatch)

;;;; THE END ;;;;


