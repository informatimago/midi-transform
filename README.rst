.. comment: -*- mode:rst;coding:utf-8 -*-


midi-transform
############################################################


This is a little CoreMIDI MacOSX command-line program whose purpose is
to convert some MIDI controller Controller Change MIDI messages into
SysEx parameter change MIDI message for some synthesizer.
Accessorily, it also forwards all the other MIDI messages from the
controller to the synthesizer.

Currently only the SysEx Parameter list for the Korg DW-8000/EX-8000
is implemented, with a default mapping for the Alesis VI61 MIDI
controller.

Demo video at:

- https://www.youtube.com/watch?v=zUB6otw7lic

See also:

- https://www.stereoping.com/synth-controller/

- https://www.youtube.com/watch?v=K6nbcbXXn5g



Features
============================================================

Main Features:
------------------------------------------------------------

- Receives MIDI messages from the controller, and

- for normal MIDI messages (note, etc), forwards them to the DW-8000 /
  EX-8000 synthesizer (updating the channel if needed).

- CC messages are transformed into parameter-change sysex (updating
  the channel if needed).


Secondary Features (implemented):
------------------------------------------------------------

- pass through mode for knobs (continuous controls).


Secondary Features (not implemented yet):
------------------------------------------------------------

- option for absolute or pass-thru mode for knobs. (absolute is better
  when creating new sounds, pass-thru is better when patching a sound
  live).

- configure the CC mapping interactively:

   + select in the user interface a parameter of the target synthesizer.

   + receive a CC message from the controller.

   + establish the mapping between that CC message and the parameter.

- support other target synthesizers (Korg DSS-1 in addition to Korg DW-8000).

- support multiple target synthesizers and multiple controllers.

- load and save programs.

- load and save whole program banks.

- currently bank MSB/LSB are ignored for program changes; they
  could be taken into account, automatically downloading new
  banks.

- add some graphical (or ascii art) features such as drawing
  the envelopes when modifying them…

- port to Linux (eg. Raspberry Pi)


Usage
============================================================

::

    $ ./midi-transform  --help


    midi-transform usage:

        midi-transform [-h|--help] [-l|--list-devices]\
                       [-dd|--dw-8000-device-name|-ed|--ex-8000-device-name  name]\
                       [-dc|--dw-8000-channel|-ec|--ex-8000-channel  midi-channel]\
                       [-cd|--controller-device-name  name]\
                       [-cc|--controller-channel  midi-channel]

      names can be found with --list-devices,
      midi-channel go from 1 to 16.
      Defaults are: -dd "Korg DW-8000" -dc 11 -cd "VI61" -cc 11



RC File
------------------------------------------------------------

When starting up, ``midi-transform`` will read the file
``.midi-transform.lisp`` in the user ``HOME`` directory, if it exists,
and evaluate the Common Lisp expressions contained.


Commands
------------------------------------------------------------

While running, ``midi-transform`` prints a prompt ("> "), and reads
commands.  Currently the following commands are recognized:

quit
    stops running.

help
    prints the list of recognized commands.


Furthermore, when unrecognized commands are entered, they are
interpreted as Common Lisp expressions.  Example: ::

    [pjb@despina :0.0 current-midi-transform]$ ./midi-transform

    > (list (lisp-implementation-type) (lisp-implementation-version))
     --> ("Clozure Common Lisp" "Version 1.11-r16635  (DarwinX8664)")
    > (+ 2 3)
     --> 5
    > (IN-PACKAGE "COM.INFORMATIMAGO.MIDI.TRANSFORM")
     --> #<Package "COM.INFORMATIMAGO.MIDI.TRANSFORM">
    > (setf *midi-verbose* t)
     --> T
    >



Example
============================================================

::

    $ ./midi-transform
        Selected group 0
    >
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 4
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 4
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 4
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 4
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 4
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 4
        Selected group 1
    CC: UPDATE PARAMETER VCA-VELOCITY-SENSITIVITY TO 3
    CC: UPDATE PARAMETER VCA-VELOCITY-SENSITIVITY TO 2
        Selected group 0
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 2
    CC: UPDATE PARAMETER VCF-VELOCITY-SENSITIVITY TO 1
        Selected group 2
    CC: UPDATE PARAMETER MG-OSC TO 22
    CC: UPDATE PARAMETER MG-OSC TO 23
    CC: UPDATE PARAMETER DELAY-EFFECT-LEVEL TO 2
    CC: UPDATE PARAMETER DELAY-EFFECT-LEVEL TO 3
    CC: UPDATE PARAMETER DELAY-EFFECT-LEVEL TO 4
    CC: UPDATE PARAMETER DELAY-EFFECT-LEVEL TO 5
    > help
    Help:
      help     Displays this help.
      quit     Stops this midi application.
    > quit

    $


Obtaining the sources
============================================================

1- create a directory where to clone it, since dependencies will be
cloned in brother directories. ::

    $ mkdir src ; cd src

2- clone the sources: ::

    $ git clone git@framagit.org:pjb/midi-transform.git

Running in ccl: ::

    > (ccl:chdir "src/midi-transform/")
    > (load "loader")
    > (com.informatimago.midi.transform:initialize)
    > (com.informatimago.midi.transform:run
              :dw-8000-device-name "Korg DW-8000"
              :dw-8000-channel 10
              :controller-device-name "VI61"
              :controller-channel 10)

Internally, MIDI channels numbers go from 0 to 15, (they're converted
while parsing the the command line arguments).  So use 10 to indicate
MIDI Channel 11, 0 for MIDI Channel 1.  The above values are default
parameters, so you can run it with just: ::

    > (com.informatimago.midi.transform:run)

if you configure your devices on the channel 11 and name them like
this in the "Audio MIDI Setup" application.


Compiling the binary program
============================================================

::

    $ make help
    $ make
    $ ./midi-transform --help

The midi-transform program can be installed in ``/usr/local/bin`` with: ::

    $ make install

or in some other directory by specifying the ``PREFIX``: ::

    $ make PREFIX=/opt/local install

will install ``/opt/local/bin/midi-transform``.


TODO
============================================================

::

    --------------------------------------------------------------------------------
    (BACKTRACE-AS-LIST :CONTEXT NIL :PROCESS NIL :ORIGIN NIL :COUNT 1152921504606846975 :START-FRAME-NUMBER 0 :PRINT-LEVEL 2 :PRINT-LENGTH 5 :SHOW-INTERNAL-FRAMES NIL)
    (PRINT-BACKTRACE #<SYNONYM-STREAM to *TERMINAL-IO* #x30200121BA3D>)
    (FUNCALL (:INTERNAL COM.INFORMATIMAGO.MIDI.TRANSFORM::MIDI-PORT-READ) #<SIMPLE-ERROR #x30200166222D>)
    (SIGNAL #<SIMPLE-ERROR #x30200166222D>)
    (%ERROR #<SIMPLE-ERROR #x30200166222D> (:EXPECTING-DATA-DUMP) 9390223)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.KORG.DW-8000::CHECK-STATE (COM.INFORMATIMAGO.MIDI.KORG.DW-8000:DW-8000-SYNTHESIZER)> #<COM.INFORMATIMAGO.MIDI.KORG.DW-8000:DW-8000-SYNTHESIZER #x302001316D9D>)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.KORG.DW-8000::SEND-PROGRAM-CHANGE (COM.INFORMATIMAGO.MIDI.KORG.DW-8000:DW-8000-SYNTHESIZER T T T)> #<COM.INFORMATIMAGO.MIDI.KORG.DW-8000:DW-8000-SYNTHESIZER #x302001316D9D> 0 0 32)
    (FUNCALL (:INTERNAL ((SETF COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER:SYNTHESIZER-CURRENT-PROGRAM) :AFTER (T COM.INFORMATIMAGO.MIDI.KORG.DW-8000:DW-8000-SYNTHESIZER))))
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.KORG.DW-8000::ENQUEUE* (T T)> #<COM.INFORMATIMAGO.MIDI.KORG.DW-8000:DW-8000-SYNTHESIZER #x302001316D9D> #<CCL:COMPILED-LEXICAL-CLOSURE (:INTERNAL #) #x30200166227F>)
    (FUNCALL (:INTERNAL ((SETF COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER:SYNTHESIZER-CURRENT-PROGRAM) :AFTER (T COM.INFORMATIMAGO.MIDI.KORG.DW-8000:DW-8000-SYNTHESIZER))) #<COM.INFORMATIMAGO.MIDI.KORG.DW-8000::INTERNAL-PARAMETER #x302001314CCD> 32)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER:UPDATE-PARAMETER (COM.INFORMATIMAGO.MIDI.KORG.DW-8000::INTERNAL-PARAMETER T)> #<COM.INFORMATIMAGO.MIDI.KORG.DW-8000::INTERNAL-PARAMETER #x302001314CCD> 32)
    (FUNCALL #<STANDARD-METHOD (SETF COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::CELL-INPUT) :AFTER (T COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::ARGUMENT)> 32 #<ARGUMENT :PARAMETER-NAME COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER:PROGRAM-CHANGE :CELL-INPUT 32 #x30200131445D>)
    (%%BEFORE-AND-AFTER-COMBINED-METHOD-DCODE (NIL #<STANDARD-METHOD # :AFTER #> . 9390283))
    (%%STANDARD-COMBINED-METHOD-DCODE (NIL (#<#>) #<STANDARD-METHOD # #>) 9390283)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::CELL-OUTPUT-CHANGED (COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::ARGUMENT COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::OUTPUT)> #<ARGUMENT :PARAMETER-NAME COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER:PROGRAM-CHANGE :CELL-INPUT 32 #x30200131445D> #<PROGRAM-CHANGE-CONTROLLER :DOWNSTREAM-CELLS-COUNT 1 :CELL-OUTPUT 32 :PROGRAM-MIN 32 :PROGRAM-MAX 63 #x302001310E0D>)
    (FUNCALL #<STANDARD-METHOD (SETF COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::CELL-OUTPUT) :AFTER (T COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::OUTPUT)> 32 #<PROGRAM-CHANGE-CONTROLLER :DOWNSTREAM-CELLS-COUNT 1 :CELL-OUTPUT 32 :PROGRAM-MIN 32 :PROGRAM-MAX 63 #x302001310E0D>)
    (%%BEFORE-AND-AFTER-COMBINED-METHOD-DCODE (NIL #<STANDARD-METHOD # :AFTER #> . 9390322))
    (%%STANDARD-COMBINED-METHOD-DCODE (NIL (#<#>) #<STANDARD-METHOD # #>) 9390322)
    (FUNCALL #<STANDARD-METHOD (SETF COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::CELL-INPUT) (T COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::PROGRAM-CHANGE-CONTROLLER)> 0 #<PROGRAM-CHANGE-CONTROLLER :DOWNSTREAM-CELLS-COUNT 1 :CELL-OUTPUT 32 :PROGRAM-MIN 32 :PROGRAM-MAX 63 #x302001310E0D>)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER:DISPATCH (COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::CONTROLLER T T)> #<PROGRAM-CHANGE-CONTROLLER :DOWNSTREAM-CELLS-COUNT 1 :CELL-OUTPUT 32 :PROGRAM-MIN 32 :PROGRAM-MAX 63 #x302001310E0D> 0 0)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER:DISPATCH (COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::COMPILED-MAP T T)> #<COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::COMPILED-MAP #x302001311EDD> 0 0)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER:DISPATCH (COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER:SELECT T T)> #<SELECT :DOWNSTREAM-CELLS-COUNT 1 :CELL-OUTPUT 0 #x302001312A2D> 0 0)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER:DISPATCH (COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::COMPILED-MAP T T)> #<COM.INFORMATIMAGO.MIDI.PARAMETER-MAP-COMPILER::COMPILED-MAP #x3020013138CD> 0 0)
    (FUNCALL #<STANDARD-METHOD COM.INFORMATIMAGO.MIDI.TRANSFORM::MAP-CONTROLLER-TO-SYSEX-REQUEST (COM.INFORMATIMAGO.MIDI.TRANSFORM::CONVERT-CC-DW-8000-APPLICATION T T)> #<COM.INFORMATIMAGO.MIDI.TRANSFORM::CONVERT-CC-DW-8000-APPLICATION #x30200130A8AD> 0 0)
    (MIDI-PORT-READ #<A Foreign Pointer #x7000060C3E70> #<A Foreign Pointer #x11144E0>)
    (%PORT-READ-CALLBACK #<A Foreign Pointer #x7000060C3E70> #<A Foreign Pointer #x113A40> #<A Foreign Pointer #x11144E0>)
    (FUNCALL CFFI-CALLBACKS::|COM.INFORMATIMAGO.MACOSX.COREMIDI::PORT-READ-CALLBACK| 15393175472062)
    --------------------------------------------------------------------------------
    EE: 17908960: Invalid synthesizer state EXPECTING-DATA-DUMP
        Program change parameter = #<INTERNAL-PARAMETER #x302001314CCD>  value = 32
    RC: (MIDI:PROGRAM-CHANGE-MESSAGE :TIME 500878002295559 :STATUS 200 :CHANNEL 8 :PROGRAM 114)
    RC: #<A Foreign Pointer #x11144E0>: PC 114


License
============================================================

It's distribued under the GNU AFFERO GENERAL PUBLIC LICENSE, Version 3.

    Copyright Pascal J. Bourguignon 2017 - 2017

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


.. comment:

       ./midi-transform  -cd VMini -cc 9 -dd 'Korg MS2000R' -dc 14
       (com.informatimago.midi.transform:run
           :controller-device-name "VMini"
           :controller-channel 9
           :dw-8000-device-name "Korg MS2000R"
           :dw-8000-channel 14)

.. comment: THE END.
