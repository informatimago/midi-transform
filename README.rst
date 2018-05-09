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

    RC:
        Program down 1
        Not implemented yet
    RC: 4194336: (MIDI:CONTROL-CHANGE-MESSAGE :TIME 480044063399527 :STATUS 186 :CHANNEL 10 :CONTROLLER 75 :VALUE 0)
    RC: 4194336: CC 75 0
    RC: 4194336: (MIDI:CONTROL-CHANGE-MESSAGE :TIME 480046353908195 :STATUS 186 :CHANNEL 10 :CONTROLLER 74 :VALUE 127)
    RC: 4194336: CC 74 127
        Program up 1
        Not implemented yet
    RC: 4194336: (MIDI:CONTROL-CHANGE-MESSAGE :TIME 480046568972751 :STATUS 186 :CHANNEL 10 :CONTROLLER 74 :VALUE 0)
    RC: 4194336: CC 74 0
    RC: 4194336: (MIDI:CONTROL-CHANGE-MESSAGE :TIME 480049600834620 :STATUS 186 :CHANNEL 10 :CONTROLLER 0 :VALUE 0)
    RC: 4194336: CC 0 0
        Program change 32
        Not implemented yet



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
