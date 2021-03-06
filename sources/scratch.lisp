

(in-package "COM.INFORMATIMAGO.MIDI.KORG.DW-8000")

(device-id-request 10)
#(240 66 74 247)

(data-dump-request 10)
#(240 66 58 3 16 247)

(write-request 10 0)
#(240 66 58 3 17 0 247)

(parameter-change-request 10 +cutoff+ 32)
#(240 66 58 3 65 15 32 247)

(data-dump 10
           '((osc1-octave 3)
             (osc1-waveform 15)
             (osc1-level 31)
             (auto-bend-select 3)
             (auto-bend-mode 1)
             (auto-bend-time 31)
             (auto-bend-intensity 31)
             (osc2-octave 3)
             (osc2-waveform 15)
             (osc2-level 31)
             (osc2-interval 7)
             (osc2-detune 7)
             (noise-level 31)
             (assign-mode 3)
             (parameter-no-memory 63)
             (cutoff 63)
             (resonance 31)
             (keyboard-track 3)
             (polarity 1)
             (vcf-eg-intensity 31)
             (vcf-attack 31)
             (vcf-decay 31)
             (vcf-break-point 31)
             (vcf-slope 31)
             (vcf-sustain 31)
             (vcf-release 31)
             (vcf-velocity-sensitivity 7)
             (vca-attack 31)
             (vca-decay 31)
             (vca-break-point 31)
             (vca-slope 31)
             (vca-sustain 31)
             (vca-release 31)
             (vca-velocity-sensitivity 7)
             (mg-wave-form 3)
             (mg-frequency 31)
             (mg-delay 31)
             (mg-osc 31)
             (mg-vcf 31)
             (bend-osc 15)
             (bend-vcf 1)
             (delay-time 7)
             (delay-factor 15)
             (delay-feedback 15)
             (delay-frequency 31)
             (delay-intensity 31)
             (delay-effect-level 15)
             (portamento 31)
             (aftertouch-osc-mg 3)
             (aftertouch-vcf 3)
             (aftertouch-vca 3)))
