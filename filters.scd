(
{
var signal, filter, cutoff, resonance;
signal = PinkNoise.ar(mul: 0.7);
cutoff = MouseX.kr(40, 10000, 1);
resonance = MouseY.kr(0.01, 2.0);
RHPF.ar(signal, cutoff, resonance)!2}.scope(1)
)
(
{
var signal, filter, cutoff, resonance;
signal = PinkNoise.ar(mul: 0.7);
cutoff = MouseX.kr(40, 10000, 1);
resonance = MouseY.kr(0.01, 2.0);
RLPF.ar(signal, cutoff, resonance)!2}.scope(1)
)
(
{
var signal, filter, cutoff, resonance;
signal = PinkNoise.ar(mul: 0.7);
cutoff = MouseX.kr(40, 10000, 1);
resonance = MouseY.kr(0.01, 2.0);
BPF.ar(signal, cutoff, resonance)!2}.scope(1)
)


(
{
var signal, filter, cutoff, resonance;
signal = Saw.ar([50, 75], mul: 0.7);
cutoff = MouseX.kr(40, 10000, 1);
resonance = MouseY.kr(0.01, 2.0);
RLPF.ar(signal, cutoff, resonance)}.scope(2)
)
(
{
var signal, filter, cutoff, resonance;
signal = Saw.ar([50, 75], mul: 0.7);
cutoff = MouseX.kr(40, 10000, 1);
resonance = MouseY.kr(0.01, 2.0);
BPF.ar(signal, cutoff, resonance)}.scope(2)
)
{RLPF.ar(Saw.ar([100,250],0.1), XLine.kr(8000,400,5), 0.05)}.play;

// Frequency control (first patch)
{SinOsc.ar(LFNoise0.kr([12, 12], 500, 500), mul: 0.5)}.play
// Same control source applied to filter cutoff with narrow rq
{RLPF.ar(PinkNoise.ar(0.3), LFNoise0.kr([12, 12], 500, 500), 0.02)}.play
// Wider rq does not result in a focused pitch
{RLPF.ar(PinkNoise.ar(0.9), LFNoise0.kr([12, 12], 500, 500), 1)}.play