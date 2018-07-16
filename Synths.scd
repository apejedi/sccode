MIDIClient.init
MIDIIn.connect(1, MIDIClient.sources.at(1));

s.plotTree
~notes = Array.newClear(128)


(
SynthDef(\vibPad, {
	|freq = 330, gate = 1, outBus = 0, amp = 1|
	var osc, osc2, sig, env;
	osc = Saw.ar(freq);
	osc = DFM1.ar(osc,
		LinLin.kr(SinOsc.kr(3), -1, 1, 1000, 8000)
		// LinLin.kr(SinOsc.kr(LinLin.kr(LFNoise2.kr(5), -1, 1, 3, 24)), -1, 1, 1000, 8000)
		// , XLine.kr(0.1, 10, 2 )
	);
	osc2 = SinOsc.ar(freq /2) * 0.2;
	sig = Mix.ar([osc]);
	sig = LPF.ar(sig, freq * LinLin.kr(SinOsc.kr(8), -1, 1, 2, 8));
	env = EnvGen.kr(Env.adsr(2, sustainLevel: 0.5, releaseTime: 1, curve:-2), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig !2);
}).add
)
x = {VarSaw.ar(200, width: Line.kr(0.1, 0.5, 3))}.play;
x.free
(
SynthDef(\sawPad, {
	|freq = 300, gate = 1, outBus = 0, amp = 1|
	var sig, sig2, env;
	sig = VarSaw.ar(freq, width: LinLin.kr(LFTri.kr(LinLin.kr(SinOsc.kr(100), -1, 1, 2, 10)), -1, 1, 0.2, 1.5)) * 0.1;
	sig2 = SawDPW.ar([freq, freq * 1.0001]);
	sig = Mix.ar(Mix.ar(sig2));
	sig = DFM1.ar(sig, XLine.kr(1000, 3000, 3));
	sig = BHiShelf.ar(sig, 3000, db: -18);
	// sig = LPF.ar(sig, LinLin.kr(SinOsc.kr(LinLin.kr(SinOsc.kr(LinLin.kr(SinOsc.kr(0.5), -1, 1, 1, 12)), -1, 1, 1, 12)), -1, 1, 1000, 5000));
	env = EnvGen.kr(Env.adsr(releaseTime: 1), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig!2);
}).add
)
(
SynthDef(\fmTest, {
	|freq = 300, gate = 1, amp = 1, outBus = 0|
	var sig, ctls, mod, env;
	ctls =
	[
		[freq, 0, 0.1],
		[freq * 7.midiratio, 0, 0.1],
		[LinLin.kr(SinOsc.kr(0.5), -1, 1, 300, 600), 0, 0.1],
		[0, 0, 0],
		[0, 0, 0],
		[0, 0, 0]
	];
	mod = [
		[Line.kr(0, 10, 2), 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0]
	];
	sig = FM7.ar(ctls, mod).slice([0, 1]);
	sig = BRF.ar(Mix.ar(sig), 120);
	env = EnvGen.kr(Env.adsr(releaseTime: 2), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig !2);
}).add
)

Env([300 * 0.5, 300, 300 * 12.midiratio, 300 * 7.midiratio], [0.5, 0.4, 0.8], [5, -4, -4]).plot
(
SynthDef(\fm2, {
	|freq = 300, gate = 1, amp = 1, outBus = 0|
	var sig, ctls, mod, env;
	ctls =
	[
		[freq, 0, 0.4],
		[freq * 9.midiratio, 0, 0.05],
		[EnvGen.kr(Env([freq, freq * 12.midiratio], [3], [-4])), 0, 0],
		[0, 0, 0],
		[0, 0, 0],
		[0, 0, 0]
	];
	mod = [
		[0, 0, 0, 0, 0, 0],
		[0, XLine.kr(0.01, 0.2, 2), 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0]
	];
	sig = FM7.ar(ctls, mod);
	sig = BLowShelf.ar(sig, 800, db: -18);
	env = EnvGen.kr(Env.adsr(0.01, releaseTime: 2), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig !2)
}).add
)


Env.adsr(1, sustainLevel: 0.7, releaseTime: 0.7, curve: 2).plot

12.midiratio
(
MIDIdef.freeAll;
MIDIdef.noteOn(\noteOn, {
	|a, b|
	~notes[b] = Synth(\test, [\freq, b.midicps]);
});
MIDIdef.noteOff(\noteOff, {
	|a, b|
	~notes[b].set(\gate, 0);
});
)

8.midiratio
