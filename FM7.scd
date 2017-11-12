(
SynthDef(\fmTest, {
	|freq = 300, dur = 2|
	var sig, sig2, env;
	sig2 = Saw.ar([freq, freq * 0.98]) * 0.1;
	sig = FM7.ar(
		[
			[freq, 0, 0.3],
			[freq * LinExp.kr(LFNoise1.kr(1/dur), -1 , 1, 1, 7).midiratio, 0, 0.2],
			[EnvGen.kr(Env.new([freq, freq * 7.midiratio], [dur * 0.1], [3])), 0, 0.2],
			[freq * Line.kr(1, 1.5, dur), 0, 0.2],
			[0, 0, 0],
			[0, 0, 0]
		],
		[
			[2, 0, XLine.kr(0.1, 1, dur), 0, 0, 0],
			[Line.kr(0.1, 2, dur * 0.5), 0, 0, 0, 0, 0],
			[0, 2, 0, 0, 0, 0],
			[0, 1, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0]
		]
	);
	sig = Mix.ar([sig[1], sig[0], sig[2]]);
	env = EnvGen.kr(Env.new([0.1, 0.6, 1, 0], [dur * 0.4, dur * 0.2, dur * 0.5], [-2, 0, 1]), doneAction: 2);
	sig = sig * env;
	sig = BPF.ar(sig, freq, 2);
	sig = BHiShelf.ar(sig, 4000, db: -48);
	Out.ar(0, sig!2);
}).add
)

Server.default.dumpOSC(0)
FreqScope.new