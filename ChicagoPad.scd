
(
SynthDef(\ChicagoPad, { |out = 0, freq = 440, cutoff = 500, amp = 0.2|
	var snd;
	freq = freq + SinOsc.kr(0.1, 0, 1, 20);
	snd = Saw.ar([freq, freq+1, freq-1, freq*3/2, freq*6/5])*0.1;
	snd = snd + VarSaw.ar(0.99*[freq, freq+1, freq-1,freq*3/2, freq*6/5],0, LFTri.kr(0.3).range(0.25,0.9))*0.1;
	snd = Mix(snd);
	snd = RLPF.ar(snd, SinOsc.kr(0.1, 0, 100, 5000), 0.1);
	snd = GVerb.ar(snd ,40, 10, 0.6, 0.6, -3, -9, -11)*0.2;

	//snd = RLPF.ar(snd, SinOsc.kr(0.08, 0.5, cutoff/10, cutoff), 0.2);
	snd = MoogFF.ar(snd, SinOsc.kr(0.08, 0.5, cutoff/10, cutoff), 3, 0);
	snd = DelayC.ar(snd, 1.5, 1,0.8);
	//snd = snd * EnvGen.ar(Env.linen(0.001, 0.01,0.01,1), doneAction:2);
	Out.ar(out, [snd, AllpassC.ar(snd, 0.5, 0.05, 0.3)]*amp);
}).send;
)

Synth(\ChicagoPad, [\freq, 46.midicps])


Synth(\ChicagoPad, [\freq, 57.midicps])

Synth(\ChicagoPad, [\freq, 49.midicps])