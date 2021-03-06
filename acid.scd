

(
SynthDef("acid", { arg out, freq = 1000, gate = 1, pan = 1, cut = 4000, rez = 0.8, amp = 1;
	Out.ar(out,
		Pan2.ar(
			RLPF.ar(
				Pulse.ar(freq,0.05),
			cut, rez),
		pan) * EnvGen.kr(Env.linen(0.01, 1, 0.3), gate, amp, doneAction:2);
	)
}).store;
)


(
Pbind(\instrument,\acid, \dur,Pseq([0.25,0.5,0.25],inf), \root,-15,
	 \degree,Pseq([0,3,5,7,9,11,5,1],inf), \pan,Pfunc({1.0.rand2}),
	 \cut,Pxrand([1000,500,2000,300],inf), \rez,Pfunc({0.7.rand +0.3}), \amp,0.2).play;
)

(
Pseq([
	Pbind(\instrument,\acid, \dur,Pseq([0.25,0.5,0.25],4), \root,-24,
		\degree,Pseq([0,3,5,7,9,11,5,1],inf), \pan,Pfunc({1.0.rand2}),
		\cut,Pxrand([1000,500,2000,300],inf),\rez,Pfunc({0.7.rand +0.3}), \amp,0.2),

	Pbind(\instrument,\acid, \dur,Pseq([0.25],6), \root,-24, \degree,Pseq([18,17,11,9],inf),
		\pan,Pfunc({1.0.rand2}),\cut,1500, \rez,Pfunc({0.7.rand +0.3}), \amp,0.16)

],inf).play;
)