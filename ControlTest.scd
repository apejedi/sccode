(
SynthDef(\kickTest, {
	|dur = 0.2, trigBus|
	var sig;
	sig = WhiteNoise.ar * 0.1 * EnvGen.kr(Env.perc(releaseTime: dur), gate: trigBus);
	Out.ar(0, sig!2);
}).add;

)

a = Synth(\kickTest, \trigBus, Impulse.ar(10))


