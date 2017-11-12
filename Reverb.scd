

(
SynthDef("someGrains", { arg centerFreq = 777, freqDev = 200, grainFreq = 2;
    var gate;
    gate = Impulse.kr(grainFreq);
    Out.ar(
        0,
        SinOsc.ar(
            LFNoise0.kr(4, freqDev, centerFreq),
            0,
            EnvGen.kr(Env.sine(0.1), gate, 0.1)
        )
    )
}).add;

SynthDef("aDelay", { arg delay = 0.1;
    Out.ar(
        1,
        DelayC.ar(
            In.ar(0, 1),
            delay,
            delay
        )
    )
}).add;
)

x = Synth(\someGrains)

y = Synth.tail(s, \aDelay)
x.free
y.free

x = {SinOsc.ar(500.rrand(1000), 0, 0.2) * Line.kr(1, 0, 0.1)}.play
/////////////////////////////////////////////////////////////////////////////////////////
// TURN ON THE INTERNAL SERVER!!
// first a comb filter and then an allpass with (with the same parameters) - compare them
/////////////////////////////////////////////////////////////////////////////////////////

s.sendMsg("n_free", 1068)
// comb example
(
x = {
    CombN.ar(
        SinOsc.ar(500.rrand(1000), 0, 0.2) * Line.kr(1, 0, 0.1),
        0.1,
        0.25,
        -6
    )
}.scope;
)

// allpass example - not much difference from the comb example
(
 x= {
    AllpassN.ar(
        SinOsc.ar(500.rrand(1000), 0, 0.2) * Line.kr(1, 0, 0.1),
        0.1,
        0.025,
        6
    )
}.scope;
)
x.free
(
x = {
	var s, z, y;
	// 10 voices of a random sine percussion sound :
	s = Mix.ar(Array.fill(10, { Resonz.ar(Dust.ar(0.2, 50), 200 + 3000.0.rand, 0.003)}) );
	// reverb predelay time :
	z = DelayN.ar(s, 0.048);
	// 7 length modulated comb delays in parallel :
	y = Mix.ar(Array.fill(7,{ CombL.ar(z, 0.1, LFNoise1.kr(0.1.rand, 0.04, 0.05), 15) }));
	// two parallel chains of 4 allpass delays (8 total) :
	6.do({ y = AllpassN.ar(y, 0.050, [0.050.rand, 0.050.rand], 1) });
	// add original sound to reverb and play it :
	//y
	s+(0.2*y)!2
}.scope
)
