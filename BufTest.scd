s.boot

s.plotTree
~buf = Buffer.read(s, "D:\\musicradar-drum-samples\\musicradar-drum-samples\\Drum Kits\\Kit 6 - Electro\\CYCdh_ElecK03-Kick02.wav");

~buf.play
{PlayBuf.ar(2, ~buf, 0.5, loop: 1)!2}.play

~clock.free
~clock = Buffer.alloc(s, s.sampleRate * 8);

~clock.numFrames


{RecordBuf.ar(Dust.ar(3), ~clock, loop: 0, doneAction:2)}.play


(
SynthDef(\BufTest, {|trigger = 1, buf = -1, rate = 1|
	var sig = SinOsc.ar(440);
	var scaledRate = rate * BufRateScale.kr(buf);
	var env = EnvGen.ar(Env.perc(0.5),
		//Select.kr(buf > -1, [BufRd.kr(1, buf, Phasor.ar(0, 1, 0, BufFrames.kr(buf)), loop: 0), 1]),
		Select.ar(buf > -1, [PlayBuf.ar(1, buf, scaledRate), 1]),
		doneAction: 0);
	Out.ar([0, 1], sig * env);
}).add
)
Synth(\BufTest, [\trigger , ~clock, \rate, 1])


{PlayBuf.kr(1, ~clock, 1, loop: 0)}.scope

{Poll.kr(Impulse.kr(10), Phasor.ar(1, 1/s.sampleRate, 0, BufFrames.kr(~clock)), "BufRateScale: ")}.play



(
{ var trig, rate, x, sr;
    rate = MouseX.kr(1, 200, 1);
    trig = Impulse.ar(rate);
    sr = SampleRate.ir;
    x = Phasor.ar(trig, rate / sr, 0, 1, [0, MouseY.kr(0, 1)]);
    SinOsc.ar(x * 500 + 500, 0, 0.2)
}.play;
)
