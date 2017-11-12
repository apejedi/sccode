MIDIIn.connectAll

(
c = 0;
MIDIdef.midiClock(\test, {
	c = c + 1;
	if (c == 6) {
		s.sendMsg(\c_set, b.index ,0);
		// s.sendMsg(\c_set, b ,1);
		"sent".postln;
		c = 0;
	}
})
)

b = Bus.control(s);
b.index

// x.free

s.sendMsg(\c_set, b.index, 0);
x = {Poll.kr(In.kr(b), DC.kr(1), "boom")}.play

(
SynthDef(\sin, {
	var sig, env;
	sig = SinOsc.ar;
	env = EnvGen.kr(Env.perc(0.01, 0.4), doneAction: 2);
	sig = sig * env;
	Out.ar(0, sig!2);
}).add
)
Synth(\sin)


(
SynthDef(\kick2, {
    var snd;
    snd = DC.ar(0);
    snd = snd + (HPF.ar(Hasher.ar(Sweep.ar), 1320) * Env.perc(0.003, 0.03).ar * 0.5);
    snd = snd + (SinOsc.ar(XLine.ar(750, 161, 0.02)) * Env.perc(0.0005, 0.02).ar);
    snd = snd + (SinOsc.ar(XLine.ar(167, 52, 0.04)) * Env.perc(0.0005, 0.3).ar(2));
    snd = snd.tanh;
    Out.ar(\out.kr(0), Pan2.ar(snd, \pan.kr(0), \amp.kr(0.1)));
}).add;
)

Synth(\kick2, [amp: 0.4]);
s.options.maxLogins = 5

s.options.memSize_(65536 * 4)
s.reboot

s.plotTree