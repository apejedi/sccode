MIDIClient.init; //Returns currently available midi devices

MIDIClient.destinations
MIDIIn.connectAll; //Connects to all devices
p = MIDIOut.new(2)

p.noteOn(16, 60, 60);

p.noteOff(16, 61, 60);
p.allNotesOff(16);



(
~notes = Array.newClear(128);

MIDIdef.noteOn(\noteOnHandler, {
	arg velocity,  number, channel, source;
	~notes[number] = Synth.new(\sawPad,[
		\freq, number.midicps,
		\cutoff, 600,
		\cutoff2, 1000,
		\amp, 0.5,
		\release, 2,
		//\amp, velocity.linexp(1, 127, 0.01, 0.03),
		\gate, 1]);
});

MIDIdef.noteOn(\noteOnHandler, {
	arg velocity,  number, channel, source;
	~notes[number] = Synth.new(\bass,[
		\freq, number.midicps,
		\amp, 0.5,
		\release, 2,
		\attack, 0.01,
		//\amp, velocity.linexp(1, 127, 0.01, 0.03),
		\gate, 1]);
});



MIDIdef.noteOff(\noteOffHandler, {
	arg velocity, number;
	~notes[number].set(\gate, 0);
	~notes[number] = nil;
});

MIDIdef.bend(\bendTest, {
	arg vel, num, src;
	~bend = vel;
	~notes.do{arg synth; synth.set(\bend, vel.linlin(0.16383, -2, 2))};
});
SynthDef.new(\testSynth,
	{
		arg freq = 440, amp = 0.3, gate = 0;
		var sig, env;
		sig = LFTri.ar(freq)!2;
		env = EnvGen.kr(Env.adsr, gate, doneAction: 2);
		sig = sig * env * amp;
		Out.ar(0, sig);
}).add;
)