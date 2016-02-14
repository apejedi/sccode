
(
SynthDef(\Bass, {
	|freq = 65.406, len = 2, level = 0.4|
	var sound, noise, release;
	sound = SinOsc.ar(freq) * EnvGen.kr(Env.perc(0.01 * len, 0.99 * len), doneAction: 2);
	noise = VarSaw.ar(freq) * EnvGen.kr(Env.perc(0.01 * len, 0.99 * len), doneAction: 2);
	Out.ar(0, Limiter.ar(Mix.ar([sound, noise]), level)!2);
}).add;
SynthDef(\SineChord, {
	|len = 2, level = 0.4|
	var sound, noise, notes, harmonics = 5;
	notes = Control.names([\freqs]).ir([36,42.811456140068,53.939054767475]);

	notes = notes.collect{
		|fund|
		Array.fill(harmonics, {
			arg count;
			fund * (count + 1)
	})};
	sound = FSinOsc.ar(notes.flatten, mul: 1/notes.size) * EnvGen.kr(Env.perc(0.1 * len, 0.9 * len), doneAction: 2) * level;
	//noise = VarSaw.ar(freq) * EnvGen.kr(Env.perc(0.01 * len, 0.99 * len), doneAction: 2);
	Out.ar(0, sound!2);
}).add;
~getScale = {
	|ratios = #[ 1, 1.1224620483089, 1.1892071150019, 1.3348398541685, 1.4983070768743, 1.5874010519653, 1.7817974362766 ], freq = 65.4, octaves = 1|
	var notes = Array.newClear(octaves * 8);
	notes.size.do({
		|idx|
		notes[idx] = ratios.wrapAt(idx) * (freq * (idx.div(8) + 1));
	});
	notes;
};
~getChords = {
	|scale = #[36, 40.40863373912, 42.811456140068, 48.054234750066, 53.939054767475, 57.146437870751, 64.144707705958], numNotes = 3|
	scale.collect {
		|note, idx|
		Array.fill(numNotes, {
			|degree|
			scale.wrapAt(idx + (2*degree))
		});
	};
}
)

(
f = 56;
~clock = TempoClock.new;
~clock.tempo = 3;
~bassClock = TempoClock.new;
~bassClock.tempo = 1;


~scale = Scale.choose;
("Using " + ~scale).postln;
~freqs = ~getScale.value(~scale.ratios, f.midicps, 3);
~chords = ~getChords.value(~freqs, 4);


b = Pbind(
	\instrument, \Bass,
	\freq, Pxrand(~getScale.value(~scale.ratios, (f - 12).midicps), inf),
	\len,4,
	\level, 0.2
).play(~bassClock);


m = Pbind(
	\instrument, \Bass,
	//\freq, Pxrand(~freqs, inf),
	//\freq, Pseq(5.collect({~freqs.choose}), inf),
	\freq, Pwalk(~freqs, Pwhite(0, 8, inf)),
	\len,1,
	\level, 0.4
).play(~clock);

// c = Pbind(
// 	\instrument, \SineChord,
// 	\freqs, Pxrand(~chords, inf),
// 	\dur, 3,
// 	\len, 3,
// 	\level, 0.1
// ).play(~bassClock);

)

