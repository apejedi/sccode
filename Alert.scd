//Fig 58.7: Red alert
//Complete patch, saw + second harmonic, resonators, distortion and post filters to create
//fixed resonance. Contains the subpatch shown in fig 58.6.

(
	SynthDef(\red, {

	var env, redAlert, dfbe, aIn, bIn, cIn, aOut, bOut, cOut;

	# aIn, bIn, cIn = LocalIn.ar(7).clumps([5,1,1]);

	env = [
		EnvGen.ar(Env.new([0,1,1,0], [0.9, 0.3, 0.0])),
		EnvGen.ar(Env.new([0,1,1,0], [0.01, 0.88, 0.01]))
	];
	env[0] = LFSaw.ar(env[0].sqrt * 487 + 360, 1, 0.5, 0.5);
	env[0] = (env[0] - 0.5) + ((env[0] * 2 * 2pi).cos * 0.3);
	redAlert = (env[0] - OnePole.ar(env[0], exp(-2pi * (1 * SampleDur.ir)))) * env[1];
	redAlert = redAlert + (cIn * 0.006) * 0.2;


	// fig 58.6: Delay feedback element
	dfbe = 0!6;
	redAlert = redAlert + (aIn * 0.7);
	5.do{|i| dfbe[i] = DelayN.ar(redAlert[i], 0.1, [0.015, 0.022, 0.035, 0.024, 0.011][i])};
	aOut = dfbe[0..4];

	redAlert = redAlert[0..4].sum;
	redAlert = (redAlert - OnePole.ar(redAlert, exp(-2pi * (12 * SampleDur.ir))));

	dfbe[5] = redAlert + (bIn * 0.7);
	dfbe[5] = DelayN.ar(dfbe[5], 0.1, 0.061);
	# bOut, cOut = dfbe[5]!2;
	LocalOut.ar(aOut ++ bOut ++ cOut);

	redAlert = Clip.ar(redAlert * 4, -1, 1);
	redAlert = BPF.ar(redAlert, [740, 1400, 1500, 1600], (12!4).reciprocal) ++ (redAlert * 0.5);
	Out.ar(0, (redAlert.sum * 0.2)!2);

}).add;
)

// ALL HANDS!! BATTLESTATIONS!
e = Synth(\red);
e.free


(
x = {
        var mod = SinOsc.ar([0.1,0.15,0.2]).range(0,1);
        var sig = LFPulse.ar(1.1).range(1,10);
        var sig2 = LFNoise1.ar(0.08).range(0.99,1.01);
        var saw = LFSaw.ar([10,12,15]*sig*sig2);
        var out = (saw - ((saw>0)*mod)+(mod*0.5));
        Splay.ar(out)*0.1;
}.play;

{
        var mod = SinOsc.ar([0.1,0.15,0.2]).range(0,1);
        var sig = LFPulse.ar(1).range(1,10);
        var sig2 = LFTri.ar(0.1,[0,1,2,3]).range(1,10*sig);
        var sig3 = LFNoise1.ar(0.05).range(0.99,1.01);
        var saw = SinOsc.ar(LFSaw.ar([10,12,15]*sig2*sig3).range(200,300));
        var out = (saw - ((saw>0)*mod)+(mod*0.5));
        Splay.ar(out)*0.1;
}.play
)


// code also available here:
// http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Red_alert