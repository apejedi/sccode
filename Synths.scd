
(
SynthDef(\vibPad, {
	|freq = 330, gate = 1, outBus = 0, amp = 1|
	var osc, osc2, sig, env;
	osc = Saw.ar(freq);
	osc = DFM1.ar(osc,
		LinLin.kr(SinOsc.kr(3), -1, 1, 1000, 8000)
		// LinLin.kr(SinOsc.kr(LinLin.kr(LFNoise2.kr(5), -1, 1, 3, 24)), -1, 1, 1000, 8000)
		// , XLine.kr(0.1, 10, 2 )
	);
	osc2 = SinOsc.ar(freq /2) * 0.2;
	sig = Mix.ar([osc]);
	sig = LPF.ar(sig, freq * LinLin.kr(SinOsc.kr(8), -1, 1, 2, 8));
	env = EnvGen.kr(Env.adsr(2, sustainLevel: 0.5, releaseTime: 1, curve:-2), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig !2);
}).add;

SynthDef(\sawPad, {
	|freq = 300, gate = 1, outBus = 0, amp = 1, cutoff =1000, cutoff2 = 3000, fdur = 3, release = 1|
	var sig, sig2, env;
	sig = VarSaw.ar(freq, width: LinLin.kr(LFTri.kr(LinLin.kr(SinOsc.kr(100), -1, 1, 2, 10)), -1, 1, 0.2, 1.5)) * 0.1;
	sig2 = SawDPW.ar([freq, freq * 1.0001]);
	sig = Mix.ar(Mix.ar(sig2));
	sig = DFM1.ar(sig,
		XLine.kr(cutoff, cutoff2, fdur)
		// LinLin.kr(SinOsc.kr(6), -1, 1, 1000, 4000)
	);
	// sig = BHiShelf.ar(sig, 1000, db: -18);
	// sig = MoogVCF.ar(sig, 1500, 0.3);
	// sig = LPF.ar(sig, LinLin.kr(SinOsc.kr(LinLin.kr(SinOsc.kr(LinLin.kr(SinOsc.kr(0.5), -1, 1, 1, 12)), -1, 1, 1, 12)), -1, 1, 1000, 5000));
	env = EnvGen.kr(Env.adsr(releaseTime: release), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\fmTest, {
	|freq = 300, gate = 1, amp = 1, outBus = 0|
	var sig, ctls, mod, env;
	ctls =
	[
		[freq, 0, 0.1],
		[freq * 7.midiratio, 0, 0.1],
		[LinLin.kr(SinOsc.kr(0.5), -1, 1, 300, 600), 0, 0.1],
		[0, 0, 0],
		[0, 0, 0],
		[0, 0, 0]
	];
	mod = [
		[Line.kr(0, 10, 2), 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0]
	];
	sig = FM7.ar(ctls, mod).slice([0, 1]);
	sig = BRF.ar(Mix.ar(sig), 120);
	env = EnvGen.kr(Env.adsr(releaseTime: 2), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig !2);
}).add;

SynthDef(\fm2, {
	|freq = 300, gate = 1, amp = 1, outBus = 0|
	var sig, ctls, mod, env;
	ctls =
	[
		[freq, 0, 0.4],
		[freq * 5.midiratio, 0, 0.05],
		[freq * 7.midiratio, 0, LinLin.kr(SinOsc.kr(6), -1, 1, 0, 0.2)],
		[0, 0, 0],
		[0, 0, 0],
		[0, 0, 0]
	];
	mod = [
		[0, 0, 0, 0, 0, 0],
		[0, XLine.kr(0.01, 3, 2), 0, 0, 0, 0],
		[XLine.kr(0.001, 8, 2), 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0],
		[0, 0, 0, 0, 0, 0]
	];
	sig = FM7.ar(ctls, mod);
	sig = BLowShelf.ar(sig, 800, db: -18);
	env = EnvGen.kr(Env.adsr(0.01, releaseTime: 2), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig !2)
}).add;

SynthDef(\pulsePad, {
	|freq = 200, gate = 1, outBus = 0, cutoff= 200, cutoff2 = 1200, fdur = 3, amp = 1, attack = 0.01, release = 1, decay = 0.3|
	var sig, sig2, env;
	sig = Pulse.ar(freq, width: 0.9);
	sig2 = SawDPW.ar(freq * LinExp.kr(LFNoise1.kr(2), -1, 1, 0.0001, 0.1).midiratio) * 0.3;
	sig = Mix.ar([sig, sig2]);
	// sig = DFM1.ar(sig, XLine.kr(500, 1500, 2));
	sig = MoogFF.ar(sig, XLine.kr(cutoff, cutoff2, fdur));
	env = EnvGen.kr(Env.adsr(attack, decay, releaseTime: release), gate: gate, doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\percTest, {
	|outBus = 0, tune = 1, distort = 4, cutoff1 = 5000, cutoff2 = 2000|
	var sig, click, env;
	sig = Pulse.ar(EnvGen.kr(Env([1000, 500, 100], [0.0001, 0.01], [-3, -2])) * tune
		, LinLin.kr(SinOsc.kr(distort), -1, 1, 0.1, 0.9)
	);
	sig = DFM1.ar(sig, XLine.kr(cutoff1, cutoff2, 0.001));
	click = Impulse.ar * EnvGen.kr(Env([0, 1, 0], [0.0001, 0.001])).clip2(0.2);
	env = EnvGen.kr(Env.perc(0.001, 0.6), doneAction: 2);
	sig = Mix.ar([click, sig]) * env;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\prophet2, {
	|amp = 1, t_gate = 1, freq = 440, cutoff = 1500, fdur = 1, rq = 0.3,  attack = 1, decay = 2, release = 1, outBus = 0|
	var snd, env;
	snd = Mix.ar([
		Pulse.ar(freq, (SinOsc.kr(1) + 1.2) * 0.1),
		Pulse.ar(freq, (SinOsc.kr(0.3) + 1.2) * 0.4),
		Pulse.ar(freq, (LFTri.kr(0.4) + 1.2) * 0.4),
		Pulse.ar(freq, (LFTri.kr(0.4, 0.19) + 1.2) * 0.4),
		Pulse.ar( freq/ 2, (LFTri.kr(LFNoise2.kr(0.2) + 2) + 1.2) * 0.8) * 0.5
	]);
	snd = Normalizer.ar(snd);
	env = EnvGen.kr(Env.adsr(attack, decay, 0.7, release), t_gate, doneAction: 2);
	snd = RLPF.ar(snd * env, Lag2.kr(cutoff, fdur), rq);
	Out.ar(outBus, snd * amp !2);
}).add;

SynthDef(\acidTest, {
	|freq= 100, env = 1, accent = 1, gate = 1, outBus = 0, cutoff = 5000|
	var w, p4, nt, ds,fr, lagfr, fenv, et,ac, acInt,gt;
	var wv=0.1; //0-1, mix between saw and pulse
	var ampComp, accMod=1;

	lagfr = LagUD.ar(K2A.ar(freq),0.39, 0.09);
	lagfr = lagfr * LFDNoise3.ar(0.3,0.0156,1);
	w = DPW4Saw.ar(lagfr, 0.5 + (wv ** 4 * 550)).softclip;
	ampComp = (1.05-wv) ** 6 + 1;
	w = w * 0.9* ampComp * LFDNoise3.ar(0.5,0.04,1);
	acInt = LPF.ar(K2A.ar(accent) * 12, 1);
	fenv = Decay2.ar(env * 0.16 + (acInt * 0.02 * accMod) , 0.01, 0.12)  * LFDNoise3.ar(0.2,0.003,1);
	w = RLPFD.ar(w, cutoff + (15000-cutoff * fenv)) * 2;
	// w = RLPFD.ar(w, cutoff + (15000-cutoff * fenv), 0.6 + (acInt * 1.2 * accMod), 0.5) * 2;
	w = LeakDC.ar(w, 0.995);
	w = w + (HPF.ar(w,400,10).softclip * 0.04);
	// w = w * Lag.ar(K2A.ar(gate), 0.001);
	Out.ar(outBus ,w * 0.1 !2);
}).add;

SynthDef(\bpfsaw2, {
	|freq = 500, atk = 2, sus = 0, rel = 3, c1 = 1, c2 = -1, detune = 0.2, pan = 0, cfhzmin = 0.1, cfhzmax = 0.3, cfmin = 500, cfmax = 2000, rqmin = 0.1, rqmax = 0.2, lsf = 200, ldb = 0, hsf = 6000, hdb = 0, amp = 1, rs = 0.5, outBus = 0|
	var env, sig, noise;
	env = EnvGen.kr(Env.new([0, 1, 1, 0], [atk, sus, rel], [c1, 0, c2]), doneAction: 2);
	freq = freq * (LFNoise0.kr(0.5) * detune).midiratio;
	sig = Saw.ar(freq!2);
	noise = LinExp.kr(LFNoise1.kr(LinExp.kr(LFNoise1.kr(4), -1 , 1, cfhzmin, cfhzmax)), -1, 1, cfmin, cfmax);
	sig = BPF.ar(sig, noise, LinExp.kr(LFNoise1.kr(0.1), -1, 1, rqmin, rqmax));
	sig = BLowShelf.ar(sig, lsf, rs, ldb);
	sig = BHiShelf.ar(sig, hsf, rs, hdb);
	sig = Balance2.ar(sig, sig);
	sig = sig * env * amp;
	Out.ar(outBus, sig);
}).add;
)




(
~stutter = { |snd, reset, fragmentlength, rate = 1.0, maxdelay = 10|
    var phase, fragment, del;
	phase = Sweep.ar(reset);
    fragment = { |ph| (ph - Delay1.ar(ph)) < 0 + Impulse.ar(0) }.value(phase / fragmentlength % 1);
	del = Latch.ar(phase, fragment) + ((fragmentlength - Sweep.ar(fragment)) * (rate - 1));
    DelayC.ar(snd, maxdelay, del);
};
SynthDef(\stuttertest, {
    |outBus = 0, audioBus = 0, t_reset = 0, fragmentlength = 0.5, rate = 1, decay = 10, play = 1|
    var snd;
	snd = In.ar(audioBus, 2);
	// snd = PlayBuf.ar(1, buf, BufRateScale.kr(buf), loop: 1);
    snd = ~stutter.(snd, t_reset, fragmentlength, rate, decay);
	ReplaceOut.ar(outBus, Select.ar(play, [In.ar(audioBus, 2), snd!2]));
}).add;
SynthDef(\scramble, {
	|outBus = 0, audioBus = 0, rate = 13, delaymin = 0, delaymax = 1|
    var snd;
    snd = In.ar(audioBus, 2);
    snd = DelayC.ar(snd, 1.0, LFNoise0.ar(rate).range(delaymin, delaymax));
	Out.ar(outBus, snd!2 * 0.3);
}).add;
SynthDef(\onsetDelay, {
	|outBus = 0, audioBus = 0, onsetBus = 0, delay = 0.2, delayMax = 0.2|
    var snd, reset;
    snd = In.ar(audioBus, 2);
	reset = Onsets.kr(FFT(LocalBuf(512), In.ar(onsetBus)), 0.5);
    snd = ~stutter.(snd, reset, 0.05);
    snd = DelayC.ar(snd, delayMax, delay);
	Out.ar(outBus, snd!2 * 0.3);
}).add;

)
