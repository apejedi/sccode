(

SynthDef(\bpfsaw, {
	|freq = 360, dur = 1, atk = 0.3, detune = 0, rq = 0.2, amp = 1, pan = 0, outBus=0|
	var env, sig;
	env = EnvGen.kr(Env.perc(atk * dur, ((dur - atk) * dur)), doneAction: 2);
	sig = SyncSaw.ar(freq + (detune * freq));
	sig = BPF.ar(sig, freq, rq) * env * amp;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\piano, {
	|freq = 244, amp=1, dur=1, vel=100, decay=0.8, release=0.8, hard=0.8, velhard=0.8, muffle=0.8, velmuff=0.8, velcurve=0.8, stereo= 0.2, tune=0.5, random=0.1, stretch=0.1, sustain=0.1|
	var sig,env;
	env = EnvGen.kr(Env.perc(1/vel, dur), doneAction: 2);
	sig = MdaPiano.ar(freq, 1, vel, decay, release, hard, velhard, muffle, velmuff, velcurve, stereo, tune, random, stretch, sustain, 1);
	sig  =  sig * env * amp;
	Out.ar(0, sig!2);
}).add;

/*SynthDef(\flute ,  {
	 |freq = 244, amp = 0.5, attack=0.1, decay=0.3, sustain=0.4, release=0.2, dur=3, trill=0|
	 var a, d, s, r, env, mod1, mod3, sig;
	 a = attack * dur;
	 d = decay * dur;
	 s = sustain * dur;
	 r = release * dur;
	env = EnvGen.kr(Env.adsr(a, d, s, r), doneAction: 2, gate: EnvGen.kr(EnvGate.new(fadeTime: dur)));
	 mod1 = EnvGen.kr(Env.linen(SinOsc.kr(6), -1, 1, (freq * 0.99), (freq * 1.01)));
	 mod3 = EnvGen.kr(Env.linen(SinOsc.kr(trill), -1, 1, 0.1, 1));
	 sig = env * SinOsc.ar([freq, mod1]);
	 sig = sig * amp * mod3;
	 Out.ar(0, sig!2);
	 }).add;*/

SynthDef(\bpfsaw2, {
	 |freq = 500, atk = 2, sus = 0, rel = 3, c1 = 1, c2 = -1, detune = 0.2, pan = 0, cfhzmin = 0.1, cfhzmax = 0.3, cfmin = 500, cfmax = 2000, rqmin = 0.1, rqmax = 0.2, lsf = 200, ldb = 0, hsf = 6000, hdb = 0, amp = 1, rs = 0.5, outBus = 0|
	 var env, f, noise, sig;
	 env = EnvGen.kr(Env.new([0, 1, 1, 0], [atk, sus, rel], [c1, 0, c2]), doneAction: 2);
	 f = freq * (LFNoise0.kr(0.5) * detune).midiratio;
	 noise = LinExp.kr(LFNoise1.kr(LinExp.kr(LFNoise1.kr(4), -1, 1, cfhzmin, cfhzmax)), -1, 1, cfmin, cfmax);
	 sig = Saw.ar([f, f]);
	 sig = BPF.ar(sig, noise, LinExp.kr(LFNoise1.kr(0.1)), -1, 1, rqmin, rqmax);
	 sig = BLowShelf.ar(sig, lsf, rs, ldb);
	 sig = BHiShelf.ar(sig, hsf, rs, hdb);
	 sig = Balance2.ar(sig[0], sig[1]);
	 sig = sig * env * amp;
	Out.ar(outBus, sig!2);
	 }).add;

SynthDef(\klangTest, {
	 |freq=440, amp=1, atk=0.1, dur=3, outBus = 0|
	 var sig, env;
	 sig = Klang.ar(`[[freq * 0.5, freq * 0.6, freq, freq * 1.3, freq * 2, freq * 2.5], [0.2, 0.1, 0.4, 0.1, 0.1, 0.1]]);
	 env = EnvGen.kr(Env.perc(atk * dur, dur * (1 - atk)), doneAction: 2);
	 sig = sig * env * amp;
	 Out.ar(outBus, sig!2);
	 }).add;

SynthDef(\bass, {
	 |freq=200 attack=0.1 amp=1 release=1 detune=3 bwr=1 outBus=0|
	 var sig, sig2, freqv, env;
	 freqv = freq + LinExp.kr(LFNoise0.kr(2), -1, 1, 0.1, detune);
	 sig = VarSaw.ar(freqv, 0, 1);
	 sig2 = 0.02 * Saw.ar(freq);
	 sig = Resonz.ar(sig, freq, bwr);
	 env = EnvGen.kr(Env.perc(attack, release), doneAction: 2);
	 sig = sig + sig2;
	 sig = sig * amp * env;
	 Out.ar(outBus, sig!2);
	 }).add;

SynthDef(\risePad , {
	 |freq = 440, t = 3, attack = 0.5, amp = 1, detune = 0.1, rq = 0.5, outBus = 0, gate=1|
	 var dur, a, s, r, sig, env;
	 dur = 1 - attack;
	 a = attack * t;
	 s = dur * 0.3 * t;
	 r = 0.7 * dur * t;
	 env = EnvGen.kr(Env.adsr(a , 0, s, r), gate, doneAction: 2);
	 freq = freq * (LFNoise0.kr(0.5) * detune).midiratio;
	 sig = Blip.ar(freq, 3);
	 sig = BPF.ar(sig, freq, rq) * 0.4;
	 sig = sig * env * amp;
	 Out.ar(outBus, sig!2);
	 }).add;

SynthDef(\riseFall, {
	 |freq=440, t = 4, amt= 0.3, amp = 0.8, delay = 0.3, decay = 0.5, damp = 0.5, lsf = 400, ldb= 0, outBus = 0|
	 var f_env, src, signal, k, distort, gate, compressor, dampener, reverb, echo, sig;
	 f_env = EnvGen.kr(Env.perc(t, t), doneAction: 2);
	 src = Saw.ar([freq, freq * 1.01]);
	 signal = RLPF.ar(0.3 * src, ((0.6 * freq) + (f_env * 2 * freq) + 0.2));
	 k =  (2 * amt) / (1 - amt);
	 distort = ((k + 1) * signal) / (1 + (k * signal.abs));
	 gate = Pulse.ar(2 * (1 + SinOsc.kr(0.05)));
	 compressor = Compander.ar(distort, gate, 0.01, 1, 0.5, 0.01, 0.01);
	 dampener = (1 + (0.5 * SinOsc.kr(damp)));
	 reverb = FreeVerb.ar(compressor, 0.5, 0.5, dampener);
	 echo = CombN.ar(reverb, delay, delay, decay);
	 echo = BLowShelf.ar(echo, lsf, 0.5, ldb);
	 sig = echo * amp;
	 Out.ar(outBus, sig!2);
	 }).add;


SynthDef(\prophet, {
	|amp = 1, freq = 440, cutoff = 1500, rq = 0.3, attack = 1, decay = 2, outBus = 0|
	var snd, env;
	snd = Pan2.ar(Mix.new([Pulse.ar(freq, (0.1 * (1.2 + SinOsc.kr(1)))),
		Pulse.ar(freq, (0.8 * ((1.2 + SinOsc.kr(0.3)) / 0.7) * 2)),
		Pulse.ar(freq, (0.8 * ((1.2 + LFTri.kr(0.4)) / 2))),
		Pulse.ar(freq, (0.8 * ((1.2 + LFTri.kr(0.4, 0.19)) / 2))),
		(0.5 * Pulse.ar(freq / 2, (0.8 * ((1.2 + LFTri.kr(2 + LFNoise2.kr(0.2))) / 2))))]));
	snd = Normalizer.ar(snd);
	env = EnvGen.kr(Env.perc(attack, decay), doneAction: 2);
	snd = RLPF.ar(snd * env * snd, cutoff, rq);
	Out.ar(outBus, amp * snd);
}).add;

SynthDef(\sin, {
	|freq = 240, atk = 0.01, release= 1, outBus = 0, amp = 1|
	var sig, env;
	sig = SinOsc.ar(freq);
	env = EnvGen.kr(Env.perc(atk, release), doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig!2);
}).add
)