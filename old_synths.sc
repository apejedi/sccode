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
}).add;

SynthDef(\acidBass, {
	|note = 50, amp=1, lg=1, dur=1, attack=0.001, outBus=0, reverb=1|
	var a,r,la,env1,env2, sig, sig1;
	a = attack * dur;
	r = 0.04 * dur;
	la = Lag.kr(note, (1 - Line.kr(lg, lg, 0.001)) * 0.12);
	env1 = EnvGen.kr(Env.perc(a, dur), doneAction: Done.freeSelf);
	env2 = EnvGen.kr(Env.adsr(a, dur, 0, r, 70, -4));
	sig = (LFPulse.ar(note.midicps, 0, 0.51) * 2) - 1;
	sig = RLPF.ar(sig, (note + env2).midicps, 0.3);
	sig1 =  sig * env1 * amp;
	//sig = BPF.ar(sig1, 3500);
	//sig = Select.ar(reverb, [sig, FreeVerb.ar(sig, 1, 0.95, 0.15)]);
	//sig = (EnvGen.kr(Env.new([0.02, 0.3, 0.02], [0.4, 0.01], [3 -4], 1)) * sig) + sig;
	//sig = HPF.ar(sig * 1.2, 40);
	//sig = Limiter.ar(sig, 1, 0.02);
	Out.ar(outBus, sig1!2);
}).add;

SynthDef(\bass2, {
	|atk=0.001, fDur=0.001, echo=1, decay=0.6, amp=1, freq=80, cutoff=2000, cutoff2=2000, outBus=0|
	var sig;
	sig = Select.ar(echo, [DC.ar(1), Decay2.ar(Impulse.ar(atk/2), atk, decay)]) * Mix.ar(Pulse.ar([freq, (freq + 1)], 0.3), amp);
	sig = MoogFF.ar(sig, XLine.kr(cutoff, cutoff2, fDur), 3);
	sig = sig * EnvGen.kr(Env.perc(atk, decay), doneAction:2) * amp;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\sample, {
	|num, outBus=0|
	var sig = PlayBuf.ar(1, num, doneAction: 2);
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\oKick, {
	|amp = 0.5, outBus=0|
	var env0, env1, env1m, o;
	env0 = EnvGen.kr(Env.new([0.5, 1, 0.5, 0], [0.005, 0.06, 0.26], [-4, -2, -4]), doneAction: 2);
	env1 = EnvGen.kr(Env.new([110, 59, 29], [0.005, 0.29], [-4, -5]));
	env1m = env1.midicps;
	o = LFPulse.ar(env1m, 0, 0.5) - 0.5;
	o = o + WhiteNoise.ar;
	o = env0 * LPF.ar(o, env1m * 1.5);
	o = o + (env0 * SinOsc.kr(env1m, 0.5));
	o = o * 1.2;
	o = o.clip2(1);
	o = o * amp * 0.5;
	Out.ar(outBus, o!2);
}).add;

SynthDef(\clipMixer, {
	|audioBus=10, volume=1, startRelease=0|
	var source;
	source = In.ar(audioBus, 2) * volume;
	DetectSilence.ar(Select.ar(startRelease, [DC.ar(1), source]), doneAction: 14);
	Out.ar(0, source);
}).add;


SynthDef(\bassSynth, {
	|freq = 200, attack = 0.1, amp=1, release=1, detune=3, bwr=1, outBus=0|
	var freqV, sig, sig2, env;
	freqV = LinExp.kr(LFNoise0.kr(2), -1, 1, 0.1, detune) + freq;
	sig =  VarSaw.ar(freqV!2, 0, 1);
	sig2 = Resonz.ar(sig, freq, bwr);
	env = EnvGen.kr(Env.perc(attack, release), doneAction: 2);
	sig = sig + sig2;
	sig = sig * amp * env;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\klangTest, {
	|freq = 440, amp=1, atk=0.1, dur=3, outBus=0|
	var sig, env;
	sig = Klang.ar(`[[freq * 0.5, freq * 2/3, freq, freq * 4/3, freq * 2, freq * 5/2],
		[0.2, 0.1, 0.4, 0.1, 0.1, 0.1], [1/6, 1/6, 1/6, 1/6, 1/6, 1/6]]);
	env = EnvGen.kr(Env.perc(atk * dur, (1- atk) * dur), doneAction: 2);
	sig = sig * env * amp;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\sin, {
	|freq = 300, dur = 2, amp = 1, outBus = 0, atk = 0.01|
	var env, sig;
	env = EnvGen.kr(Env.new([0.1, 1, 0], [(atk * dur), (dur * (1 - atk))], 'welch'), doneAction: 2);
	sig = env * Mix.ar([SinOsc.ar(freq), SinOsc.ar(freq + 19.midicps) * 0.08, SinOsc.ar(freq - 12.midicps) * 0.04]) * amp;
	Out.ar(outBus, sig!2);
}).add;

SynthDef(\bowed, {
	|freq = 440, outBus = 0, atk = 0.2, ampB= 0.5, start= 0.1, end = 0.7, force= 1, dur= 2, c1= 0.25, c3= 31, amp 1|
	var vib, rest, env, pos, son;
	vib = (Gendy1.kr(1, 1, 1, 1, 0.1, 4) * 0.003) + 1;
	rest = 1 - atk;
	env = EnvGen.kr(Env.new([0, 0.7, 1, 0.8, 0], [(dur * atk), (dur * 0.2 * rest), (dur * 0.4 * rest), (dur * rest * 0.4)]), doneAction: 2);
	pos = Line.kr(start, end, dur);
	son = DWGBowedTor.ar(freq * vib, ampB, force, 1, pos, 0.1, c1, c3, impZ:2, mistune: 8, c3tor: 10000, c3: 10);
	son = DWGSoundBoard.ar(son);
	son = son * env * amp;
	Out.ar(outBus, son!2);
}).add;

SynthDef(\cs80, {
	|freq=880, amp=0.5, dur=3, atk=0.3, rq=0.5, cutoff=10000, dtune=0.002, vibrate=4, vibdepth=0.015, freqLag=0.1, outBus=0 |
	var env, sig, vib, att, rest;
	att = dur * atk;
	rest = dur - att;
	freq = Lag.kr(freq, freqLag);
	env = EnvGen.kr(Env.new([0, 0.5, 1, 0.001], [att, rest * 0.2, rest * 0.5, rest * 0.3]),  doneAction: 2);
	vib = LinLin.kr(SinOsc.kr(vibrate), -1, 1, -1 * vibdepth, vibdepth) + 1;
	freq = freq * vib;
	sig = Mix.ar(env * amp * Saw.ar([freq, freq * (1 + dtune)]));
	sig = RLPF.ar(sig, cutoff, rq);
	Out.ar(outBus, sig!2);
}).add;


)