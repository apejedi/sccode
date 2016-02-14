(
{
var trigger, fund;
trigger = Dust.kr(3/7);
fund = rrand(100, 400);
Mix.ar(
Array.fill(16,
{arg counter;
var partial;
partial = counter + 1;
Pan2.ar(
SinOsc.ar(fund*partial) *
EnvGen.kr(Env.adsr(0, 0, 1.0, 5.0),
trigger, 1/partial
) * max(0, LFNoise1.kr(rrand(5.0, 12.0))), 1.0.rand2)
})
)*0.5 //overall volume
}.play
)


//Several of the above mixed down
(
{
var trigger, fund, flashInst;
flashInst = Array.fill(5,
{
trigger = Dust.kr(3/7);
fund = rrand(100, 400);
Pan2.ar(
Mix.ar(
Array.fill(16,
{arg counter;
var partial;
partial = counter + 1;
SinOsc.ar(fund*partial) *
EnvGen.kr(Env.adsr(0, 0, 1.0, 5.0),
trigger, 1/partial
) * max(0, LFNoise1.kr(rrand(5.0, 12.0)))
})
)*0.2,
1.0.rand2)
});
Mix.ar(flashInst)*0.6
}.play
)



// Gaggle of sines varations
(
{
var harmonics = 16, fund = 50, speeds;
speeds = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]/5;
Mix.fill(harmonics,
{ arg count;
Pan2.ar(
FSinOsc.ar(
fund * (count + 1),
mul: max(0, FSinOsc.kr(speeds.wrapAt(count)))),
1.0.rand2)
}
) / (2*harmonics)
}.play;
)



(
{
var harmonics = 16, fund, speeds;
speeds = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]/20;
fund = (MouseX.kr(0, 36).round(7) + 24).midicps;
Mix.fill(harmonics,
{ arg count;
Pan2.ar(
FSinOsc.ar(
fund * (count + 1),
mul: max(0, FSinOsc.kr(speeds.choose))),
1.0.rand2)
}
) / (2*harmonics)
}.play;
)



// Use mouse to change fundamental
(
{
var harmonics = 16, fund;
fund = (MouseX.kr(0, 36).round(7) + 24).midicps;
Mix.fill(harmonics,
{ arg count;
Pan2.ar(
FSinOsc.ar(
fund * (count + 1),
mul: max(0, FSinOsc.kr(rrand(1, 1/3), mul: 20).softclip)),
1.0.rand2)
}
) / (2*harmonics)
}.play;
)
(
{
var harmonics = 16;
Mix.fill(harmonics,
{ arg count;
Pan2.ar(
FSinOsc.ar(
exprand(100, 2000),
mul: max(0, FSinOsc.kr(rrand(1/3, 1/6))*rrand(0.1, 0.9))),
1.0.rand2)
}
139
) / (2*harmonics)
}.play;
)
















// Dissipating and converging gongs illustrates how a patch can be built
// from duplicating one idea; classic additive synthesis. It also shows
// how additive synthesis can be used to control each harmonic.
// Listen in stereo to hear the harmonics diverge.
(
{
var dur = 6, base, aenv, fenv, out, trig;
base = Rand(40, 100);
trig = SinOsc.ar(1/10);
out = Mix.fill(15,{
var thisDur;
thisDur = dur * rrand(0.5, 1.0);
aenv = EnvGen.kr(Env.perc(0, thisDur), trig);
fenv = EnvGen.kr(Env.new([0, 0, 1, 0], [0.25*thisDur, 0.75*thisDur, 0]), trig);
Pan2.ar(SinOsc.ar( Rand(base, base * 12) *
LFNoise1.kr(10, mul: 0.02 * fenv, add: 1), // freq
mul: aenv // amp
), ([1, -1].choose) * fenv)
}) * 0.05;
out
}.play(s);

{
var dur = 6, base, aenv, fenv, out, trig, detune;
base = Rand(40, 60);
detune = 0.1; // increase this number to detune the second bell
trig = SinOsc.ar(1/10, pi);
out = Mix.fill(15,
{ arg count;
var thisDur;
thisDur = dur * rrand(0.5, 1.0);
aenv = EnvGen.kr(Env.perc(0, thisDur), trig);
fenv = EnvGen.kr(Env.new([1, 1, 0, 1], [0.05*thisDur, 0.95*thisDur, 0]), trig);
Pan2.ar(SinOsc.ar( base*(count+1+ detune.rand) *
LFNoise1.kr(10, mul: 0.02 * fenv, add: 1), // freq
mul: aenv // amp
), ([1, -1].choose) * fenv)
}) * 0.05;
out
}.play(s);
)


// Decaying bell
(
{
var aenv, fenv, out, trig, dur, base;
dur = rrand(1.0, 6.0);
base = exprand(100, 1000);
trig = Impulse.kr(1/6);
out = Mix.ar(
Array.fill(15,{
arg count;
var thisDur;
thisDur = dur * rrand(0.5, 1.0);
aenv = EnvGen.kr(
Env.new([0, 1, 0.4, 1, 0], [0, 0.5, 0.5, 0]), trig,
timeScale: thisDur);
fenv = EnvGen.kr(
Env.new([0, 0, 0.5, 0.5, 0], [0.25, 0.5, 0.25, 0]),
trig, timeScale: thisDur);
Pan2.ar(SinOsc.ar( Rand(base, base * 12) *
LFNoise1.kr(10, mul: 0.1 * fenv, add: 1), // freq
mul: aenv // amp
), ([1, -1].choose) * fenv)
})
) * EnvGen.kr(Env.linen(0, dur, 0), Impulse.kr(6), timeScale: dur,
levelScale: 0.05, doneAction: 2);
out*0.3;
}.play;
)
