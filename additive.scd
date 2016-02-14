{Saw.ar!2}.play


(
{
(
SinOsc.ar(400, mul: 1) + SinOsc.ar(800, mul: 1/2) +
SinOsc.ar(1200, mul: 1/3) + SinOsc.ar(1600, mul: 1/4) +
SinOsc.ar(2000, mul: 1/5) + SinOsc.ar(2400, mul: 1/6) +
SinOsc.ar(2800, mul: 1/7) + SinOsc.ar(3200, mul: 1/8) +
SinOsc.ar(3600, mul: 1/9) + SinOsc.ar(4000, mul: 1/10) +
SinOsc.ar(4400, mul: 1/11) + SinOsc.ar(4800, mul: 1/12)
)*0.1
!2}.scope
)

(
{
var speed = 28;
f = 300;
t = Impulse.kr(1/3);
Mix.ar([
SinOsc.ar(f*1, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/1),
SinOsc.ar(f*2, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/2),
SinOsc.ar(f*3, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/3),
SinOsc.ar(f*4, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/4),
SinOsc.ar(f*5, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/5),
SinOsc.ar(f*6, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/6),
SinOsc.ar(f*7, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/7),
SinOsc.ar(f*8, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/8),
SinOsc.ar(f*9, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/9),
SinOsc.ar(f*10, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/10),
SinOsc.ar(f*11, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/11),
SinOsc.ar(f*12, mul: LFNoise1.kr(rrand(speed, speed*2), 0.5, 0.5)/12)
])*0.5
!2}.scope(1)
)

(
{
f = 100;
t = Impulse.kr(1/3);
Mix.ar([
SinOsc.ar(f*1, mul: EnvGen.kr(Env.perc(0, 1.4), t)/1),
SinOsc.ar(f*2, mul: EnvGen.kr(Env.perc(0, 1.1), t)/2),
SinOsc.ar(f*3, mul: EnvGen.kr(Env.perc(0, 2), t)/3),
SinOsc.ar(f*4, mul: EnvGen.kr(Env.perc(0, 1), t)/4),
SinOsc.ar(f*5, mul: EnvGen.kr(Env.perc(0, 1.8), t)/5),
SinOsc.ar(f*6, mul: EnvGen.kr(Env.perc(0, 2.9), t)/6),
SinOsc.ar(f*7, mul: EnvGen.kr(Env.perc(0, 4), t)/7),
SinOsc.ar(f*8, mul: EnvGen.kr(Env.perc(0, 0.3), t)/8),
SinOsc.ar(f*9, mul: EnvGen.kr(Env.perc(0, 1), t)/9),
SinOsc.ar(f*10, mul: EnvGen.kr(Env.perc(0, 3.6), t)/10),
SinOsc.ar(f*11, mul: EnvGen.kr(Env.perc(0, 2.3), t)/11),
SinOsc.ar(f*12, mul: EnvGen.kr(Env.perc(0, 1.1), t)/12)
])*0.5
}.scope(1)
)


(
{
t = Impulse.kr(3);
Mix.ar(
SinOsc.ar(
[60, 64, 67, 71, 74, 78].midicps,
mul: EnvGen.kr(
Env.perc(0, 1),
t,
levelScale: 1/(1..6),
timeScale: rrand(1.0, 3.0).dup
)
)
)*[0.3, 0.3]
}.scope(1)
)

{ SinOsc.ar(LFNoise0.ar(2).poll(1, label: \LFNoise), 0, 0.2) }.play;

(
{
var gate, fund;
gate = Impulse.kr(1);
//fund = MouseX.kr(50, 1000);
fund = 55.cpsmidi;
	Mix.ar(
Array.fill(16,
{arg counter;
var partial;
partial = counter + 1;
SinOsc.ar(fund*partial) *
EnvGen.kr(Env.adsr(0, 0, 1.0, TRand.kr(0.2, 2.0, gate)),
gate, 1/partial)
})
)*0.2 //overall volume
!2}.scope(1)
)

~getMaj = {|fund = 36|
	[fund, fund + 4, fund + 7]
};

~getMaj(50).value;

(
{
var harmonics = 16, fund = 72;
Mix.fill(harmonics,
{ arg count;
Pan2.ar(
FSinOsc.ar(
fund * (count + 1), // calculates each harmonic
mul: FSinOsc.kr(rrand(1/3, 1/6), mul: 0.5, add: 0.5 )),
1.0.rand2)
}
) / (2*harmonics)
}.play;
)

(
{Mix.ar(
SinOsc.ar(
[72, 135, 173, 239, 267, 306, 355, 473, 512, 572, 626],
0, //phase
[0.25, 0.11, 0.12, 0.04, 0.1, 0.15, 0.05, 0.01, 0.03, 0.02, 0.12]
))!2}.scope(1)
)

(
{
RandSeed.kr(1, 1956);
SinOsc.ar(LFNoise0.kr(7, 12, 72).midicps, mul: 0.5)
}.play
)

(
{
RandSeed.kr(Impulse.kr(1/5), 1956);
SinOsc.ar(LFNoise0.kr(7, 12, 72).midicps, mul: 0.5)
}.play
)


( // Let it run for .a while, the strikes are random
{
var trigger, partials = 12;
trigger = Dust.kr(7);
Pan2.ar(
Mix.ar(
{
SinOsc.ar(exprand(50.0, 4000)) *
EnvGen.kr(
Env.perc(0, rrand(0.2, 3.0)),
trigger,
1.0.rand
)
}.dup(partials)
)/partials,
1.0.rand2
)
}.play
)

