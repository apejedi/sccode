( //double click to select the entire example
SynthDef("PureTone",
{arg justFreq = 440, equalFreq = 440;
Out.ar(0, SinOsc.ar([justFreq, equalFreq], mul: 0.4)
*EnvGen.kr(Env.perc(0, 1), doneAction:2));
}).load(s);
Task({
var jfreq = 440, efreq = 69, next = 6, equalInt, justInt;
equalInt = [-10, -8, -7, -5, -3, -1,
0, 2, 4, 5, 7, 9, 11];
justInt = [9/16, 5/8, 2/3, 3/4, 5/6, 15/16, 1/1,
9/8, 5/4, 4/3, 3/2, 5/3, 15/8];
{
[equalInt[next], justInt.at(next).round(0.01)].post;
Synth("PureTone", [\justFreq, jfreq.round(0.01),
\equalFreq, efreq.midicps.round(0.01)].postln);
next = 13.rand;
jfreq = jfreq*justInt.at(next);
efreq = efreq + equalInt.at(next);
if(jfreq < 100, {jfreq = jfreq*2; efreq = efreq + 12});
if(jfreq > 1000, {jfreq = jfreq/2; efreq = efreq - 12});
[0.125, 0.125, 0.124, 0.25, 0.5, 1].choose.wait
}.loop;
}).play(SystemClock);
)


// Same example with just a scale.
( //double click to select the entire example
SynthDef("PureTone",
{arg justFreq = 440, equalFreq = 440;
Out.ar(0, SinOsc.ar([justFreq, equalFreq], mul: 0.4)
*EnvGen.kr(Env.perc(0, 1), doneAction:2));
}).load(s);
Task({
var jfreq = 440, efreq = 69, next = 0, equalInt, justInt;
equalInt = [-12, 2, 2, 1, 2, 2, 3];
justInt = [1/2, 9/8, 9/8, 16/15, 9/8, 9/8, 6/5];
{
[equalInt.wrapAt(next), justInt.wrapAt(next).round(0.01)].post;
Synth("PureTone", [\justFreq, jfreq.round(0.01),
\equalFreq, efreq.midicps.round(0.01)].postln);
next = next + 1;
jfreq = jfreq*justInt.wrapAt(next);
efreq = efreq + equalInt.wrapAt(next); 0.25.wait
}.loop;
}).play(SystemClock);
)


// Free just intonation only
( //double click to select the entire example
SynthDef("PureTone",
{arg justFreq = 440, equalFreq = 440;
Out.ar(0, SinOsc.ar([justFreq, equalFreq], mul: 0.4)
*EnvGen.kr(Env.perc(0, 1), doneAction:2));
}).load(s);
Task({
var jfreq = 440, efreq = 69, next = 0, equalInt, justInt;
equalInt = [-12, 2, 2, 1, 2, 2, 3];
justInt = [1/2, 9/8, 9/8, 16/15, 9/8, 9/8, 6/5];
{
[equalInt.wrapAt(next), justInt.wrapAt(next).round(0.01)].post;
Synth("PureTone", [\justFreq, jfreq.round(0.01),
\equalFreq, jfreq.round(0.01)].postln);
next = next + 1;
jfreq = jfreq*justInt.wrapAt(next);
efreq = efreq + equalInt.wrapAt(next); 0.25.wait
}.loop;
}).play(SystemClock);
)
