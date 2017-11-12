(
{
	var chime, freqSpecs, burst, totalHarm = 10;
	var burstEnv, att = 0, burstLength = 0.1;
	freqSpecs = `[
		{rrand(1, 30)*100}.dup(totalHarm),
		{rrand(0.1, 0.9)}.dup(totalHarm).normalizeSum,
		{rrand(1.0, 3.0)}.dup(totalHarm)];
	burstEnv = Env.perc(0, burstLength);
	burst = PinkNoise.ar(EnvGen.kr(burstEnv, gate: Impulse.kr(1)));
	Klank.ar(freqSpecs, burst)!2*MouseX.kr(0.1, 0.8)
}.scope(1)
)

(
{
	var chime, freqSpecs, burst, totalHarm = 10;
	var burstEnv, att = 0, burstLength = 0.1;
	freqSpecs = `[
		{rrand(100, 1200)}.dup(totalHarm), //freq array
		{rrand(0.3, 1.0)}.dup(totalHarm).normalizeSum.round(0.01), //amp array
		{rrand(2.0, 4.0)}.dup(totalHarm)]; //decay rate array
	burstEnv = Env.perc(0, burstLength); //envelope times
	burst = PinkNoise.ar(EnvGen.kr(burstEnv, gate: Impulse.kr(1))); //Noise burst
	Klank.ar(freqSpecs, burst)!2*MouseX.kr(0.1, 0.8)
}.scope(1)
)


(
{
	var totalInst, totalPartials, baseFreq, ampControl, chimes, cavern;
	totalInst = 5; //Total number of chimes
	totalPartials = 12; //Number of partials in each chime
	baseFreq = rrand(200, 1000); //Base frequency for chimes
	chimes =
	Mix.ar(
		{
			Pan2.ar(
				Klank.ar(`[
					{baseFreq*rrand(1.0, 12.0)}.dup(totalPartials),
					Array.rand(totalPartials, 0.3, 0.9),
					Array.rand(totalPartials, 0.5, 6.0)],
					Decay.ar(
						Dust.ar(2, 0.02), //Times per second, amp
						0.001, //decay rate
						PinkNoise.ar //Noise
			)), 1.0.rand2) //Pan position
		}.dup(totalInst)
	);
	cavern =
	Mix.ar(
		{
			var base;
			base = exprand(50, 500);
			Klank.ar(
				`[ //frequency, amplitudes, and decays
					{rrand(1, 24) * base *
						rrand(1.0, 1.1)}.dup(totalPartials),
					Array.rand(10, 1.0, 5.0).normalizeSum
				],
				GrayNoise.ar( [rrand(0.03, 0.1), rrand(0.03, 0.1)])
			)*max(0, LFNoise1.kr(3/rrand(5, 20), mul: 0.005))
	}.dup(5));
	cavern + chimes
}.play
)

// Cavern variation
(
{
	var totalPartials = 12;
	Mix.ar(
		{
			var base;
			base = exprand(50, 1000);
			Pan2.ar(
				Klank.ar(
					`[ //frequency, amplitudes, and decays
						{rrand(1, 24) * base *
							rrand(1.0, 1.1)}.dup(totalPartials),
						Array.rand(10, 1.0, 5.0).normalizeSum
					],
					GrayNoise.ar( rrand(0.03, 0.1))
				)*max(0, LFNoise1.kr(6, mul: 0.005)),
				LFNoise0.kr(1))
	}.dup(5));
}.play
)
(
// Rotating wheels
{
	var totalPartials = 4;
	Mix.ar(
		{
			var base;
			base = exprand(50, 10000);
			Pan2.ar(
				Klank.ar(
					`[ //frequency, amplitudes, and decays
						{rrand(1, 24) * base *
							rrand(1.0, 1.1)}.dup(totalPartials),
						Array.rand(10, 1.0, 5.0).normalizeSum
					],
					GrayNoise.ar( rrand(0.03, 0.1))
				)*max(0, SinOsc.kr(6/rrand(1, 10), mul: 0.005)),
				LFNoise1.kr(1))
	}.dup(8));
}.play
)

