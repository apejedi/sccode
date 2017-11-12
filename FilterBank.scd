(
x ={
	Klank.ar(
		`[[100, 200, 300, 400, 500, 600, 700, 800, 900, 1000], //freq array
			[0.05, 0.2, 0.04, 0.06, 0.11, 0.01, 0.15, 0.03, 0.15, 0.2]],
		PinkNoise.ar(MouseX.kr(0.01, 0.1)))
	!2}.scope(1)
)

x.free

(
{
	Klank.ar(
		`[Array.series(10, 50, 50),
			Array.series(10, 1.0, -0.1)],
		ClipNoise.ar(0.01)
	)
	!2}.scope(1)
)

(
{
	Klank.ar(
		`[{exprand(60, 10000)}.dup(15)],
		PinkNoise.ar(0.005)
	)
	!2}.scope(1);
)


(
{
	var burstEnv, att = 0, burstLength = 0.1, signal; //Variables
	burstEnv = Env.perc(0, burstLength); //envelope times
	signal = WhiteNoise.ar(EnvGen.kr(burstEnv, gate: Impulse.kr(1))); //Noise burst
	signal!2;
}.play
)

