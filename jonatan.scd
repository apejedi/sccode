n=LFNoise1;Ndef(\x,{a=SinOsc.ar(65,Ndef(\x).ar*n.ar(0.1,3),n.ar(3,6)).tanh;9.do{a=AllpassL.ar(a,0.3,{0.2.rand+0.1}!2,5)};a.tanh}).play
(
play{
    i=Impulse.ar(_);
    h = LPF.ar(WhiteNoise.ar(Decay.ar(Impulse.ar([1,1/7],[0.5,0.75]).sum,0.1)!2),12000);
    Limiter.ar(
    (z=Pluck.ar(Saw.ar(4),i.(1),1,1,6/7,SinOsc.ar(PulseCount.ar(i.(11),i.(5)).cos%[3,4]*120),2.5).sin)
    + LPF.ar(HPF.ar(FreeVerb.ar(z.mean,1,0.999,0.2)*0.3,750)*Line.kr(0,1,16),2500)
    + HPF.ar(Ringz.ar(LPF.ar(Impulse.ar([0.5,1],[1/6,0]).sum,3000),40,0.8,3).sin*3,40).tanh.madd(0.6)
    + h + RHPF.ar(CombL.ar(h*0.4,1,5/8,10)*Line.kr(0,1,8),10000,0.5)
     )
}
)
