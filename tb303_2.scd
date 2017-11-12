
(

SynthDef("sc303", { arg out=0, freq=440, wave=0, ctf=100, res=0.2,
		sus=0, dec=1.0, env=1000, gate=1, vol=0.2;
	var filEnv, volEnv, waves;

	// can't use adsr with exp curve???
	//volEnv = EnvGen.ar(Env.adsr(1, 0, 1, dec, vol, 'exp'), In.kr(bus));
	volEnv = EnvGen.ar(Env.new([10e-10, 1, 1, 10e-10], [0.01, sus, dec], 'exp'), gate);
	filEnv = EnvGen.ar(Env.new([10e-10, 1, 10e-10], [0.01, dec], 'exp'), gate);

	waves = [Saw.ar(freq, volEnv), Pulse.ar(freq, 0.5, volEnv)];

	Out.ar(out,
		RLPF.ar( Select.ar(wave, waves), filEnv * env, res).dup * vol
		//RLPF.ar( Select.ar(wave, waves), ctf + (filEnv * env), res).dup * vol

	);
}).send(s);
x.free
)
x = 1003
(
s.sendBundle(s.latency, ["n_set", ~node, 'gate', 0, 'freq', 200]);
//~node.set(\gate, 0);
//~node.set(\sus, 2);
//~node.set(\dec, 2);
s.sendBundle(s.latency, ["n_set", ~node, 'gate', 1]);
//~node.set(\gate, 1);
)
/*
s.sendMsg("s_new", "sc303", 3000, 0, 0);
s.sendMsg("n_set", 3000, 'gate', 1, 'freq', 220);
s.sendMsg("n_set", 3000, 'gate', 0);
s.sendMsg("n_set", 3000, 'dec', 1);

s.sendMsg("n_free", ~node)
*/


// make GUI!
(
~bpm = 140;
~step = 0;
	// set default note ons
~defaultNoteOns = Array.fill(16,1);
	// this is the note on array that will be used in the sequencer routine
~noteOns = ~defaultNoteOns;

	// set root
~root = 36;

	// set default pitches
~defaultPitches = [ 0, 12, 0, 0, -12, 0, 0, 0, -12, 0, 12, 0, 3, -5, 2, 0 ];
	// this is the pitch array that will be used in the sequencer routine
~pitches = ~defaultPitches;

~tStepLED = Array.new; 	// the step indicator LEDs as an array
~sNoteOn = Array.new;  	// the note-on sliders as an array
~tNoteOn = Array.new;	// the note-on value boxes as an array
~sPitch = Array.new;	// the pitch sliders as an array
~tPitch = Array.new; 	// the pitch value boxes as an array

w = Window("SC-303", Rect(128, 64, 510, 320));
w.view.background = Color.white;
w.front;

~layoutLeft = CompositeView(w, Rect(0, 0, 400, 320));	// make the left pane of the window
~layoutLeft.decorator = FlowLayout(~layoutLeft.bounds); // auto-arrange the elements
~layoutRight = CompositeView(w, Rect(400, 0, 400, 120));
~layoutRight.decorator = FlowLayout(~layoutRight.bounds);

// make transport controls

	// Tempo
StaticText(~layoutRight, Rect(0,0, 45, 14)).string_("BPM: ");
~tBpm = NumberBox(~layoutRight, Rect(0,0, 45, 14))
	.string_(~bpm)
	.action_({ arg item;
		~bpm = item.value;
		~sBpm.value_((~bpm-40)/200);
	});

~layoutRight.decorator.nextLine;
~sBpm = Slider(~layoutRight, Rect(0,0, 100, 20))
	.background_(Color.new(0.8,0.8,0.8))
	.value_((~bpm-40)/200)
	.action_({ arg sldr;
		~bpm = ((sldr.value)*200) + 40;
		~tBpm.string_(~bpm);
	});

~layoutRight.decorator.nextLine;

	// Reset
Button(~layoutRight, Rect(0,0,45,20))
	.states_([["|<"]])
	.action_({ arg item;
		~tStepLED[~step].background_(Color.new(0.6,0,0));
		~step = 0;
		~tStepLED[0].background_(Color.new(1,0,0));
	});

	// Play/Pause
Button(~layoutRight, Rect(0,0,45,20))
	.states_([[">"],["||"]])
	.action_({ arg item;
		if( item.value == 0, {
			~seqr.stop;
		},{
			~seqr.reset;
			~seqr.play;
		});
	});

~layoutRight.decorator.nextLine;

	// Root note
StaticText(~layoutRight, Rect(0,0, 45, 14)).string_("Root: ");
~tRoot = StaticText(~layoutRight, Rect(0,0, 45, 14)).string_(~root);
~layoutRight.decorator.nextLine;
Slider(~layoutRight, Rect(0,0, 100, 20))
	.background_(Color.new(0.8,0.8,0.8))
	.value_((~root-24)/36)
	.step_(1/48)
	.action_({ arg sldr;
		~root = ((sldr.value)*36) + 24;
		~tRoot.string_(~root);
	});


// make synth control labels
StaticText(~layoutLeft, Rect(0,0, 45, 14)).string_("Wave");
StaticText(~layoutLeft, Rect(0,0, 80, 14)).string_("Cutoff");
StaticText(~layoutLeft, Rect(0,0, 80, 14)).string_("Resonance");
StaticText(~layoutLeft, Rect(0,0, 80, 14)).string_("Decay");
StaticText(~layoutLeft, Rect(0,0, 80, 14)).string_("Envelope");
~layoutLeft.decorator.nextLine;

// make the synth controls
	// wave-type
Button(~layoutLeft, Rect(0,0,45,20))
	.states_([["Saw"],["Square"]])
	.action_({ arg item;
		s.sendMsg("n_set", ~node, 'wave', item.value);
	});
	// cut-off frequency
Slider(~layoutLeft, Rect(0,0, 80, 20))
	.background_(Color.new(0.8,0.8,0.8))
	.action_({ arg sldr;
		s.sendMsg("n_set", ~node, 'ctf', ((sldr.value)*(10000-100))+100); // modify synth param
	});

	// resonance amt
Slider(~layoutLeft, Rect(0,0, 80, 20))
	.background_(Color.new(0.8,0.8,0.8))
	.action_({ arg sldr;
		s.sendMsg("n_set", ~node, 'res', (1-sldr.value)*(0.97)+0.03); // modify synth param
	});

	// decay amt
Slider(~layoutLeft, Rect(0,0, 80, 20))
	.background_(Color.new(0.8,0.8,0.8))
	.action_({ arg sldr;
		s.sendMsg("n_set", ~node, 'dec', (sldr.value)*2); // modify synth param
	});

	// envelope amt
Slider(~layoutLeft, Rect(0,0, 80, 20))
	.background_(Color.new(0.8,0.8,0.8))
	.action_({ arg sldr;
		s.sendMsg("n_set", ~node, 'env', (sldr.value)*10000); // modify synth param
	});

~layoutLeft.decorator.nextLine;

	// make step LEDs
16.do({ arg i;
	~tStepLED = ~tStepLED.add(
		StaticText(~layoutLeft, Rect(0,0,20,12))
			.background_(Color.new(0.6,0,0))
			.stringColor_(Color.white)
			.string_(i+1)
	);
});
~tStepLED[~step].background_(Color.new(1,0,0));
~layoutLeft.decorator.nextLine;

	// make the note-on sliders
16.do({ arg i;
	~sNoteOn = ~sNoteOn.add(
		Slider(~layoutLeft, Rect(0,0,20,60))			// position doesn't matter; width, height do
			.background_(Color.new(0,0,0.7))		// color the slider
			.step_(0.1)						// values are stepped by 0.1
			.value_(~noteOns[i])				// assign it its value from the note on array
			.action_({ arg sldr;				// when the slider is moved...
				~noteOns[i] = sldr.value;		// ... update note-on array
				~tNoteOn[i].string = sldr.value; // ... update its value box
					// ...change color of value box
				~tNoteOn[i].background = Color.new((1-~noteOns[i]),1,(1-~noteOns[i]));
			})
	);
});
~layoutLeft.decorator.nextLine;

	// make the note-on value boxes
16.do({ arg i;
	~tNoteOn = ~tNoteOn.add(
		StaticText(~layoutLeft, Rect(0,0,20,20))
			.background_(Color.new((1-~noteOns[i]),1,(1-~noteOns[i])))
			.string_(~sNoteOn[i].value)
			.align_(0)
			.action_({ arg text;
				text.string.postln;
			})
	);
});
~layoutLeft.decorator.nextLine;

	// make the pitch sliders
16.do({ arg i;
	~sPitch = ~sPitch.add(
		Slider(~layoutLeft, Rect(0,0,20,120))
			.value_((~pitches[i]+12)/24)
			.step_(1/24)
			.background_(Color.new(0.8,0.8,0.8))
			.action_({ arg item;
				~pitches[i] = ((item.value * 24)-12);
				~tPitch[i].string = ((item.value * 24)-12);
			})
	);
});
~layoutLeft.decorator.nextLine;

	// make the pitch value boxes
16.do({ arg i;
	~tPitch = ~tPitch.add(
		StaticText(~layoutLeft, Rect(0,0,20,20))
			.background_(Color.white)
			.string_(~pitches[i].value)
			.align_(0)
	);
});
/*~tempButt = Button(w, Rect(0,0,60,60))
			.states_([
				["0", Color.red, Color.black]
			]);*/


)

// do other stuff
~node = 938
~node = 1003
(

~node = s.nextNodeID;

// start the synth
s.sendMsg("s_new", "sc303", ~node, 1, 1);

// make the step sequencer
~seqr = Routine.new({
	loop({

			// turn on LED
		{~tStepLED[~step].background_(Color.new(1,0,0)); /*(~step.asString + "on").postln;*/}.defer;

		if(~noteOns[~step].coin, {
			// play note!
				// this is out of sync
//			s.sendMsg("n_set", ~node, 'gate', 1, 'freq', (~pitches[~step]+~root).midicps);
//			(7.5/~bpm).wait;
//			s.sendMsg("n_set", ~node, 'gate', 0);
//			(7.5/~bpm).wait;

				// buffer playback on server (adds start/stop delay, but better inter-note timing)
			(~pitches[~step]+~root).midicps.postln;
			s.sendBundle(s.latency, ["n_set", ~node, 'gate', 1, 'freq', (~pitches[~step]+~root).midicps]);
			(7.5/~bpm).wait;
			s.sendBundle(s.latency, ["n_set", ~node, 'gate', 0]);
			(7.5/~bpm).wait;
		},{
				// it's a rest
			(15/~bpm).wait;
		});

			// turn off LED
		{~tStepLED[(~step-1)%16].background_(Color.new(0.6,0,0)); /*(((~step-1)%16).asString + "off").postln;*/}.defer;

		~step = (~step + 1)%16;

	});
});
)


~seqr.play;
~seqr.stop;
~seqr.reset;
~pitches