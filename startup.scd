(
s.options.maxLogins = 12;

s.options.memSize_(65536 * 4);


~client = NetAddr.new("127.0.0.1", 4420);

f = { |msg, time, replyAddr, recvPort|
	if(msg[0] == '/evalCode') {
		var resp = msg[1].asString.interpretPrint;
		var key = "default";
		if (msg.size > 2) {
			key = msg[2];
		};
		~client.sendMsg("/response", resp, key);
		// replyAddr.sendMsg("/response", resp);
	}
};

thisProcess.addOSCRecvFunc(f);
// thisProcess.removeOSCRecvFunc(f)
//Server.default.options.outDevice_("ASIO : ReaRoute ASIO (x64)")
//Server.default.options.outDevice_("Realtek HD Audio 2nd output (Re   (device #5 with 0 ins 2 outs)")
//Server.default.options.numOutputBusChannels = 16
s.boot;
StageLimiter.activate;
)

//Gated synths
(
OSCdef.freeAll;
~test = {
	|msg, time, addr|
	msg.postln;
	addr.sendRaw(['/test', 1, 2, 'sds']);
};
OSCdef.new(\test, ~test, '/test');

~getSynthControls = {
	|msg, time, addr|
	var synth, controls;
	controls = ['/controls'];
	/*if (msg.size > 1) {
		synth = msg[1];
		SynthDescLib.global.at(synth).controlNames.do({|s|  controls = controls ++ '' ++ s ++ ' ' });
		addr.sendMsg('/controls', synth, controls);
	} else {*/
	SynthDescLib.global.synthDescs.do{|s|
		controls = controls.add(s.name);
		controls = controls.add(s.controlNames.size);
		controls = controls ++ s.controlNames;
	};

	 addr.sendRaw(controls.asRawOSC);
	//}
};
OSCdef.new(\controls, ~getSynthControls, '/get_controls');

~gated = ();
~nodes = ();
~handleGated = {
	|msg|
	var node, synth, key, args;
	msg.removeAt(0);
	synth = msg[0];
	msg.removeAt(0);
	//key = msg[msg.atIdentityHash(\freq) + 1].round(0.01);
	key = msg[msg.atIdentityHash(\freq) + 1].cpsmidi.round;
	3.do{msg.removeAt(0)};
	args = msg.clump(2);

	if (~gated.includesKey(synth).not, {
		~gated[synth] = ();
	});

	if (~gated[synth].includesKey(key), {
		args.do({|ctrl|
			~gated[synth][key].set(ctrl[0], ctrl[1]);
		});
	}, {
		node = Synth(synth, msg);
		~gated[synth][key] = node;
		~nodes[node.asNodeID] = [synth, key];
	});
};

~deleteNode = {
	|msg|
	var id;
	id = msg[1];
	if (~nodes.includesKey(id), {
		~gated[~nodes[id][0]].removeAt(~nodes[id][1]);
		~nodes.removeAt(id);
	});
};


OSCdef.new(\handleGated, ~handleGated, '/s_new');

OSCdef.new(\node_off, ~deleteNode ,'/n_end');

)