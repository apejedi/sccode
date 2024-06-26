(
Server.default.options.outDevice_("ASIO : ReaRoute ASIO (x64)");
Server.default.options.numOutputBusChannels = 16;
//SimpleMix(10).connect.start.gui;
)
(
s.options.maxLogins = 4;

s.options.memSize_(65536 * 4);

// NUMBER OF BUFFERS GETS DIVIDED BY NUMBER OF LOGINS!
s.options.numBuffers = 2046;

s.options.numAudioBusChannels = 1024;


//Server.default.options.outDevice_("Realtek HD Audio 2nd output (Re   (device #5 with 0 ins 2 outs)")

OSCdef.freeAll;

~getSynthControls = {
	|msg, time, addr|
	var synth, controls;
	synth = msg[1].asString;
	controls = [synth];
	SynthDescLib.global.synthDescs.do{|s|
		if (s.name == synth, {
			s.controls.do{|c| controls = controls ++ c.name ++ c.defaultValue};
		}, {});
	};
	addr.sendMsg("/synth_controls", *controls);
};
OSCdef.new(\controls, ~getSynthControls, '/get_controls');

~gated = ();
~nodes = ();
~handleGated = {
	|msg|
	var node, synth, key, target, args;
	if (msg[4] == nil, {
		target = 0;
	}, {
		target = msg[4];
	});
	msg.removeAt(0);
	synth = msg[0];
	msg.removeAt(0);
	key = msg[msg.atIdentityHash(\freq) + 1].cpsmidi.round;
	3.do{msg.removeAt(0)};
	args = msg.clump(2);
	if (~gated.includesKey(target).not, {
		~gated[target] = ();
	});

	if (~gated[target].includesKey(key), {
		args.do({|ctrl|
			~gated[target][key].set(ctrl[0], ctrl[1]);
		});
	}, {
		node = Synth(synth, msg, target);
		~gated[target][key] = node;
		~nodes[node.asNodeID] = [target, key];
	});
};

~stopGated = {
	|msg|
	if (msg[1] == nil, {
		~gated.do({|nodes, synth| nodes.do({|node, key| node.set('gate', 0)}); });
		~gated = ();
		~nodes = ();
	}, {
		~gated[msg[1]].pairsDo({|key, node|
			node.set('gate', 0);
		});
		//~gated.removeAt(msg[1]);
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

~client = NetAddr.new("127.0.0.1", 6814);

~evalCode = { |msg, time, replyAddr, recvPort|
		var resp = msg[1].asString.interpretPrint;
		var key = "default";
		if (msg.size > 2) {
			key = msg[2];
		};
		//~client.sendMsg("/response", resp, key);
		replyAddr.sendMsg("/response", resp);
};

OSCdef.new(\evalCode, ~evalCode, '/eval_code');

OSCdef.new(\handleGated, ~handleGated, '/s_new');

OSCdef.new(\node_off, ~deleteNode ,'/n_end');

OSCdef.new(\stopGated, ~stopGated ,'/stop_gated');

)
(
s.boot;
//s.waitForBoot({
	//20.collect{Bus.audio(s, 2)};
//});
StageLimiter.activate;
)