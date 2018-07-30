(
s.options.maxLogins = 2;

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

s.boot;
)

s.shutdown
s.plotTree