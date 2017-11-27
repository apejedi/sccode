(
s.options.maxLogins = 5;

s.options.memSize_(65536 * 4);


~client = NetAddr.new("127.0.0.1", 4420);

f = { |msg, time, replyAddr, recvPort|
	// msg.asString.postln;
    if(msg[0] == '/evalCode') {
		~client.sendMsg("/response", msg[1].asString.interpretPrint);
    }
};

thisProcess.addOSCRecvFunc(f);
// thisProcess.removeOSCRecvFunc(f)

s.boot;
)

s.shutdown