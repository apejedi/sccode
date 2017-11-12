s.options.maxLogins = 5

s.options.memSize_(65536 * 4)

(
f = { |msg, time, replyAddr, recvPort|
	b = NetAddr.new("127.0.0.1", 4420);
    if(msg[0] == '/evalCode') {
		b.sendMsg("/response", msg[1].asString.interpretPrint);
    }
};
)

thisProcess.addOSCRecvFunc(f);
//thisProcess.removeOSCRecvFunc(f)

s.boot

s.addOSCRecvFunc(f)