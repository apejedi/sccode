(
 DebugNetAddr : NetAddr {
	sendRaw {
		|rawArray|
  if(active) { this.dump(nil, rawArray) };
  super.sendRaw(rawArray);
 };

};
 sendMsg { arg ... args;
  if(active) { this.dump(nil, [args]) };
  super.sendMsg(*args);
 };
 sendBundle { arg time ... args;
  if(active) { this.dump(time, args) };
  super.sendBundle(time, *args);
 };
 dump { arg time, args;
  var str, docStr;
  if(args[0][0].asSymbol === '/status') { ^this };
  if(doc.isNil) { this.makeDocument };
  args.postln;

  defer {
   str = "latency" + time ++ Char.nl;
   args.do {arg msg;
    str = str ++ Char.tab;
    msg = msg.collect { arg el;
     if(el.isKindOf(RawArray) and: { el.size > 15 })
      { "data[" + el.size + "]" } { el };
    };
    str = str ++ msg.asCompileString ++ Char.nl;
   };
   ("string:" + str).postln;
   str = str ++ Char.nl;

   doc.selectedString_(str)
  };


 };
 makeDocument {
  doc = Document(this.asCompileString)
  .onClose_({ doc = nil; active = false })
  .background_(Color.rand);

  // UI.registerForShutdown({ doc.close }); // doesn't work properly.
 };
}
)


