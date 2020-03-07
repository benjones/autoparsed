module autoparsed.log;

public:

mixin template CTLog(Args...){
  
  debug(AutoparsedCTLog){
    pragma(msg, Args);
  }
}

void RTLog(Args...)(Args args){
  debug(AutoparsedRTLog){
    import std.stdio : writeln;
    writeln(args);
  }
}
