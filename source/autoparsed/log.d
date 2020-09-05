module autoparsed.log;

public:

mixin template CTLog(Args...){
  
  debug(AutoparsedCTLog){
    
    pragma(msg, Args);
    //    import std.array;
    //    pragma(msg, [Args].join(", "));
  }
}

void RTLog(Args...)(Args args){
  debug(AutoparsedRTLog){
    import std.stdio : writeln;
    writeln(args);
  }
}
