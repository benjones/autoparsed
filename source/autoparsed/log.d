module autoparsed.log;

public:

///Compile time logging, enable with debug version AutoparsedCTLog
mixin template CTLog(Args...){

  debug(AutoparsedCTLog){

    pragma(msg, Args);
    //enum so(X) = X.stringof;
    //import std.meta;
    //import std.array;
    //pragma(msg, [StaticMap!(so,Args)].join(", "));
  }
}

///run time logging, enable with debug version AutoparsedRTLog
void RTLog(Args...)(Args args){
  debug(AutoparsedRTLog){
    import std.stdio : writeln;
    writeln(args);
  }
}
