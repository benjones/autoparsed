module autoparsed.log;

public:

mixin template CTLog(Args...){
  
  debug(AutoparsedCTLog){
    pragma(msg, Args);
  }
}
