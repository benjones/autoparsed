import autoparsed.autoparsed;
import exampleGrammar;


void main(string[] args){
  import std.stdio;
  import std.traits;
  static foreach(sym; symbolsFromModule!tokens){
	
	pragma(msg, " a symbol " ~ fullyQualifiedName!sym);
	pragma(msg, "is type? " ~ isType!sym);
  }

  
  static foreach(tok; tokensFromModule!tokens){
	pragma(msg, tok);
	pragma(msg, "making");
	pragma(msg, TokenType!(tok));
	pragma(msg, "done");
  }

  pragma(msg, "token types");
  static foreach(tt; tokenTypes!tokens){
	pragma(msg, tt);
  }


  alias PayloadType = TokenPayload!tokens;

  pragma(msg, __traits(allMembers, PayloadType));
  
  parse!exampleGrammar;
}
