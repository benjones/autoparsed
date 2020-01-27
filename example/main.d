import autoparsed.autoparsed;
import exampleGrammar;
import sexpGrammar;

void main(string[] args){
  import std.stdio;
  import std.traits;
  pragma(msg, "symbols from tokens\n");
  static foreach(sym; symbolsFromModule!tokens){
	
	pragma(msg, " a symbol " ~ fullyQualifiedName!sym);
	pragma(msg, "is type? " ~  (isType!sym ? "yes" : "no") );
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
  pragma(msg, "payload members");
  pragma(msg, __traits(allMembers, PayloadType));

  auto x = (tokenTypes!tokens)[0]();
  PayloadType pt;

  pt= x;
  
  parse!exampleGrammar;


  
  pragma(msg, "\ngrammar symbols\n");
  static foreach(sr; symbolsFromModule!sexpGrammar){
	pragma(msg, fullyQualifiedName!sr);
  }


  pragma(msg, "\nsyntax rules\n");
  static foreach(sr; SyntaxRulesFromModule!sexpGrammar){
	pragma(msg, "\n A SYNTAX RULE: \n" ~ fullyQualifiedName!sr ~ "\n");
	static foreach(uda; getUDAs!(sr, Syntax)){
	  pragma(msg, "annotation: " ~ fullyQualifiedName!uda);
	}

	pragma(msg, "PEG string: " ~ RuleToPegString!sr);
  }
  
}
