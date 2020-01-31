import autoparsed.autoparsed;
import autoparsed.recursivedescent;
import exampleGrammar;
import sexpGrammar;

void main(string[] args){
  import std.stdio;
  import std.traits;
  pragma(msg, "symbols from tokens\n");
  static foreach(sym; symbolsFromModule!sexpGrammar){
	
	pragma(msg, " a symbol " ~ fullyQualifiedName!sym);
	pragma(msg, "is type? " ~  (isType!sym ? "yes" : "no") );
  }

  
  static foreach(tok; tokensFromModule!sexpGrammar){
	pragma(msg, tok);
	pragma(msg, "making");
	pragma(msg, TokenType!(tok));
	pragma(msg, "done");
  }

  pragma(msg, "token types");
  static foreach(tt; tokenTypes!sexpGrammar){
	pragma(msg, tt);
  }


  alias PayloadType = TokenPayload!sexpGrammar;
  pragma(msg, "payload members");
  pragma(msg, __traits(allMembers, PayloadType));

  auto x = (tokenTypes!sexpGrammar)[0]();
  PayloadType pt;
  pragma(msg, PayloadType);
  pt= x;
  
  
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

  PayloadType[] tokens;
  tokens ~= PayloadType(TokenType!lparen());
  tokens ~= PayloadType(TokenType!Atom(Atom("hello")));
  tokens ~= PayloadType(TokenType!rparen());
  
  auto parsed = parse!Sexp(tokens);
  writeln(parsed);
}
