import autoparsed.autoparsed;
import autoparsed.recursivedescent;

pragma(msg, "start main module");

void main(string[] args){
  import std.stdio;
  import std.traits;
  import std.array;
  import std.algorithm;
  import sumtype;
  pragma(msg, "start of main");
  pragma(msg, "symbols from sexpGrammar\n");
  import sexpGrammar;
  static foreach(sym; symbolsFromModule!sexpGrammar){
	
	pragma(msg, " a symbol " ~ fullyQualifiedName!sym);
	pragma(msg, "is type? " ~  (isType!sym ? "yes" : "no") );
  }

  pragma(msg, "tokens from module sexpGrammar");
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
  pragma(msg, "payload type");

  auto x = (tokenTypes!sexpGrammar)[0]();
  PayloadType pt;
  pragma(msg, typeof(pt));
  pt= x;
  
  /*
  pragma(msg, "\ngrammar symbols\n");
  static foreach(sr; symbolsFromModule!sexpGrammar){
	pragma(msg, fullyQualifiedName!sr);
  }
  */

  pragma(msg, "\nsyntax rules\n");
  static foreach(sr; SyntaxRulesFromModule!sexpGrammar){
	pragma(msg, "\n A SYNTAX RULE: \n" ~ fullyQualifiedName!sr ~ "\n");
	static foreach(uda; getUDAs!(sr, Syntax)){
	  pragma(msg, "annotation: " ~ fullyQualifiedName!uda);
	}

	pragma(msg, "PEG string: " ~ RuleToPegString!sr);
  }

  import autoparsed.lexer;
  writeln("starting lexer");
  auto lexer = Lexer!sexpGrammar("(hello(goodbye))");
  PayloadType[] tokens = lexer.array;
  writeln("lexed into tokens");
  writeln(tokens);
  /*  tokens ~= PayloadType(TokenType!lparen());
  tokens ~= PayloadType(TokenType!Atom(Atom("hello")));

  tokens ~= PayloadType(TokenType!lparen());
  tokens ~= PayloadType(TokenType!Atom(Atom("goodbye")));
  tokens ~= PayloadType(TokenType!rparen());

  tokens ~= PayloadType(TokenType!rparen());
  */

  //auto parsed = parse!Sexp(tokens);
  //writeln(parsed);
}

pragma(msg, "end main module");
