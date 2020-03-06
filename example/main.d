import autoparsed.autoparsed;
import autoparsed.recursivedescent;


void main(string[] args){
  import std.stdio;
  import std.traits;
  import std.array;
  import std.algorithm;
  import sumtype;

  import sexpGrammar;
  
  pragma(msg, "\nsyntax rules for sepGrammar\n");
  static foreach(sr; SyntaxRulesFromModule!sexpGrammar){
	pragma(msg, "PEG string: " ~ RuleToPegString!sr);
  }
  
  import autoparsed.lexer;
  writeln("starting lexer");
  auto lexer = Lexer!sexpGrammar("( \t hello (  goodbye)\n)");
  auto tokens = lexer.filter!(
	x => x.match!( (TokenType!Whitespace w){
		writefln("got whitespace: `%s`", w.value.val);
		return false;
	  },
	  _ => true)
							  ).array;
  writeln("lexed into tokens");
  writeln(tokens);
  
  auto parsed = parse!Sexp(tokens);
  writeln(parsed);
}

