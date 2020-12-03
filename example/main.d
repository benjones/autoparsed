import autoparsed.autoparsed;
import autoparsed.recursivedescent;


void main(string[] args){
  import std.stdio;
  import std.traits;
  import std.array;
  import std.algorithm;
  import sumtype;

  import sexpGrammar;

  pragma(msg, "\nsyntax rules for sepGrammar\n\n");
  static foreach(i, sr; SyntaxRulesFromModule!sexpGrammar){
    pragma(msg, "PEG string ", i, ": " ~ RuleToPegString!sr);
  }

  import autoparsed.lexer;
  writeln("starting lexer");
  auto lexer = Lexer!sexpGrammar("( \t hello (  goodbye)\n)");
  auto tokens = lexer.filter!(
    x => x.match!( (Whitespace w){
        writefln("got whitespace: `%s`", w.val);
        return false;
      },
      _ => true)
                              ).array;
  writeln("lexed into tokens");
  writeln(tokens);
  pragma(msg, "lexer setup fine, about to start parser\n\n\n");
  auto parsed = parse!Sexp(tokens);
  writeln(parsed.getPayload.contents);
}
