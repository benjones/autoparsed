/**
  Example of a grammar for JSON

**/

module jsongrammar;

import autoparsed.autoparsed;
import autoparsed.lexer;
import autoparsed.syntax;
import autoparsed.log;

import sumtype;

@Token {
  enum lcurly = '{';
  enum rcurly = '}';
  enum lbracket = '[';
  enum rbracket = ']';
  enum comma = ',';
  
  @Lex!(RegexPlus!(OneOf!(' ', '\t', '\r', '\n')))
    struct Whitespace {
      const(dchar)[] val;
    }

  @Lex!('"', RegexStar!(Not!('"'), Token), '"')
    struct QuotedString {
      const(dchar)[] val;
    }

  private alias Digit = OneOf!('0', '1');//, '2', '3', '4', '5', '6', '7', '8', '9');

  //regex for a number -?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?
  //I'm allowing leading 0s because who cares, todo: scientific notation
  @Lex!(Optional!(Keep!('-')), RegexPlus!Digit) //, Optional!(Keep!('.'), RegexPlus!Digit))
    struct Number{
      this(string rep){
        RTLog("making Number from `", rep, "`");
        import std.conv : to;
        val = to!double(rep);
      }
      double val;
    }

}




unittest{
  import std.stdio;
  
  auto testString = `{} "as df" -10   101 -011    [{,]}`;

  auto lexer = Lexer!jsongrammar(testString);

  foreach(token;  lexer){
    writeln(token);
  }

}
