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
  

  struct Whitespace {
    const(dchar)[] val;
    @Syntax!(RegexPlus!(OneOf!(' ', '\t')))//, '\r', '\n')))
    this(const(dchar)[] val_){
      val = val_;
    }
  }
  
  
  struct QuotedString {
    const(dchar)[] val;
    @Syntax!('"', RegexStar!(Not!('"'), Token), '"')
    this(const(dchar)[] val_){
      val = val_;
    }
  }

  private alias Digit = OneOf!('0', '1');//, '2', '3', '4', '5', '6', '7', '8', '9');

  //regex for a number -?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?
  //I'm allowing leading 0s because who cares, todo: scientific notation

  struct Number{
    import std.typecons : Nullable;
    @Syntax!(Optional!('-'), RegexPlus!Digit) //, Optional!(Keep!('.'), RegexPlus!Digit))
    this(Nullable!(TokenType!('-')) minusSign, string rep){
      RTLog("making Number from `", minusSign, " and ", rep, "`");
      import std.conv : to;
      val = to!double(rep);
      if(!minusSign.isNull){
        val *= -1;
      }
    }
    double val;
  }

}




unittest{
  import std.stdio;
  import std.array;
  
  auto testString = `{} "as df" -10   101 -011    [{,]}`;

  auto lexer = Lexer!jsongrammar(testString);

  auto tokens = lexer.array;
  writeln(tokens);

}
