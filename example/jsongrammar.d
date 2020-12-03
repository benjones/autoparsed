/**
  Example of a grammar for JSON

**/

module jsongrammar;

import autoparsed.autoparsed;
import autoparsed.lexer;
import autoparsed.syntax;
import autoparsed.log;

import std.typecons : Tuple;

import sumtype;

@Token {
  enum lcurly = '{';
  enum rcurly = '}';
  enum lbracket = '[';
  enum rbracket = ']';
  enum colon = ':';
  enum comma = ',';
  enum dot = '.';
  
  @Syntax!(RegexPlus!(OneOf!(' ', '\t')))//, '\r', '\n')))
  struct Whitespace {
    const(dchar)[] val;
    this(const(dchar)[] val_){
      val = val_;
    }
  }
  
  @Syntax!('"', RegexStar!(Not!('"'), Token), '"')
  struct QuotedString {
    const(dchar)[] val;
    this(const(dchar)[] val_){
      val = val_;
    }
  }

  private alias Digit = InRange!('0', '9');//OneOf!('0', '1, '2', '3', '4', '5', '6', '7', '8', '9');

  //regex for a number -?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?
  //I'm allowing leading 0s because who cares, todo: scientific notation

  @Syntax!(Optional!('-'), RegexStar!Digit, Optional!dot, RegexStar!Digit)
  struct Number{
    import std.typecons : Nullable;

    this(string rep){
      import std.conv : to;
      RTLog("Making a number from: ", rep);
      val = to!double(rep);
    }
    double val;
    alias val this;
  }

}


alias JSONValueSyntax = OneOf!(Number, JSONObject, JSONArray, QuotedString);
alias JSONValue = JSONValueSyntax.NodeType;

import std.meta : AliasSeq;
alias FieldSyntax = AliasSeq!(QuotedString, colon, JSONValueSyntax);

@Syntax!(lcurly, RegexStar!(FieldSyntax, comma), Optional!(FieldSyntax), rcurly)
struct JSONObject{
  private JSONValue[QuotedString] vals;
  alias vals this;
  this(Tuple!(QuotedString, JSONValue)[] data){
    foreach(tup; data){
      vals[tup[0]]= tup[1];
    }
  }
  
  string toString(){
    import std.conv: to;
    return to!string(vals);
  }
}

@Syntax!(lbracket, RegexStar!(JSONValueSyntax, comma), Optional!(JSONValueSyntax), rbracket)
struct JSONArray{
  private JSONValue[] data;
  alias data this;

  this(JSONValue[] data_){
    data = data_;
  }
  
}


unittest{
  import std.stdio;
  import std.array;
  import std.algorithm;
  import std.conv : to;
  import autoparsed.recursivedescent;

  writeln("\n\nRUNNING\n\n");
  auto testString = `{"hello" : "world", "key" : 45, "anArray" :[1, "a string", {}]}`;
  writeln("lexing: ", testString);
  auto lexer = Lexer!jsongrammar(testString);

  auto tokens = lexer.filter!( x => x.match!(
                                 (Whitespace w) => false,
                                 _ => true)
                               ).array;
  writeln("\n\nTOKENS:\n", tokens, "\n\n");

  auto jo = parse!JSONObject(tokens).getPayload.contents;
  writeln("\n\n\n", jo, "\n\n\n");
  jo[QuotedString("key")].data.match!(
    (Number n){assert(n.val == 45);},
    (_){assert(false);});

  jo[QuotedString("anArray")].data.match!(
    (JSONArray arr){assert(arr.length == 3);},
    (_){assert(false);});

  double[] numbers = [1, -10, 37.5, 36., -.98];
  foreach(number; numbers){
    auto str = to!string(number);
    writeln("checking ", str);
    auto lex = Lexer!jsongrammar(str);
    Number parsed = parse!Number(lex).getPayload.contents;
    writeln("parsed is: ", parsed);
    assert(parsed.val == number);

  }
  
}
