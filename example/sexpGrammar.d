/**
   Example of how to use autoparsed to define a grammar for S-Expressions
   It also shows how to annotate tokens so they work with [autoparsed.Lexer].

   The annotations are based on PEG grammars.
**/
module sexpGrammar;

import autoparsed.autoparsed;
import autoparsed.lexer;
import autoparsed.syntax;

import sumtype;



@Token{
  enum lparen = '(';
  enum rparen = ')';

  @Lex!(RegexPlus!(OneOf!(' ', '\t', '\r', '\n')))
    struct Whitespace {
      const(dchar)[] val;
    }

  @Lex!(RegexPlus!(Not!(OneOf!(lparen, rparen)), Token))
    struct Atom {
      const(dchar)[] val;
    }
}

class Sexp {
public:
  @Syntax!(lparen, RegexPlus!(OneOf!(Atom, Sexp)), rparen)
  this(SumType!(Atom, Sexp)[] members_){
    members = members_;
  }

  override string toString() {
    import std.array : join;
    import std.algorithm : map;
    return "Sexp( " ~ join(map!(a => a.toString)(members), ", ") ~ ")";
  }
private:
  SumType!(Atom, Sexp)[] members;
}

