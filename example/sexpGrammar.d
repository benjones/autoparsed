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

  struct Whitespace {
    const(dchar)[] val;
    @Syntax!(RegexPlus!(OneOf!(' ', '\t', '\r', '\n')))
    this(const(dchar)[] val_){
      val = val_;
    }
  }

  struct Atom {
    const(dchar)[] val;
    @Syntax!(RegexPlus!(Not!(OneOf!(lparen, rparen)), Token))
    this(const(dchar)[] val_){
      val = val_;
    }
  }
}

class Sexp {
public:
  @Syntax!(lparen, RegexPlus!(OneOf!(Atom, Sexp)), rparen)
  this(OneOf!(Atom, Sexp).NodeType[] members_){
    members = members_;
  }

  override string toString() {
    import std.array : join;
    import std.algorithm : map;
    return "Sexp( " ~ join(map!(a => a.toString)(members), ", ") ~ ")";
  }
private:
  OneOf!(Atom, Sexp).NodeType[] members;
}

