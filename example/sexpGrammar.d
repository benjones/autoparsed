module sexpGrammar;

import autoparsed.autoparsed;
import autoparsed.lexer;
import autoparsed.syntax;

import sumtype;


pragma(msg, "parsing sexpgrammar module");

@Token
enum lparen = '(';

@Token
enum rparen = ')';

@Token
@Lex!(RegexPlus!(OneOf!(' ', '\t', '\r', '\n')))
struct Whitespace {
  const(dchar)[] val;
}

@Token
@Lex!(RegexPlus!(Not!(OneOf!(lparen, rparen)), Token))
struct Atom {
  const(dchar)[] val;
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

pragma(msg, "end parsing sexpgrammar module");
