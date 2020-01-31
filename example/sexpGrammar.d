
import autoparsed.autoparsed;
import std.variant;

@Token
enum lparen = "(";

@Token
enum rparen = ")";

@Token
struct Atom {
  string val;
}

class Sexp {
public:
  @Syntax!(lparen, RegexPlus!(OneOf!(Atom, Sexp)), rparen)
  this(Algebraic!(Atom, Sexp)[] members_){
	members = members_;
  }

private:
  Algebraic!(Atom, Sexp)[] members;
}
