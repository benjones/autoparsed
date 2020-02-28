module autoparsed.syntax;

pragma(msg, "starting syntax module");
///UDA type for annotating constructors as grammar rules
struct Syntax(T...){
  alias Elements = T;
}

///UDA type for annotating the syntax for Tokens
///Similar to synatx, but "tokens" will be chars here
struct Lex(T...){
  alias Elements = T;
}

///UDA type for annotating that a type or enum is a token
enum Token;

struct TokenType(alias T){
  import std.traits : isType, fullyQualifiedName;
  pragma(msg, "token type instantiation for " ~ fullyQualifiedName!T);

  static if(isType!T){
	pragma(msg, "type");
	alias type = T;
	T value;
  } else {
	pragma(msg, "not a type");
	enum value = T;
  }
}


///PEG rule for picking between options
struct OneOf(Ts...){
  import std.traits: fullyQualifiedName, TemplateOf, isInstanceOf, isType;
  import std.meta : staticMap;
  
  static if(Ts.length < 2){
	pragma(msg, "Warning: using OneOf to pick between "
		   ~ Ts.length~ " options.  You should probably provide at least 2 options.");

	static if(Ts.length ==1){
	  pragma(msg, "type provided: " ~ fullyQualifiedName!Ts[0]);
	}
  }
  import sumtype;
  template Wrap(alias T){
	static if(!isType!T) {
	  alias Wrap = TokenType!T;
	} else {
	  alias Wrap = T;
	}
  }

  alias NodeType = SumType!(staticMap!(Wrap, Ts));
}

///PEG rule for "at least one"
struct RegexPlus(T...){

}

///PEG rule for "0 or more"
struct RegexStar(T...){

}

///PEG rule for 0 or 1 of this
struct Optional(T...){
  
}

struct Not(T...){

}
pragma(msg, "end syntax module");
