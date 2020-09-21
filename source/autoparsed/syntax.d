module autoparsed.syntax;

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

///Wrapper type necessary for value tokens '(', ')', whitespace, etc
struct TokenType(alias T){
  import std.traits : isType;

  static if(isType!T){
    alias type = T;
    T value;
  } else {
    enum value = T;
  }
}


///Rule to literal that we want to keep.  Useful in conjunction with Optional.
///By default, literals are checked, but don't don't return anything
struct Keep(alias V){
}

///PEG rule for picking between options
struct OneOf(Ts...){
  import std.traits: fullyQualifiedName, TemplateOf, TemplateArgsOf, isInstanceOf, isType, CommonType;
  import std.meta : staticMap, allSatisfy;
  
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

  alias WrappedTypes = staticMap!(Wrap, Ts);
  
  struct NodeType{
    alias ST = SumType!WrappedTypes;
    ST data;
    alias data this;
    static foreach(WT; WrappedTypes){
      this(WT wt){
        data = wt;
      }
    }
  }
  
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

///PEG rule that is satisfied if the next token is Not any of the Ts
struct Not(T...){

}

// for internal use to handle sequences of tokens
package struct Sequence(Ts...){
  alias Elements = Ts;
}


/*

  Types of things we want to parse, and what we expect them to return:

  Lexable Token -> Token //like a normal syntax tagged type, I guess
  Token -> Token
  TokenType -> Token?
  @Syntax object -> that object
  * -> object[]
  + -> object[] of length 1+
  Optional -> Arg or nothing, either is fine
  Choice -> one of the options, or error
  Not -> nothing or error
  Sequence -> tuple of args or nothing
  Literals (tokens?) -> token or error

  keep?
 */
