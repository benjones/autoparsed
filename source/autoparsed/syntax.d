module autoparsed.syntax;

import autoparsed.log;
import autoparsed.traits;

///UDA type for annotating constructors as grammar rules
struct Syntax(T...){
  alias Elements = T;
}

///UDA type for annotating the syntax for Tokens
///Similar to synatx, but "tokens" will be chars here
/*struct Lex(T...){
  alias Elements = T;
  }*/

///UDA type for annotating that a type or enum is a token
enum Token;

///Wrapper type necessary for value tokens '(', ')', whitespace, etc
struct TokenType(alias T){
  import std.traits : isType;
  mixin CTLog!("insantiating TokenType of ", T);

  static if(isType!T){
    mixin CTLog!(T, "is a type!");
    alias type = T;
    static if(isInstantiable!T){
      mixin CTLog!("and", T, "is instantiable");
      T value;
    } else {
      mixin CTLog!("but", T, "is not instantiable");
    }
  } else {
    mixin CTLog!("it's NOT a type!");
    alias type = typeof(T);
    enum value = T;
  }
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
  import std.sumtype;
  import autoparsed.traits;
  template Wrap(alias T){
    static if(!isType!T || !isInstantiable!T) {
      alias Wrap = TokenType!T;
    } else {
      alias Wrap = T;
    }
  }

  alias WrappedTypes = staticMap!(Wrap, Ts);
  //pragma(msg, "Wrapped types for ", Ts, ": ", WrappedTypes);

  struct NodeType{

    template CommonValueType(Ts...){

      template isValueToken(T){
        static if(isInstanceOf!(TokenType,T)){
          enum isValueToken = !isType!(TemplateArgsOf!T[0]);
        } else static if(isInstanceOf!(InRange, T)){
          enum isValueToken = true;
        } else {
          enum isValueToken = false;
        }
      }

      enum allValueTokens = allSatisfy!(isValueToken, Ts);
      static if(allValueTokens){

        template getValueType(T){
          static if(isInstanceOf!(TokenType, T)){
            alias getValueType = T.type;
          } else static if(isInstanceOf!(InRange, T)){
            alias getValueType = T.LimitType;
          } else {
            static assert(false);
          }
        }
        /*alias getValue(T)= TemplateArgsOf!T[0];
        alias TokenValues = staticMap!(getValue, Ts);
        alias getType(alias V) = typeof(V);
        */

        alias ValueTypes = staticMap!(getValueType, Ts);

        alias CT = CommonType!(ValueTypes);
        static if(is(CT == void)){
          enum HasCommonType = false;
        } else {
          enum HasCommonType = true;
          alias CommonValueType = CT;
        }

      } else {
        enum HasCommonType = false;
      }

      static if(!HasCommonType){
        alias CommonValueType = void;
      }
    }

    static if(is(CommonValueType!(WrappedTypes) == void)){
      alias ST = SumType!WrappedTypes;
      ST data;
      alias data this;
      static foreach(WT; WrappedTypes){
        this(WT wt){
          data = wt;
        }
      }
      this(ST st){
        data = st;
      }
    } else {
      CommonValueType!WrappedTypes data;
      alias data this;

      this(X)(X x) if(is(typeof(cast(typeof(data))X.init))){ //char/dchar... ugh
        data = cast(typeof(data))x;
      }

      this(X)(X x)
        if(is(X : TokenType!Lit, alias Lit) &&
           is(typeof(cast(typeof(data))Lit))){ //complicated because of char/dchar conversion
        data = TemplateArgsOf!X[0];
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

///PEG rule satisfied when token in inclusive range [First, Last]
struct InRange(alias First, alias Last){
  static assert( is(typeof(First) : typeof(Last)) || is(typeof(Last) : typeof(First)),
                 "type of InRange limits must be compatible");

  static if(is(typeof(First) : typeof(Last)))
    alias LimitType = typeof(Last);
  else
    alias LimitType = typeof(First);

}

/// represent a sequences of tokens
/*struct Sequence(Ts...){
  alias Elements = Ts;
  }*/


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
