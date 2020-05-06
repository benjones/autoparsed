module autoparsed.recursivedescent;
import autoparsed.syntax;
import autoparsed.autoparsed;
import autoparsed.log;

import std.stdio;

import std.typecons : nullable, Nullable, Tuple;
import std.traits;
import std.range.primitives;
import sumtype;

template contains(T, Ts...) {
  import std.meta : anySatisfy;
  enum isSame(U) = allSameType!(TokenType!T, U);
  alias contains = anySatisfy!(isSame, Ts);
}

template partOfStream(T, TArgs){
  import std.range;
  enum partOfStream = contains!(T, TemplateArgsOf!TArgs);
}


///parse a lexable token from a tokenStream
///If the stream can return this token, this doesn't apply
///This is used by the lexer
Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Lex) &&
   (!isInstanceOf!(SumType, typeof(tokenStream.front())) ||
    !partOfStream!(T, typeof(tokenStream.front())))){

  alias uda = getUDAs!(T, Lex);
  mixin CTLog!("Parser for Lexable token `", T, "` with UDA `", uda, "`");

  RTLog("parsing lexable token `", T.stringof, "` from stream: ", tokenStream);

  alias tArgs = TemplateArgsOf!uda;
  static if(tArgs.length > 1){
    alias parseType = Sequence!(tArgs);
  } else {
    alias parseType = tArgs;
  }
  auto parsed = parse!(parseType)(tokenStream);

  static if(isInstanceOf!(Nullable, typeof(parsed))){
    if(parsed.isNull){
      return Nullable!T();
    } else {
      return Nullable!T(construct!T(parsed.get));
    }
  } else {
    if(parsed){
      return Nullable!T(construct!T(parsed));
    } else {
      return Nullable!T();
    }
  }
}

///return a T if it's at the front of the stream
Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Token) &&
   isInstanceOf!(SumType, typeof(tokenStream.front())) &&
   partOfStream!(T, typeof(tokenStream.front()))){
  
  mixin CTLog!("Parser for already lexed Token `", T, "` from stream of type `", TokenStream, "`");
  RTLog("parsing token `", T.stringof, "` from stream: ", tokenStream);

  if(tokenStream.empty) return Nullable!T();
  return tokenStream.front.match!(
    (TokenType!T t){
      auto ret = t.value.nullable;
      tokenStream.popFront;
      return ret;
    },
    _ => Nullable!T()
                                  );

}

///Parse a token, forwards either to another overload, or the literal checker
Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(TokenType, T)){
  import std.traits: isType;
  mixin CTLog!("Forwarding Parser for TokenType wrapper `", T, "`");

  alias TArgs = TemplateArgsOf!T;

  static if(isType!(TArgs)){
    auto res = parse!(TemplateArgsOf!T)(tokenStream);
    return res.isNull ? Nullable!T() : Nullable!T(T(res.get));
  } else {
    return parse!TArgs(tokenStream) ? Nullable!T(T()) : Nullable!T();
  }

}

///parse an element that has a constructor annotated with the @Syntax UDA
T parse(T, TokenStream)(ref TokenStream tokenStream)
if(annotatedConstructors!(T).length > 0) {
  import std.traits : getUDAs, isType, Parameters;
  import std.meta : staticMap;

  alias ac = annotatedConstructors!T;
  alias syntax = getUDAs!(ac, Syntax)[0];

  mixin CTLog!("Parser for type `", T, "` with annotated constructor syntax`", syntax, "`");
  RTLog("parsing token `", T.stringof, "` with annotated constructor from stream: ", tokenStream);

  alias Args = Parameters!(annotatedConstructors!(T)[0]);
  alias Seq = Sequence!(TemplateArgsOf!syntax);

  auto parsed =  parse!Seq(tokenStream);
  if(parsed.isNull){
    return null;
  } else {
    return new T(parsed.get);
  }

}

///parse a * expression (0 or more repeats)
auto parse(RS, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexStar, RS)){

  mixin CTLog!("Parser for RegexStar `", RS, "`");
  RTLog("parsing `", RS.stringof, "` from stream: ", tokenStream);

  alias Elems = TemplateArgsOf!RS;
  static if(Elems.length > 1){
    alias ElemType = Sequence!(Elems);
  } else {
    alias ElemType = Elems[0];
  }

  mixin CTLog!("Elem type: `", ElemType, "`");

  TokenStream copy = tokenStream;
  auto elem = parse!ElemType(copy);
  static if(isInstanceOf!(Nullable, typeof(elem))){
    alias RetType = TemplateArgsOf!(typeof(elem))[0];
  } else {
    alias RetType = typeof(elem);
    
  }
  
  RetType[] ret;
  while(!isNullish(elem)){
    static if(isInstanceOf!(Nullable, typeof(elem))){
      ret ~= elem.get;
    } else {
      ret ~= elem;
    }
    elem = parse!ElemType(copy);
  }

  if(ret.length > 0){
    tokenStream = copy; //something was consumed
  }

  RTLog("regex star returning: `", ret, "` with stream: ", tokenStream);
  return ret;
}

///parse a regex + expression (1 or more repeats)
auto parse(RP, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexPlus, RP)){

  mixin CTLog!("Parser for RegexPlus `", RP, "`");
  RTLog("parsing `", RP.stringof, "` from stream: ", tokenStream);
  
  alias StarType = RegexStar!(TemplateArgsOf!RP);
  auto ret = parse!(StarType)(tokenStream);

  return ret.length > 0 ? ret : null;
}

///parse a choice between alternatives.  Return the first successful option
auto parse(OO, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(OneOf, OO)){
  import std.typecons : nullable;

  mixin CTLog!("Parser for OneOf `", OO, "`");
  RTLog("parsing `", OO.stringof, "` from stream: ", tokenStream);

  alias Ts = TemplateArgsOf!(OO.NodeType);
  static foreach(T; Ts){{
      TokenStream copy = tokenStream; //only advance the stream on success
      auto res = parse!T(copy);

      static if(isInstanceOf!(Nullable, typeof(res))){
        if(!res.isNull){
          auto ret = nullable(OneOf!(Ts).NodeType(res.get));
          tokenStream = copy;
          RTLog("one of successfully parsed ` ", ret,
                "` with type `", typeof(ret).stringof, "` with stream: ", tokenStream);

          return ret;
        }
      } else {
        if(res !is null){
          tokenStream = copy;

          RTLog("one of successfully parsed ` ", res,
                "` with type `", typeof(res).stringof, "` with stream: ", tokenStream);
          return nullable(OneOf!(Ts).NodeType(res));
        }
      }
  }}
  return Nullable!(OneOf!(Ts).NodeType)();
}

///returns true if you cannot parse N from the stream right now
///Does not consume any tokens from the stream
bool parse(N, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Not, N)){

  mixin CTLog!("Parser for Not `", N, "`");
  RTLog("parsing Not element, `", N.stringof, "`, from stream: ", tokenStream);
  
  TokenStream copy = tokenStream; //don't consume any input
  auto ret = isNullish(parse!(TemplateArgsOf!N)(copy));
  RTLog("returning from Not element, `", N.stringof, "`, from stream: ", tokenStream, " with ", ret);
  return ret;

}

///returns the next token, whatever it is
auto parse(T: Token, TokenStream)(ref TokenStream tokenStream){
  mixin CTLog!("Parser for wildcard token");
  RTLog("parsing wildcard token from stream: ", tokenStream);

  alias U = typeof(tokenStream.front());
  if(tokenStream.empty){ return Nullable!U(); }
  
  auto ret = tokenStream.front;
  tokenStream.popFront;
  return nullable(ret);
}

///parse a sequence of tokens in orders
auto parse(S, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Sequence, S)){

  mixin CTLog!("Parser for Sequence `", S, "`");
  RTLog("parsing `", S.stringof, "` from stream: ", tokenStream);
  
  import std.typecons : Tuple;
  import std.meta : ReplaceAll;
  
  alias Ts = S.Elements;
  alias TType = ElementType!TokenStream;
  alias TokensReplaced = ReplaceTokensRecursive!(TType, Ts);
  alias Values = ValueTypes!TokensReplaced;

  mixin CTLog!(S, ": values: ", Values);
  static if(Values.length > 1){
    alias RetType = Tuple!(Values);
  } else {
    alias RetType = Values[0];
  }
  mixin CTLog!(S, ": Ret type: ", RetType);
  Nullable!RetType ret;

  
  static size_t argNumber(size_t syntaxNumber)(){
    size_t v = 0;
    static foreach(i; 0..syntaxNumber){
      static if(hasValue!(Ts[i])){
        ++v;
      }
    }
    return v;
  }

  void set(size_t i, T)(T val){
    static if(isInstanceOf!(Tuple, RetType)){
      ret[i] = val;
    } else {
      static assert(i == 0);
      ret = val;
    }
  }

  TokenStream copy = tokenStream; //don't advance on failure
  static foreach(i, elem; Ts){{
    static if(isType!elem){

      static if(isInstanceOf!(Not, elem)){
        if(!parse!elem(copy)){
          return Nullable!RetType();
        }
      } else {
        auto x = parse!(elem)(copy);
        mixin CTLog!(S, ": typeof x: ", typeof(x));
        //an empty array from regexStar is OK
        static if(!isInstanceOf!(RegexStar, elem)){
          if(isNullish(x)){
            return Nullable!RetType();
          }
        }
        set!(argNumber!i)(x);
      }

    } else {
      if(!check!elem(copy)){
        return Nullable!RetType();
      }
    }
  }}

  tokenStream = copy;
  return ret;
}


template hasValue(alias T){
  enum hasValue = isType!T && !isInstanceOf!(Not, T);
}

template ValueTypes(Ts...){

  import std.meta : staticMap, Filter;

  alias TsWithValues = Filter!(hasValue, Ts);
  alias ValueTypes = staticMap!(ValueType, TsWithValues);
}

template ValueType(alias T){

  static if(isInstanceOf!(OneOf, T)){
    alias ValueType = T.NodeType;

  } else static if(isInstanceOf!(RegexStar, T) || isInstanceOf!(RegexPlus, T)){
    alias ChildType = ValueTypes!(TemplateArgsOf!T);
    alias ValueType = SliceOf!ChildType;
  } else {
    mixin CTLog!("val type neither");
    alias ValueType = T;
  }
}

///return true if the next token ins the stream is an "S"
bool check(alias S, TokenStream)(ref TokenStream tokenStream){
  import std.range;

  mixin CTLog!("Parser(check) for literal `", S, "`");
  RTLog("parsing(checking) for `", S.stringof, "` from stream: ", tokenStream);
  
  if(tokenStream.empty) return false;
  static if(isInstanceOf!(SumType, typeof(tokenStream.front()))){
    return tokenStream.front.match!(
      (TokenType!S t) {
        tokenStream.popFront();
        return true;
      },
      _ => false);
  } else {
    if(tokenStream.front == S){
      tokenStream.popFront();
      return true;
    } else {
      return false;
    }
  }
}
  
bool isNullish(T)(const auto ref T t){
  import std.traits : isPointer, isDynamicArray;
  static if(isInstanceOf!(Nullable, T)){
    return t.isNull();
  }  else static if(isPointer!T){
    return t is null;
  } else static if(isDynamicArray!T){
    return t.length == 0;
  } else {
    return !t;
  }
}



///Is a literal at the front?
bool parse(alias Lit, TokenStream)(ref TokenStream tokenStream){
  mixin CTLog!("Parse(non-check) literal `", Lit, "`");
  RTLog("parsing(non-check) for `", Lit.stringof, "` with type ", typeof(Lit).stringof," from stream: ", tokenStream);

  if(tokenStream.empty()){
    RTLog("stream empty, returning false");
    return false; }
  if(tokenStream.front == Lit){
    RTLog("found it, returning true");
    tokenStream.popFront;
    return true;
  } else {
    RTLog("didn't find it, returning false");
    return false;
  }
}

template CArgs(T){
  static assert(isType!T && isAggregateType!T);
  static if(hasMember!(T, "__ctor")){
    //  pragma(msg, T, " has a contructor");
    alias CArgs = Parameters!(__traits(getMember, T, "__ctor"));
    
  } else {
    //  pragma(msg, T, " doesn't have a constructor");
    alias CArgs = Fields!T;
  }
}

template ConstructableWith(T, Args...){
  enum ConstructableWith = is(Tuple!(CArgs!T) == Tuple!Args);
}



template CommonValueType(T) if(isInstanceOf!(SumType, T)){
  import std.meta : allSatisfy;

  template isValueToken(T){
    static if(isInstanceOf!(TokenType,T)){
      enum isValueToken = !isType!(TemplateArgsOf!T[0]);
    } else {
      enum isValueToken = false;
    }
  }

  enum allValueTokens = allSatisfy!(isValueToken, TemplateArgsOf!T);
  //  pragma(msg, " are all value tokens? ", allValueTokens);
  static if(allValueTokens){

    alias getValue(T)= TemplateArgsOf!T[0];
    alias TokenValues = staticMap!(getValue, TemplateArgsOf!T);
    alias getType(alias V) = typeof(V);
    alias ValueTypes = staticMap!(getType, TokenValues);
    //  pragma(msg, "Value Types", ValueTypes);
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

  //  pragma(msg, "has common type? ", HasCommonType);
  static if(!HasCommonType){
    alias CommonValueType = void;
  }
}




Target convert(Target, Src)(Src src){
  import std.conv : to;

  static if(isInstanceOf!(SumType, Src)){

    //can a common type work for all the variants?
    //  pragma(msg, "Checking compatibility, sumtype: ", Src);
    alias CVT = CommonValueType!Src;
    static assert(!is(CVT == void), "Can't pass a " ~ Src.stringof ~
                  " as expected constructor argument type " ~ Target.stringof);

    CVT val = src.match!(x => x.value);
    return to!Target(val);

  } else static if(isArray!Target && isArray!Src){
    import std.algorithm: map;
    import std.array;

    return map!(convert!(ForeachType!Target, ForeachType!Src))(src).array;
  } else {
    return to!(Target)(src);
  }
}



T construct(T, Args...)(Args args){

  //  pragma(msg, "Constructable? :", T, " Args: ", Args, " ? ");
  //  pragma(msg, ConstructableWith!(T, Args));
  static if(CArgs!T.length != Args.length){
    static assert(false, "Wrong number of args provided.  T is constructable with " ~ CArgs!T.stringof
                  ~ " but attempting to construct with " ~ Args.stringof); 
  }

  static if(ConstructableWith!(T, Args)){
    return T(args);
  } else {
    CArgs!T cargs;
    static foreach(i, Arg; CArgs!T){
      static if(is(Arg == Args[i])){
        cargs[i] = args[i];
      } else {
        cargs[i] = convert!(Arg)(args[i]);
      }
    }
    return T(cargs);
  }
}

template ReplaceTokenRecursive(Replacement, alias T)
if(!isType!T){
  alias ReplaceTokenRecursive = T;
 }

template ReplaceTokenRecursive(Replacement, T){
  static if(isType!T){
    static if(is(TemplateArgsOf!T)){
      alias Ts = TemplateArgsOf!T;
      alias Temp = TemplateOf!T;
      alias ReplaceTokenRecursive = Temp!(ReplaceTokensRecursive!(Replacement, Ts));
    } else {
      static if(is(T == Token)){
        alias ReplaceTokenRecursive = Replacement;
      } else {
        alias ReplaceTokenRecursive = T;
      }
    }
  } else {
    alias ReplaceTokenRecursive = T;
  }
}

template ReplaceTokensRecursive(Replacement, Ts...){
  import std.meta : staticMap;
  alias F(alias T) = ReplaceTokenRecursive!(Replacement, T);
  alias ReplaceTokensRecursive = staticMap!(F, Ts);

}

unittest {
  static assert(is(ReplaceTokensRecursive!(dchar, '"', (RegexStar!(Not!'"', Token)), '"', "`") ==
                   RegexStar!(Not!'"', dchar)));
}


template SliceOf(T){
  alias SliceOf = T[];
}
