module autoparsed.recursivedescent;
import autoparsed.syntax;
import autoparsed.autoparsed;
import autoparsed.log;

import std.stdio;
import std.conv : to;
import std.typecons : nullable, Nullable, Tuple;
import std.traits;
import std.range.primitives;
import sumtype;

template TypesOnly(T...){
  import std.meta;
  alias TypesOnly = Filter!(isType, T);
}

struct Payload(T){
  alias Types = T;
  static if(!is(T == void))
    T contents;
}

struct ParseError(T){
  alias Types = T;
  T contents;
}

alias DefaultError = ParseError!string;


struct ParseResult(P, PE)
if(is(P : Payload!Args1, Args1) && is(PE : ParseError!Args2, Args2)){
  SumType!(P, PE) data;
  alias data this;
  alias PayloadType = P;
  this(P p){ data = p;}
  this(PE pe){ data = pe;}
}

bool isParseError(PR: ParseResult!(P, PE), P, PE)(PR pr){
  return pr.data.match!(
    (P p) => false,
    (PE pe) => true);
}

auto getPayload(PR: ParseResult!(P, PE), P, PE)(PR pr){
  return pr.data.match!(
    (P p) => p,
    function P(PE _){assert(false);});
}

auto getParseError(PR: ParseResult!(P, PE), P, PE)(PR pr){
  return pr.data.match!(
    (PE pe) => pe,
    function PE(P _){assert(false);}
  );
}

unittest {
    alias PEmpty = Payload!();
    alias PInt = Payload!int;
    alias PID = Payload!(int, double);

    alias PEEmpty = ParseError!();
    alias PEString = ParseError!string;
    alias PESS = ParseError!(string, string);

    ParseResult!(PEmpty, PEEmpty) p1;
    ParseResult!(PInt, PEEmpty) p2;
    ParseResult!(PEmpty, PEString) p3;
    ParseResult!(PInt, PEString) p4;

    ParseResult!(PID, PESS) p5;
}


template contains(T, Ts...) {
  import std.meta : anySatisfy;
  enum isSame(U) = allSameType!(T, U);
  alias contains = anySatisfy!(isSame, Ts);
}

template partOfStream(T, StreamElement){
  import std.range;
  pragma(msg, "POS check: T: ", T, " StreamElem: ", StreamElement);
  static if(is(StreamElement.ST)){ pragma(msg, " .ST: ", StreamElement.ST);}
  static if(is(StreamElement == OneOf!Args2.NodeType, Args2)){
    pragma(msg, "one of check passed");
    pragma(msg, "TA of ST.ST: ", TemplateArgsOf!(StreamElement.ST));
    pragma(msg, "contains check: ", contains!(T, TemplateArgsOf!(StreamElement.ST)));
  }
  enum partOfStream = is(StreamElement == OneOf!Args.NodeType, Args) &&
    contains!(T, TemplateArgsOf!(StreamElement.ST));
  pragma(msg, "result: ", partOfStream);
    //enum partOfStream = contains!(T, TemplateArgsOf!TArgs);
}


///parse a lexable token from a tokenStream
///If the stream can return this token, this doesn't apply
///This is used by the lexer
auto parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Lex) &&
   //   (!isInstanceOf!(SumType, typeof(tokenStream.front())) ||
    !partOfStream!(T, typeof(tokenStream.front()))){

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

  alias PayloadType = Payload!T;
  alias RetType = ParseResult!(PayloadType, DefaultError);
  pragma(msg, "PT: ", PayloadType, " RT ", RetType);
  pragma(msg, "type of pasrsed: ", typeof(parsed));
  
  return parsed.data.match!(
    (typeof(parsed).PayloadType payload){
      T toRet = construct!T(payload.contents);
      return RetType(PayloadType(toRet));
    },
    _ => RetType(DefaultError(to!string(_))));
  
  /*
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
    }*/
}

///return a T if it's at the front of the stream
auto parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Token) &&
   partOfStream!(T, typeof(tokenStream.front()))){
  
  mixin CTLog!("Parser for already lexed Token `", T, "` from stream of type `", TokenStream, "`");
  RTLog("parsing token `", T.stringof, "` from stream: ", tokenStream);

  alias PayloadType = Payload!T;
  alias ResultType = ParseResult!(PayloadType, DefaultError);
  
  if(tokenStream.empty) return ResultType(DefaultError("empty stream"));
  return tokenStream.front.match!(
    (T t){
      auto ret = ResultType(PayloadType(t));
      tokenStream.popFront;
      return ret;
    },
    _ => ResultType(DefaultError( "looking for "~ T.stringof~ " but found "~ to!string(tokenStream.front)))
  );
  
}

///Parse a token, forwards either to another overload, or the literal checker
auto parse(T, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(TokenType, T)){
  import std.traits: isType;
  mixin CTLog!("Forwarding Parser for TokenType wrapper `", T, "`");

  alias TArgs = TemplateArgsOf!T;
  return parse!TArgs(tokenStream);
  /*
  static if(isType!(TArgs)){
    auto res = parse!(TemplateArgsOf!T)(tokenStream);
    return res.isNull ? Nullable!T() : Nullable!T(T(res.get));
  } else {
    return parse!TArgs(tokenStream) ? Nullable!T(T()) : Nullable!T();
    }*/

}

template SyntaxReturnType(T){
  alias PayloadType = Payload!T;
  alias SyntaxReturnType = ParseResult!(PayloadType, DefaultError);
}

///parse an element that has a constructor annotated with the @Syntax UDA
SyntaxReturnType!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(annotatedConstructors!(T).length > 0) {
  import std.traits : getUDAs, isType, Parameters;
  import std.meta : staticMap;

  alias ac = annotatedConstructors!T;
  alias syntax = getUDAs!(ac, Syntax)[0];

  mixin CTLog!("Parser for type `", T, "` with annotated constructor syntax`", syntax, "`");
  RTLog("parsing token `", T.stringof, "` with annotated constructor from stream: ", tokenStream);

  alias Args = Parameters!(annotatedConstructors!(T)[0]);
  alias Seq = Sequence!(TemplateArgsOf!syntax);
  pragma(msg, "seq is: ", Seq);
  auto parsed =  parse!Seq(tokenStream);
  pragma(msg, "typeof parsed payload in @syntax parser: ", ElementType!(typeof(parsed).PayloadType.Types).ST);
  alias PayloadType = Payload!T;
  alias RetType = ParseResult!(PayloadType, DefaultError);
  
  if(parsed.isParseError){
    return RetType(parsed.getParseError);
  } else {
    return RetType(PayloadType(new T(parsed.getPayload.contents)));
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

  static assert(!isInstanceOf!(Optional, ElemType), "RegexStar of an Optional doesn't make sense");
  
  TokenStream copy = tokenStream;
  auto elem = parse!ElemType(copy);

  alias RetType = typeof(elem).PayloadType.Types; //? always 1 arg here?
  /*
  static if(isInstanceOf!(Nullable, typeof(elem))){
    alias RetType = TemplateArgsOf!(typeof(elem))[0];
  } else {
    alias RetType = typeof(elem);
    
    }*/
  
  RetType[] ret;
  pragma(msg, "Regex star ret type: ", typeof(ret));
  pragma(msg, "typeof elem ", typeof(elem), " typeof payload contents: ", typeof(elem.getPayload.contents));
  
  while(!elem.isParseError){
    ret ~= elem.getPayload.contents;
    /*static if(isInstanceOf!(Nullable, typeof(elem))){
      ret ~= elem.get;
    } else {
      ret ~= elem;
      }*/
    elem = parse!ElemType(copy);
  }

  if(ret.length > 0){
    tokenStream = copy; //something was consumed
  }

  RTLog("regex star returning: `", ret, "` with stream: ", tokenStream);
  alias PayloadType = Payload!(typeof(ret));
  return ParseResult!(PayloadType, DefaultError)(PayloadType(ret));
}

///parse a regex + expression (1 or more repeats)
auto parse(RP, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexPlus, RP)){

  mixin CTLog!("Parser for RegexPlus `", RP, "`");
  RTLog("parsing `", RP.stringof, "` from stream: ", tokenStream);
  
  alias StarType = RegexStar!(TemplateArgsOf!RP);
  auto ret = parse!(StarType)(tokenStream);

  pragma(msg, "ret type: ", typeof(ret));
  alias PayloadType = typeof(ret).PayloadType;
  pragma(msg, "payload type: ", PayloadType);
  alias RetType = ParseResult!(PayloadType, DefaultError);
  
  return ret.getPayload.contents.length > 0 ? RetType(ret.getPayload) :
    RetType(DefaultError("Regex+ didn't find any " ~ RP.stringof));
}

///parse an optional element.  Is always "successful" so, don't check the return value to decide
auto parse(Opt, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Optional, Opt)){
  mixin CTLog!("Parser for Optional `", Opt, "`");
  RTLog("Parsing `", Opt.stringof, "` from stream: ", tokenStream);
  //defer to the element parse, return whatever they do
  alias Args = TemplateArgsOf!Opt;
  static if(Args.length > 1){
    return parse!(Sequence!Args)(tokenStream);
  } else {
    return parse!(Args)(tokenStream);
  }
 }

///parse a choice between alternatives.  Return the first successful option
auto parse(OO, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(OneOf, OO)){
  import std.typecons : nullable;

  mixin CTLog!("Parser for OneOf `", OO, "`");
  RTLog("parsing `", OO.stringof, "` from stream: ", tokenStream);

  alias Ts = TemplateArgsOf!(OO.NodeType);
  alias OONT = OneOf!(Ts).NodeType;
  alias PayloadType = Payload!(OONT);
  alias RetType = ParseResult!(PayloadType, DefaultError);
  
  static foreach(T; Ts){{
      TokenStream copy = tokenStream; //only advance the stream on success
      pragma(msg, "OO: ", OO, " trying to parse ", T);
      auto res = parse!T(copy);
      pragma(msg, "typeof res: ", typeof(res));
      if(!res.isParseError){
        auto oont = OONT(res.getPayload.contents);
        tokenStream = copy;
        return RetType(PayloadType(oont));
      }
      
  }}
  return RetType(DefaultError("None of " ~ Ts.stringof ~ " could be parsed"));
  /*
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
  */
}

///returns true if you cannot parse N from the stream right now
///Does not consume any tokens from the stream
auto parse(N, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Not, N)){

  mixin CTLog!("Parser for Not `", N, "`");
  RTLog("parsing Not element, `", N.stringof, "`, from stream: ", tokenStream);

  alias ElemType = TemplateArgsOf!N;
  static assert(!isInstanceOf!(Optional, ElemType), "Not of Optional doesn't make sense");

  TokenStream copy = tokenStream; //don't consume any input
  auto ret = parse!(ElemType)(copy);
  alias PayloadType = Payload!void;
  alias RetType = ParseResult!(PayloadType, DefaultError);
  if(!ret.isParseError){
    return RetType(DefaultError("Not failed, could parse " ~ typeof(ret).stringof));
  } else {
    
    RTLog("returning from Not element, `", N.stringof, "`, from stream: ", tokenStream, " with ", ret);
    return RetType(PayloadType());
  }

}

///returns the next token, whatever it is
auto parse(T: Token, TokenStream)(ref TokenStream tokenStream){
  mixin CTLog!("Parser for wildcard token");
  RTLog("parsing wildcard token from stream: ", tokenStream);

  alias U = typeof(tokenStream.front());
  alias PayloadType = Payload!U;
  alias RetType = ParseResult!(PayloadType, DefaultError);
  
  if(tokenStream.empty){ return RetType(DefaultError("empty stream")); }
  
  auto ret = tokenStream.front;
  tokenStream.popFront;
  return RetType(PayloadType(ret));
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

  RetType ret;
  alias PayloadType = Payload!RetType;
  alias ResultType = ParseResult!(PayloadType, DefaultError);
  pragma(msg, "Seq result type: ", ResultType);

  
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
    pragma(msg, "setting ", i, " with ", T);
    static if(isInstanceOf!(Tuple, RetType)){
      ret[i] = val;
    } else {
      static assert(i == 0);
      pragma(msg, "typeof ret: ", typeof(ret), " typeof val: ", typeof(val));
      static if(is(typeof(ret) == typeof(val))){
        ret = val;//nullish check already happened, so get should be fine
      } else static if(isInstanceOf!(Nullable, typeof(val)) && is(typeof(val) == Nullable!(typeof(ret)))){
        pragma(msg, "assigning value from a nullable");
        ret = val.get();
      } else {
        pragma(msg, "targs typeof val: ", TemplateArgsOf!(typeof(val)), " typeof(ret) ", typeof(ret));
        assert(0, "can't assign value in sequence set");
        static assert(false);
      }
    }
  }

  TokenStream copy = tokenStream; //don't advance on failure
  static foreach(i, elem; Ts){{
      pragma(msg, "OO elem: ", elem);
      auto piece = parse!elem(copy);
      if(piece.isParseError){
        static if(is(elem == Token)){ enum elemName = "Token"; }
        else enum elemName = elem.stringof;
        return ResultType(DefaultError("Couldn't parse " ~ elemName));
      }
      alias piecePayloadTypes = typeof(piece).PayloadType.Types;
      pragma(msg, "piece: ", typeof(piece), " elem: ", elem, " types: ", piecePayloadTypes);
      //does piece actually hold some data?
      static if(!is(piecePayloadTypes == void) && !is(piecePayloadTypes == TokenType!Lit, alias Lit)){
        set!(argNumber!i)(piece.getPayload.contents);
      }
    }}
    tokenStream = copy;
  return ResultType(PayloadType(ret));
  /*
    static if(isType!elem){
      
      static if(isInstanceOf!(Not, elem)){
        if(!parse!elem(copy)){
          return Nullable!RetType();
        }
      } else {
        auto x = parse!(elem)(copy);
        mixin CTLog!(S, ": i: ", i, " typeof x: ", typeof(x));
        //an empty array from regexStar is OK
        static if(!isInstanceOf!(RegexStar, elem) && !isInstanceOf!(Optional, elem)){
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
  return nullable(ret);
  */
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
  pragma(msg, "val type for ", T);
  static if(isInstanceOf!(OneOf, T)){
    alias ValueType = T.NodeType;

  } else static if(isInstanceOf!(RegexStar, T) || isInstanceOf!(RegexPlus, T)){
    alias ChildType = ValueTypes!(TemplateArgsOf!T);
    alias ValueType = SliceOf!ChildType;
  } else static if(isInstanceOf!(Optional, T)){
    alias ChildType = ValueTypes!(TemplateArgsOf!T);
    alias ValueType = Nullable!ChildType;
  } else static if(isInstanceOf!(Keep, T)){
    //Not nullable.  Return/value types are different here (I think that makes sense)
    alias ValueType = typeof(TemplateArgsOf!T); //TODO, handle general cases
    pragma(msg, "ValueType for Keep: ", ValueType);
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


///Parse a kept literal
auto parse(KL, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Keep, KL)){
  mixin CTLog!("Parse Kept literal: `", KL, "`");
  RTLog("Parsing Kept literal: `", KL.stringof, "` from stream: ", tokenStream);
  
  alias Val = TemplateArgsOf!KL;
  alias ValType = typeof(Val);
  return isNullish(parse!Val(tokenStream)) ? Nullable!ValType() : Nullable!ValType(Val);
}

///Is a literal at the front?
auto parse(alias Lit, TokenStream)(ref TokenStream tokenStream){

  RTLog("parsing(non-check) for `", Lit.stringof, "` with type ", typeof(Lit).stringof," from stream: ", tokenStream);

  alias StreamElementType = ElementType!TokenStream;
  alias PayloadType = Payload!(TokenType!Lit);
  alias RetType = ParseResult!(PayloadType, DefaultError);
  mixin CTLog!("Parse(non-check) literal `", Lit, "`",
               "payload type: `", PayloadType, "` RetType: `", RetType, "`");
  pragma(msg, "steram type", TokenStream);
  if(tokenStream.empty()){
    RTLog("stream empty, returning false");
    return RetType(DefaultError("empty stream"));
  } else {
    //stream is one of many possible token types
    static if(is(StreamElementType == OneOf!(Args).NodeType, Args...)){
      pragma(msg, "stream elemtn type: ", StreamElementType.ST);
      auto fn = tokenStream.front;
      pragma(msg, "front now: ", typeof(fn), typeof(fn.data));

      auto ret = tokenStream.front.data.match!(
        function RetType(TokenType!Lit tl){ return RetType(PayloadType(TokenType!Lit()));},
        _ => RetType(DefaultError("did't find " ~ Lit ~ " token"))
      );
      if(!ret.isParseError)
        tokenStream.popFront;
      return ret;
      
    } else { //stream is probably a dchar[]
      if(tokenStream.front == Lit){
        RTLog("found it, returning true");
        auto ret = RetType(PayloadType());
        tokenStream.popFront;
        RTLog("stream after parsing literal: ", tokenStream);
        return ret;
      } else {
        RTLog("didn't find it, returning false");
        return RetType(DefaultError("Looking for "~ Lit~ " but found "~ to!string(tokenStream.front)));
      }
    }
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



template CommonValueType(T) if(is(T : OneOf!Args.NodeType, Args)){
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
  pragma(msg, "convert, src ", Src, " Target: ", Target);
  static if(is(Src: OneOf!Args.NodeType, Args...)){

    //can a common type work for all the variants?
    pragma(msg, "Checking compatibility, OneOF NodeType: ", Src, " ST: ", Src.ST);
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
    //  static assert(is(ReplaceTokensRecursive!(dchar, '"', (RegexStar!(Not!'"', Token)), '"', "`") ==
    //                   RegexStar!(Not!'"', dchar)));
}


template SliceOf(T){
  alias SliceOf = T[];
}
