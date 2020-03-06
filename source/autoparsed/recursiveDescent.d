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


///parse an lexable token from a tokenStream
///If the stream can return this token, this doesn't apply
///This is used by the lexer
Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Lex) &&
   (!isInstanceOf!(SumType, typeof(tokenStream.front())) ||
	!partOfStream!(T, typeof(tokenStream.front())))){

  alias uda = getUDAs!(T, Lex);
  mixin CTLog!("Parser for Lexable token `", T, "` with UDA `", uda, "`");
  
  auto parsed = parse!(TemplateArgsOf!uda)(tokenStream);

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

  writeln("parsing token ", T.stringof, " from stream ", tokenStream);
  if(tokenStream.empty) return Nullable!T();
  return tokenStream.front.match!(
    (TokenType!T t){
	  auto ret = t.value.nullable;
	  tokenStream.popFront;
	  writeln("matched token, returning ", ret);
	  return ret;
	},
	_ => Nullable!T()
  );

}

///Pare a token, forwards either to another overload, or the literal checker
Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(TokenType, T)){
  import std.traits: isType;
  mixin CTLog!("Forwarding Parser for TokenType wrapper `", T, "`");

  static if(isType!(TemplateArgsOf!T)){
	auto res = parse!(TemplateArgsOf!T)(tokenStream);
	return res.isNull ? Nullable!T() : Nullable!T(T(res.get));
  } else {
	return parseLiteral!T(tokenStream) ? Nullable!T(T()) : Nullable!T();
  }

}

///parse an element that has a constructor annotated with the @Syntax UDA
T parse(T, TokenStream)(ref TokenStream tokenStream)
if(annotatedConstructors!(T).length > 0) {
  import std.traits : getUDAs, isType, Parameters;
  import std.meta : staticMap;


  writeln("parsing ", T.stringof, " token stream: ", tokenStream);

  alias ac = annotatedConstructors!T;
  alias syntax = getUDAs!(ac, Syntax)[0];
  mixin CTLog!("Parser for type `", T, "` with annotated constructor syntax`", syntax, "`");

  /*static foreach(i, x; TemplateArgsOf!syntax){
	pragma(msg, fullyQualifiedName!(TemplateArgsOf!syntax[i]));
	}*/


  alias Args = Parameters!(annotatedConstructors!(T)[0]);
  //pragma(msg, "constructor args: ");
  //pragma(msg, Args);
  alias Seq = Sequence!(TemplateArgsOf!syntax);
  //pragma(msg, "seq:");
  //pragma(msg, Seq);
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
  writeln("parsing regex star ", RS.stringof, " token stream: ", tokenStream);
  mixin CTLog!("Parser for RegexStar `", RS, "`");

  alias Elems = TemplateArgsOf!RS;
  static if(Elems.length > 1){
	alias ElemType = Sequence!(Elems);
  } else {
	alias ElemType = Elems[0];
  }
  TokenStream copy = tokenStream;
  auto elem = parse!ElemType(copy);
  static if(isInstanceOf!(Nullable, typeof(elem))){
	//	pragma(msg, "nullable elem type: ");
	//	pragma(msg, ElemType);
	alias RetType = TemplateArgsOf!(typeof(elem))[0];
  } else {
	alias RetType = typeof(elem);
	
  }
  
  RetType[] ret;
  /*  pragma(msg, "RS parser, ret type:");
  pragma(msg, RetType);
  pragma(msg, "typeof elem");
  pragma(msg, typeof(elem));
  pragma(msg, "typeof ret");
  pragma(msg, typeof(ret));
  pragma(msg, "typeof RetType[]");
  pragma(msg, RetType[]);*/
  while(!isNullish(elem)){
	writeln("elem not nullish: ", elem);
	writeln("trying to append a ", typeof(elem).stringof, " to a ", typeof(ret).stringof);
	static if(isInstanceOf!(Nullable, typeof(elem))){
	  ret ~= elem.get;
	} else {
	  ret ~= elem;
	}
	writeln("appended elem to ret in regexstar: ", ret);
	elem = parse!ElemType(copy);
  }

  if(ret.length > 0){
	tokenStream = copy; //something was consumed
  }
  writeln("regex star returning: ", ret, " stream is ", tokenStream);
  return ret;
}

///parse a regex + expression (1 or more repeats)
auto parse(RP, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexPlus, RP)){

  writeln("parsing regex plus ", RP.stringof, " token stream: ", tokenStream);

  mixin CTLog!("Parser for RegexPlus `", RP, "`");

  alias StarType = RegexStar!(TemplateArgsOf!RP);
  auto ret = parse!(StarType)(tokenStream);
  //  pragma(msg, "RP got this type back from RS: ");
  //  pragma(msg, typeof(ret));
  return ret.length > 0 ? ret : null;
}

///parse a choice between alternatives.  Return the first successful option
auto parse(OO, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(OneOf, OO)){
  import std.typecons : nullable;
  
  writeln("parsing one of ", OO.stringof, " token stream: ", tokenStream);

  mixin CTLog!("Parser for OneOf `", OO, "`");

  alias Ts = TemplateArgsOf!(OO.NodeType);
  static foreach(T; Ts){{
	  TokenStream copy = tokenStream; //only advance the stream on success
	  writeln("trying option: ", T.stringof);
	  auto res = parse!T(copy);

	  /*pragma(msg, "\nrestype");
	  pragma(msg, typeof(res));
	  pragma(msg, isInstanceOf!(Nullable, res));
	  */
	  
	  static if(isInstanceOf!(Nullable, typeof(res))){
		if(!res.isNull){
		  writeln("res Nullable and not nullish: ", res);
		  auto ret = nullable(OneOf!(Ts).NodeType(res.get));
		  writeln("returning ", ret, " with type ", typeof(ret).stringof);
		  tokenStream = copy;
		  return ret;
		}
	  } else {
		if(res !is null){
		  writeln("res pointer like and not nullish: ", res);
		  tokenStream = copy;
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
  
  TokenStream copy = tokenStream; //don't consume any input
  return isNullish(parse!(TemplateArgsOf!N)(copy));

}

///returns the next token, whatever it is
auto parse(T: Token, TokenStream)(ref TokenStream tokenStream){
  mixin CTLog!("Parser for wildcard token");
  
  alias U = typeof(tokenStream.front());
  if(tokenStream.empty){ return Nullable!U(); }
  
  auto ret = tokenStream.front;
  tokenStream.popFront;
  return nullable(ret);
}

///parse a sequence of tokens in order
auto parse(S, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Sequence, S)){

  mixin CTLog!("Parser for Sequence `", S, "`");
  
  import std.typecons : Tuple;
  import std.meta : Filter, staticMap, ReplaceAll;
  
  alias Ts = S.Elements;
  alias TsWithValues = Filter!(hasValue, Ts);
  alias Values = ReplaceAll!(Token, typeof(tokenStream.front()), staticMap!(ValueType, TsWithValues));
  static if(Values.length > 1){
	alias RetType = Tuple!(Values);
  } else {
	alias RetType = Values[0];
  }
  Nullable!RetType ret;

  //  pragma(msg, "RetType: ");
  //  pragma(msg, RetType);
  
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
  writeln("parsing sequence: ", S.stringof, " from stream ", tokenStream);
  static foreach(i, elem; Ts){{
	  /*	pragma(msg, "parse ");
	pragma(msg, elem);
	pragma(msg, "corresponding to arg number");
	pragma(msg, argNumber!i);
	pragma(msg, "with type");
	static if(isInstanceOf!(Tuple, RetType)){
	  pragma(msg, typeof(ret[argNumber!i]));
	} else {
	  pragma(msg, typeof(ret));
	}

	  */
	static if(isType!elem){

	  static if(isInstanceOf!(Not, elem)){
		if(!parse!elem(copy)){
		  return Nullable!RetType();
		}
	  } else {
		auto x = parse!(elem)(copy);

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
	//	pragma(msg, "done with");
	//	pragma(msg, elem);
  }}

  tokenStream = copy;
  return ret;
}

struct Sequence(Ts...){
  alias Elements = Ts;
}

template hasValue(alias T){
  enum hasValue = isType!T && !isInstanceOf!(Not, T);
}

template ValueType(T){
  static if(isInstanceOf!(OneOf, T)){
	alias ValueType = T.NodeType;
  } else static if(isInstanceOf!(RegexStar, T) || isInstanceOf!(RegexPlus, T)){
	alias ValueType = ValueType!(TemplateArgsOf!T)[];
  } else {
	alias ValueType = T;
  }
}

///return true if the next token ins the stream is an "S"
bool check(alias S, TokenStream)(ref TokenStream tokenStream){
  import std.range;

  writeln("checking ", S.stringof, " on tokens ", tokenStream);

  mixin CTLog!("Parser(check) for literal `", S, "`");

  if(tokenStream.empty) return false;
  writeln("actual token ", tokenStream.front, " wanted ", (TokenType!S).stringof);
  
  return tokenStream.front.match!(
    (TokenType!S t) {
	  tokenStream.popFront();
	  writeln("check OK");
	  return true;
	},
	_ => false);
}
  
bool isNullish(T)(const auto ref T t){
  import std.traits : isPointer, isDynamicArray;
  //  pragma(msg, "\nnullish");
  //  pragma(msg, T);
  static if(isInstanceOf!(Nullable, T)){
	return t.isNull();
  }  else static if(isPointer!T){
	return t is null;
  } else static if(isDynamicArray!T){
	return t.length == 0;
  } else {
	return t;
  }
}



///Is a literal at the front?
bool parseLiteral(Lit, TokenStream)(ref TokenStream tokenStream){
  writeln("checking for literal ", Lit.stringof , " at front of stream: ", tokenStream);
  mixin CTLog!("Parse(non-check) literal `", Lit, "`");
  if(tokenStream.empty()){ return false; }
  if(tokenStream.front == Lit.value){
	tokenStream.popFront;
	writeln("got it");
	return true;
  }
  return false;
}

template CArgs(T){
  static assert(isType!T && isAggregateType!T);
  static if(hasMember!(T, "__ctor")){
	//	pragma(msg, T, " has a contructor");
	alias CArgs = Parameters!(__traits(getMember, T, "__ctor"));
	
  } else {
	//	pragma(msg, T, " doesn't have a constructor");
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
	//	pragma(msg, "Value Types", ValueTypes);
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
	//	pragma(msg, "Checking compatibility, sumtype: ", Src);
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

