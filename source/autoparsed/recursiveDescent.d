module autoparsed.recursivedescent;
import autoparsed.syntax;
import autoparsed.autoparsed;

import std.stdio;

import std.typecons : nullable, Nullable;
import std.traits: ReturnType, Unqual, TemplateArgsOf, isInstanceOf, isType, hasUDA, getUDAs, allSameType, fullyQualifiedName;
import std.range.primitives;
import sumtype;

pragma(msg, "recursive descent module");

template contains(T, Ts...) {
  import std.meta : anySatisfy;
  pragma(msg, "contains");
  pragma(msg, T);
  pragma(msg, Ts);
  enum isSame(U) = allSameType!(TokenType!T, U);
  alias contains = anySatisfy!(isSame, Ts);
}

template partOfStream(T, TArgs){
  import std.range;
  pragma(msg, "part of stream");
  pragma(msg, T);
  pragma(msg, TArgs);
  enum partOfStream = contains!(T, TemplateArgsOf!TArgs);
}

Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Lex) &&
   (!isInstanceOf!(SumType, typeof(tokenStream.front())) ||
	!partOfStream!(T, typeof(tokenStream.front())))){
  
  pragma(msg, "making parser for lexable token type");
  pragma(msg, T);
  alias uda = getUDAs!(T, Lex);
  pragma(msg, "lexable with UDA:");
  pragma(msg, uda);
  pragma(msg, "template args");
  pragma(msg, TemplateArgsOf!uda);

  auto parsed = parse!(TemplateArgsOf!uda)(tokenStream);
  pragma(msg, "type of parsed for lexable");
  pragma(msg, typeof(parsed));

  static if(isInstanceOf!(Nullable, typeof(parsed))){
	if(parsed.isNull){
	  return Nullable!T();
	} else {
	  return Nullable!T(T(parsed.get));
	}
  } else {
	if(parsed){
	  return Nullable!T(T(parsed));
	} else {
	  return Nullable!T();
	}
  }
}

Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Token) &&
   isInstanceOf!(SumType, typeof(tokenStream.front())) &&
   partOfStream!(T, typeof(tokenStream.front()))){
  
  pragma(msg, "making parser for Non-lexable Token ");
  pragma(msg, T);

  writeln("parsing token ", T.stringof, " from stream ", tokenStream);
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

Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(TokenType, T)){
  import std.traits: isType;
  pragma(msg, "making parser for token type ");
  pragma(msg, T);
  static if(isType!(TemplateArgsOf!T)){
	auto res = parse!(TemplateArgsOf!T)(tokenStream);
	return res.isNull ? Nullable!T() : Nullable!T(T(res.get));
  } else {
	return parseLiteral!T(tokenStream) ? Nullable!T(T()) : Nullable!T();
  }

}

T parse(T, TokenStream)(ref TokenStream tokenStream)
if(annotatedConstructors!(T).length > 0) {
  import std.traits : getUDAs, isType, Parameters;
  import std.meta : staticMap;
  
  writeln("parsing ", T.stringof, " token stream: ", tokenStream);

  pragma(msg, "\n\n\nmaking parser for ");
  pragma(msg, T);

  alias ac = annotatedConstructors!T;
  pragma(msg, "with ac");
  pragma(msg, ac);
  alias syntax = getUDAs!(ac, Syntax)[0];

  pragma(msg, "with syntax ");
  pragma(msg, syntax);
  static foreach(i, x; TemplateArgsOf!syntax){
	pragma(msg, fullyQualifiedName!(TemplateArgsOf!syntax[i]));
  }


  alias Args = Parameters!(annotatedConstructors!(T)[0]);
  pragma(msg, "constructor args: ");
  pragma(msg, Args);
  alias Seq = Sequence!(TemplateArgsOf!syntax);
  pragma(msg, "seq:");
  pragma(msg, Seq);
  auto parsed =  parse!Seq(tokenStream);
  if(parsed.isNull){
	return null;
  } else {
	return new T(parsed.get);
  }

}

auto parse(RS, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexStar, RS)){
  writeln("parsing regex star ", RS.stringof, " token stream: ", tokenStream);
  pragma(msg, "\n\nRegexStar parser");
  pragma(msg, RS);
  alias Elems = TemplateArgsOf!RS;
  static if(Elems.length > 1){
	alias ElemType = Sequence!(Elems);
  } else {
	alias ElemType = Elems[0];
  }
  auto elem = parse!ElemType(tokenStream);
  static if(isInstanceOf!(OneOf, ElemType)){
	alias RetType = RemoveNone!(typeof(elem));
  } else static if(isInstanceOf!(Nullable, typeof(elem))){
	pragma(msg, "nullable elem type: ");
	pragma(msg, ElemType);
	alias RetType = TemplateArgsOf!(typeof(elem))[0];
  } else {
	alias RetType = typeof(elem);
	
  }
  
  RetType[] ret;
  pragma(msg, "RS parser, ret type:");
  pragma(msg, RetType);
  pragma(msg, "typeof elem");
  pragma(msg, typeof(elem));
  pragma(msg, "typeof ret");
  pragma(msg, typeof(ret));
  pragma(msg, "typeof RetType[]");
  pragma(msg, RetType[]);
  while(!isNullish(elem)){
	writeln("elem not nullish: ", elem);
	writeln("trying to append a ", typeof(elem).stringof, " to a ", typeof(ret).stringof);
	static if(isInstanceOf!(OneOf, ElemType)){
	  ret ~= transformVariant!RetType(elem);
	} else {
	  ret ~= elem;
	}
	writeln("appended elem to ret in regexstar: ", ret);
	elem = parse!ElemType(tokenStream);
  }
  return ret;
}

auto parse(RP, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexPlus, RP)){

  writeln("parsing regex plus ", RP.stringof, " token stream: ", tokenStream);
  pragma(msg, "\n\nRegexPlus parser");
  pragma(msg, RP);
  alias StarType = RegexStar!(TemplateArgsOf!RP);
  auto ret = parse!(StarType)(tokenStream);
  pragma(msg, "RP got this type back from RS: ");
  pragma(msg, typeof(ret));
  return ret.length > 0 ? ret : null;
}



auto parse(OO, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(OneOf, OO)){

  writeln("parsing one of ", OO.stringof, " token stream: ", tokenStream);
  
  pragma(msg, "\n\nOneOf parser");
  pragma(msg, OO);
  alias Ts = TemplateArgsOf!OO;
  static foreach(T; Ts){{
	  auto res = parse!T(tokenStream);
	  pragma(msg, "\nrestype");
	  pragma(msg, typeof(res));
	  pragma(msg, isInstanceOf!(Nullable, res));
	  static if(isInstanceOf!(Nullable, typeof(res))){
		if(!res.isNull){
		  writeln("res Nullable and not nullish: ", res);
		  auto ret = OneOf!(Ts).NodeType(res.get);
		  writeln("returning ", ret, " with type ", typeof(ret).stringof);
		  return ret;
		}
	  } else {
		if(res !is null){
		  writeln("res pointer like and not nullish: ", res);
		  return OneOf!(Ts).NodeType(res);
		}
	  }
  }}
  return OneOf!(Ts).NodeType(None());
}

bool parse(N, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Not, N)){
  
  return isNullish(parse!(TemplateArgsOf!N)(tokenStream));

}

auto parse(T: Token, TokenStream)(ref TokenStream tokenStream){
  auto ret = tokenStream.front;
  tokenStream.popFront;
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
	alias ValueType = RemoveNone!(ValueType!(TemplateArgsOf!T))[];
  } else {
	alias ValueType = T;
  }
}

auto parse(S, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(Sequence, S)){

  pragma(msg, "sequence parser for");
  pragma(msg, S);
  
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

  pragma(msg, "RetType: ");
  pragma(msg, RetType);
  
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
  
  static foreach(i, elem; Ts){
	pragma(msg, "parse ");
	pragma(msg, elem);
	pragma(msg, "corresponding to arg number");
	pragma(msg, argNumber!i);
	pragma(msg, "with type");
	static if(isInstanceOf!(Tuple, RetType)){
	  pragma(msg, typeof(ret[argNumber!i]));
	} else {
	  pragma(msg, typeof(ret));
	}

	
	static if(isType!elem){
	  static if(hasUDA!(elem, Token)){
		auto tok = parse!elem(tokenStream);
		if(tok.isNull){
		  return Nullable!RetType();
		}
		set!(argNumber!i)( tok.get);
	  }
	  static if(isInstanceOf!(OneOf, elem)){
		auto oo = parse!(elem)(tokensStream);
		if(!oo){
		  return Nullable!RetType();
		}
		set!(argNumber!i)(oo);
	  } else static if(isInstanceOf!(RegexPlus, elem)){
		auto rp = parse!(elem)(tokenStream);
		if(!rp){
		  return Nullable!RetType();
		}
		set!(argNumber!i)(rp);
	  } else static if(isInstanceOf!(Not, elem)){
		if(!parse!elem(tokenStream)){
		  return Nullable!RetType();
		}
	  } else static if(is(elem == Token)){
		if(tokenStream.empty) return Nullable!RetType();

		set!(argNumber!i)(tokenStream.front);
		tokenStream.popFront;
		
	  } else {
		pragma(msg, elem);
		static assert(false, "uh oh");
	  }
	} else {
	  if(!check!elem(tokenStream)){
		return Nullable!RetType();
	  }
	}
	pragma(msg, "done with");
	pragma(msg, elem);
  }

  
  return ret;
}

bool check(alias S, TokenStream)(ref TokenStream tokenStream){
  import std.range;

  writeln("checking ", S.stringof, " on tokens ", tokenStream);
  pragma(msg, "\n\ncheck ");
  pragma(msg, TokenStream);
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
  pragma(msg, "\nnullish");
  pragma(msg, T);
  static if(isInstanceOf!(Nullable, T)){
	return t.isNull();
  } else static if(isSumType!T){
	return t.match!( (None n) => true, _ => false);
  } else static if(isPointer!T){
	return t is null;
  } else {
	return t;
  }
}


template RemoveNone(T){

  import std.meta : Filter;
  import std.traits : allSameType;
  static if(isSumType!T){
	alias args = TemplateArgsOf!T;
	enum NotNone(S) = !allSameType!(S, None);
	alias RemoveNone = SumType!(Filter!(NotNone, args));
  } else {
	alias RemoveNone = T;
  }
}

auto transformVariant(U, T)(ref T t)
if(isSumType!T && isSumType!U)
{
  pragma(msg, "converting from ");
  pragma(msg, T);
  pragma(msg, " to");
  pragma(msg, U);
  writeln("converting from ", T.stringof, " to ", U.stringof);
  return t.match!( x => U(x),
    function U(None){ assert(false);}
  );
}

bool parseLiteral(Lit, TokenStream)(ref TokenStream tokenStream){
  return false;
}


pragma(msg, "end recursive descent module");
