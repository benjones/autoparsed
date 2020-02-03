module autoparsed.recursivedescent;
import autoparsed.syntax;
import autoparsed.autoparsed;

import std.stdio;

import std.typecons : nullable, Nullable;
import std.traits: TemplateArgsOf, isInstanceOf, hasUDA;
import std.range.primitives;
import std.variant : Algebraic, VariantN;

Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Token)){
  writeln("parsing token ", T.stringof, " from stream ", tokenStream);
  if(tokenStream.front.type == typeid(TokenType!T)){
	writeln("checks out");
	auto ret = tokenStream.front.get!(TokenType!T).value.nullable;
	tokenStream.popFront();
	writeln("we're good, returning ", typeof(ret).stringof);
	return ret;
  } else {
	writeln("wrong token type");
	return Nullable!T();
  }
  
}


T parse(T, TokenStream)(ref TokenStream tokenStream)
if(annotatedConstructors!(T).length > 0) {
  import std.traits : getUDAs, isType, Parameters;
  
  writeln("parsing ", T.stringof, " token stream: ", tokenStream);

  pragma(msg, "\n\n\nmaking parser for ");
  pragma(msg, T);

  alias ac = annotatedConstructors!T;
  pragma(msg, "with ac");
  pragma(msg, ac);
  alias syntax = getUDAs!(ac, Syntax)[0];

  pragma(msg, "with syntax ");
  pragma(msg, syntax);

  static size_t argNumber(size_t syntaxNumber)(){
	size_t ret = 0;
	pragma(msg, "\ncomputing arg number of");
	pragma(msg, syntaxNumber);
	static foreach(i; 0..syntaxNumber){
	  pragma(msg, "looking at");
	  pragma(msg, TemplateArgsOf!syntax[i]);
	  static if(isType!(TemplateArgsOf!syntax[i])){
		pragma(msg, "does count");
		++ret;
	  }
	}
	return ret;
  }
  
  alias Args = Parameters!(annotatedConstructors!(T)[0]);
  pragma(msg, "constructor args: ");
  pragma(msg, Args);
  Args args; 
  static foreach(i, elem; TemplateArgsOf!syntax){
	pragma(msg, "parse ");
	pragma(msg, elem);
	pragma(msg, "corresponding to arg number");
	pragma(msg, i);
	pragma(msg, "argnumber: ");
	pragma(msg, argNumber!i);

	static if(isType!elem){
	  pragma(msg, "type");
	  static if(hasUDA!(elem, Token)){
		auto tok = parse!elem(tokenStream);
		if(tok.isNull){
		  return null;
		}
		args[argNumber!i] = tok.get;
	  }
	  static if(isInstanceOf!(OneOf, elem)){
		auto oo = parse!(elem)(tokensStream);
		if(!oo){
		  return null;
		}
		args[argNumber!i] = oo;
	  } else static if(isInstanceOf!(RegexPlus, elem)){
		auto rp = parse!(elem)(tokenStream);
		if(!rp){
		  return null;
		}
		pragma(msg, "one of case");
		pragma(msg, typeof(args[argNumber!i]));
		pragma(msg, "type of rp");
		pragma(msg, typeof(rp));
		args[argNumber!i] = rp;
	  } else {
		pragma(msg, elem);
		static assert(false, "uh oh");
	  }
	} else {
	  pragma(msg, "not a type");
	  if(!check!elem(tokenStream)){
		return null;
	  }
	}
  }
				 
  return new T(args);
}

auto parse(RS, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexStar, RS)){
  writeln("parsing regex star ", RS.stringof, " token stream: ", tokenStream);
  pragma(msg, "\n\nRegexStar parser");
  pragma(msg, RS);
  alias Elem = TemplateArgsOf!RS[0];
  auto elem = parse!Elem(tokenStream);
  alias RetType = RemoveNone!(typeof(elem));
  RetType[] ret;
  while(!isNullish(elem)){
	writeln("elem not nullish: ", elem);
	writeln("trying to append a ", typeof(elem).stringof, " to a ", typeof(ret).stringof);
	ret ~= transformVariant!RetType(elem);
	writeln("appended elem to ret in regexstar: ", ret);
	elem = parse!Elem(tokenStream);
  }
  return ret;
}

auto parse(RS, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexPlus, RS)){

  writeln("parsing regex plus ", RS.stringof, " token stream: ", tokenStream);
  pragma(msg, "\n\nRegexPlus parser");
  pragma(msg, RS);
  alias StarType = RegexStar!(TemplateArgsOf!RS);
  auto ret = parse!(StarType)(tokenStream);
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

bool check(alias S, TokenStream)(ref TokenStream tokenStream){
  import std.range;

  writeln("checking ", S.stringof, " on tokens ", tokenStream);
  pragma(msg, "\n\ncheck ");
  pragma(msg, TokenStream);
  writeln("actual type ", tokenStream.front.type, " wanted ", (TokenType!S).stringof);
  if(tokenStream.front.type == typeid(TokenType!S)){
	tokenStream.popFront();
	writeln("check ok");
	return true;
  }
  writeln("check bad");
  return false;
}
  
bool isNullish(T)(const ref T t){
  pragma(msg, "\nnullish");
  pragma(msg, T);
  static if(isInstanceOf!(Nullable, T)){
	return t.isNull();
  } else static if(isInstanceOf!(VariantN, T)){
	return t.type == typeid(None);
  } else {
	return t is null;
  }
}


template RemoveNone(T) if(isInstanceOf!(VariantN, T)){
  import std.meta : Filter;
  import std.traits : allSameType;
  alias args = TemplateArgsOf!T;
  enum NotNone(S) = !allSameType!(S, None);
  alias RemoveNone = VariantN!(args[0], Filter!(NotNone, args[1..$]));
}

auto transformVariant(U, T)(ref T t)
if(isInstanceOf!(VariantN, T) && isInstanceOf!(VariantN, U))
{
  pragma(msg, "converting from ");
  pragma(msg, T);
  pragma(msg, " to");
  pragma(msg, U);
  writeln("converting from ", T.stringof, " to ", U.stringof);
  static foreach(Type; TemplateArgsOf!U[1..$]){
	if(t.type == typeid(Type)){
	  writeln("type match: ", Type.stringof);
	  return U(t.get!Type);
	}
  }
  assert(false);
}
