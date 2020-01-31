module autoparsed.recursivedescent;
import autoparsed.syntax;
import autoparsed.autoparsed;

import std.typecons : nullable, Nullable;
import std.traits: TemplateArgsOf, isInstanceOf, hasUDA;
import std.range.primitives;
import std.variant : Algebraic, VariantN;

Nullable!T parse(T, TokenStream)(ref TokenStream tokenStream)
if(hasUDA!(T, Token)){
  if(tokenStream.front.type == typeid(TokenType!T)){
	auto ret = tokenStream.front.get!(T).nullable;
	tokenStream.popFront();
	return ret;
  } else {
	return Nullable!T();
  }
  
}


T parse(T, TokenStream)(ref TokenStream tokenStream)
if(!isInstanceOf!(RegexStar, T) &&
   !isInstanceOf!(RegexPlus, T) &&
   !isInstanceOf!(OneOf, T) &&
   !hasUDA!(T, Token)){
  
  import std.traits : getUDAs, TemplateArgsOf, isType, Parameters, isInstanceOf;
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
  pragma(msg, "\n\nRegexStar parser");
  pragma(msg, RS);
  alias Elem = TemplateArgsOf!RS[0];
  auto elem = parse!Elem(tokenStream);
  alias RetType = RemoveNone!(typeof(elem));
  RetType[] ret;
  while(!isNullish(elem)){
	ret ~= elem.get!RetType;
	elem = parse!Elem(tokenStream);
  }
  return ret;
}

auto parse(RS, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(RegexPlus, RS)){
  pragma(msg, "\n\nRegexPlus parser");
  pragma(msg, RS);
  alias StarType = RegexStar!(TemplateArgsOf!RS);
  auto ret = parse!(StarType)(tokenStream);
  return ret.length > 0 ? ret : null;
}



auto parse(OO, TokenStream)(ref TokenStream tokenStream)
if(isInstanceOf!(OneOf, OO)){
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
		  return OneOf!(Ts).NodeType(res.get);
		}
	  } else {
		if(res !is null){
		  return OneOf!(Ts).NodeType(res);
		}
	  }
  }}
  return OneOf!(Ts).NodeType(None());
}

bool check(alias S, TokenStream)(ref TokenStream tokenStream){
  import std.range;
  pragma(msg, "\n\ncheck ");
  pragma(msg, TokenStream);
  if(tokenStream.front.type == typeid(TokenType!S.value)){
	tokenStream.popFront();
	return true;
  }
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
