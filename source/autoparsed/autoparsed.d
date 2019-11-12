module autoparsed.autoparsed;

public import autoparsed.syntax;

enum Token;

struct TokenType(alias T){
  import std.traits : isType;
  static if(isType!T){
	pragma(msg, "type");
	alias type = T;
  } else {
	pragma(msg, "not a type");
  }
}

void parse(alias Module)(){
  
}


template symbolsFromModule(alias Module){
  import std.meta : staticMap;
  alias getSymbol(alias T) = __traits(getMember, Module, T);
  alias symbolsFromModule = staticMap!(getSymbol, __traits(allMembers, Module));
}


template tokensFromModule(alias Module){

  import std.traits : hasUDA;
  import std.meta : Filter, templateNot;

  alias isToken(alias T) = hasUDA!(T, Token);
  alias tokensFromModule = Filter!(isToken, symbolsFromModule!Module);

}

template tokenTypes(alias Module){
  import std.meta : staticMap;
  

  alias tokenTypes = staticMap!(TokenType, tokensFromModule!Module);
  
}

template TokenPayload(alias Module){
  import std.variant;
  alias TokenPayload = Algebraic!(tokenTypes!Module);
}
