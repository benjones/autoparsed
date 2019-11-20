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
	enum value = T;
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

template notModuleOrPackage(alias T){
  enum notModuleOrPackage = !(__traits(isModule, T) || __traits(isPackage, T));
}

template annotatedConstructors(alias T){
  import std.meta : staticMap, Filter;
  import std.traits : getUDAs, fullyQualifiedName, isType, hasUDA;

  alias getSymbol(alias S) = __traits(getMember, T, S);
  enum hasSyntaxUDA(alias S) = hasUDA!(S, Syntax);

  alias members = staticMap!(getSymbol, __traits(allMembers, T));  
  alias annotatedConstructors = Filter!(hasSyntaxUDA, members);

}


template SyntaxRulesFromModule(alias Module){
  import std.meta: Filter, templateNot, staticMap;
  import std.traits: isAggregateType, fullyQualifiedName;

  alias declarations = Filter!(isAggregateType, Filter!(notModuleOrPackage, symbolsFromModule!Module));
  alias SyntaxRulesFromModule = staticMap!(annotatedConstructors, declarations);
}

template RuleToPegString(alias Constructor){
  import std.traits: getUDAs;
  enum RuleToPegString = __traits(identifier, __traits(parent, Constructor)) ~ " <- " ~
	FormatSyntax!(getUDAs!(Constructor, Syntax)[0]);
}


template FormatSyntax(alias S){
  import std.meta: staticMap;
  import std.array: join;
  enum formattedPieces = [staticMap!(FormatExpression, S.Elements)];
  pragma(msg, formattedPieces);
  enum FormatSyntax = join(formattedPieces, " ");
}

template FormatExpression(alias S) {
  import std.traits: isType;
  static if(!isType!S){
	enum FormatExpression = "`" ~ S ~ "`";
  } else {
	enum FormatExpression = __traits(identifier, S);
  }
}
