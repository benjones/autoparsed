module autoparsed.autoparsed;

public import autoparsed.syntax;
pragma(msg, "autoparsed module");


template symbolsFromModule(alias Module){
  import std.meta : staticMap;
  alias getSymbol(alias T) = __traits(getMember, Module, T);
  alias symbolsFromModule = staticMap!(getSymbol, __traits(allMembers, Module));
}


template tokensFromModule(alias Module){

  import std.traits;// : hasUDA, fullyQualifiedName;
  import std.meta : Filter;
  pragma(msg, "insantiating tokensFromModule for " ~ fullyQualifiedName!Module);
  enum isToken(alias T) = hasUDA!(T, Token);
  alias syms = symbolsFromModule!Module;
  //pragma(msg, syms);
  static foreach(i, _; syms){
	pragma(msg, fullyQualifiedName!(syms[i]));
	pragma(msg, __traits(getAttributes, syms[i]));
  }
  pragma(msg, staticMap!(fullyQualifiedName, getSymbolsByUDA!(Module, Token)));
  alias tokensFromModule = getSymbolsByUDA!(Module, Token);//Filter!(isToken, syms);
}

template tokenTypes(alias Module){
  import std.meta : staticMap;
  alias tokenTypes = staticMap!(TokenType, tokensFromModule!Module);
}

template TokenPayload(alias Module){
  import sumtype;
  alias TokenPayload = SumType!(tokenTypes!Module);
}

template notModuleOrPackage(alias T){
  enum notModuleOrPackage = !(__traits(isModule, T) || __traits(isPackage, T));
}

template annotatedConstructors(alias T){
  import std.meta : staticMap, Filter, AliasSeq;
  import std.traits : getUDAs, fullyQualifiedName, isType, hasUDA, isInstanceOf;
  pragma(msg, "AC for ", T);
  enum isSyntax = isInstanceOf!(OneOf, T) ||
	isInstanceOf!(RegexPlus, T) ||
	isInstanceOf!(RegexStar, T) ||
	isInstanceOf!(Optional, T) ||
	isInstanceOf!(Not, T);
  static if(isSyntax){
	alias annotatedConstructors = AliasSeq!();
  } else {
	alias getSymbol(alias S) = __traits(getMember, T, S);
	enum hasSyntaxUDA(alias S) = hasUDA!(S, Syntax);
	
	alias members = staticMap!(getSymbol, __traits(allMembers, T));  
	alias annotatedConstructors = Filter!(hasSyntaxUDA, members);
  }
}


template SyntaxRulesFromModule(alias Module){
  import std.meta: Filter, templateNot, staticMap;
  import std.traits: isType, isAggregateType, fullyQualifiedName;

  alias declarations = Filter!(isAggregateType, Filter!(isType, Filter!(notModuleOrPackage, symbolsFromModule!Module)));
  alias SyntaxRulesFromModule = staticMap!(annotatedConstructors, declarations);
}

template RuleToPegString(alias Constructor){
  import std.traits: getUDAs;
  enum RuleToPegString = __traits(identifier, __traits(parent, Constructor)) ~ " <- " ~
	FormatSyntax!(getUDAs!(Constructor, Syntax)[0]);
}


template FormatSyntax(alias S){
  import std.traits : hasMember, fullyQualifiedName, TemplateArgsOf;
  import std.meta: staticMap;
  import std.array: join;
  //pragma(msg, "FS: " ~ fullyQualifiedName!S);
  enum formattedPieces = [staticMap!(FormatExpression, S.Elements)];
  //pragma(msg, formattedPieces);
  enum FormatSyntax = join(formattedPieces, " ");
}

template FormatExpression(alias S) {
  import std.traits: isType, TemplateOf, TemplateArgsOf, fullyQualifiedName;
  import std.meta : staticMap;
  import std.array : join;
  /*pragma(msg, "FE: ");
  pragma(msg, S);//fullyQualifiedName!S);
  static if(isType!S){
	pragma(msg, "Tempof: " ~ fullyQualifiedName!(TemplateOf!S));
	pragma(msg, "istypeof ...: " ~ is(typeof(TemplateOf!S)));
  }
  */
  static if(!isType!S){
	enum FormatExpression = "`" ~ S ~ "`";
  } else static if(is(typeof(TemplateOf!S))) {
	//	pragma(msg, "template: ", fullyQualifiedName!S );
	//	pragma(msg, staticMap!(fullyQualifiedName, TemplateArgsOf!S));
	alias tArgs = TemplateArgsOf!S;
	alias recurse(alias T) = FormatExpression!T;
	//	pragma(msg, "tArgs");
	//	pragma(msg, tArgs);
	
	enum fp = [staticMap!(recurse, tArgs)];
	//	pragma(msg, fp);
	enum joinedParts = join(fp, ", ");
	//pragma(msg, "formatted pieces ", fp);
	enum FormatExpression = __traits(identifier, S) ~ "(" ~ joinedParts ~ ")";
  } else {
	//	pragma(msg, "not a template");
	enum FormatExpression = __traits(identifier, S);
  }
}


template typeTypeCheck(alias Constructor){

}

pragma(msg, "end autoparsed module");
