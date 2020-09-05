module autoparsed.autoparsed;

public import autoparsed.syntax;
import autoparsed.log;

template symbolsFromModule(alias Module){
  import std.meta : staticMap;
  alias getSymbol(alias T) = __traits(getMember, Module, T);
  alias symbolsFromModule = staticMap!(getSymbol, __traits(allMembers, Module));
}


template tokensFromModule(alias Module){
  import std.traits : getSymbolsByUDA;
  alias tokensFromModule = getSymbolsByUDA!(Module, Token);
}

template tokenTypes(alias Module){
  import std.meta : staticMap;
  import std.traits : isType;
  
  template wrap(alias T){
    static if(!isType!T){
      alias wrap = TokenType!T;
    } else {
      alias wrap = T;
    }
  }
  alias tokenTypes = staticMap!(wrap, tokensFromModule!Module);
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
  enum isSyntax = isInstanceOf!(Sequence, T) ||
    isInstanceOf!(OneOf, T) ||
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

  alias declarations = Filter!(isAggregateType,
                               Filter!(isType,
                                       Filter!(notModuleOrPackage, symbolsFromModule!Module)));
  
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

  enum formattedPieces = [staticMap!(FormatExpression, S.Elements)];

  enum FormatSyntax = join(formattedPieces, " ");
}

template FormatExpression(alias S) {
  import std.traits: isType, TemplateOf, TemplateArgsOf, fullyQualifiedName;
  import std.meta : staticMap;
  import std.array : join;

  static if(!isType!S){
    enum FormatExpression = "`" ~ S ~ "`";
  } else static if(is(typeof(TemplateOf!S))) {

    alias tArgs = TemplateArgsOf!S;
    alias recurse(alias T) = FormatExpression!T;
    
    enum fp = [staticMap!(recurse, tArgs)];
    enum joinedParts = join(fp, ", ");
    enum FormatExpression = __traits(identifier, S) ~ "(" ~ joinedParts ~ ")";
  } else {
    enum FormatExpression = __traits(identifier, S);
  }
}
