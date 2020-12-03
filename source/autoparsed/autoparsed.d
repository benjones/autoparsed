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
  alias tokenTypes = tokensFromModule!Module; //staticMap!(wrap, tokensFromModule!Module);
}

template TokenPayload(alias Module){
  import sumtype;
  alias TokenPayload = SumType!(tokenTypes!Module);
}

template notModuleOrPackage(alias T){
  enum notModuleOrPackage = !(__traits(isModule, T) || __traits(isPackage, T));
}



template SyntaxRulesFromModule(alias Module){
  import std.meta: Filter, templateNot, staticMap;
  import std.traits: isType, isAggregateType, fullyQualifiedName, hasUDA;

  alias declarations = Filter!(isAggregateType,
                               Filter!(isType,
                                       Filter!(notModuleOrPackage, symbolsFromModule!Module)));
  alias hasSyntaxUDA(X) = hasUDA!(X, Syntax);
  alias SyntaxRulesFromModule  = Filter!(hasSyntaxUDA, declarations);
}

template RuleToPegString(alias Type){
  import std.traits: getUDAs, fullyQualifiedName;
  enum RuleToPegString = fullyQualifiedName!Type ~ " <- " ~
    FormatSyntax!(getUDAs!(Type, Syntax)[0]);
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
