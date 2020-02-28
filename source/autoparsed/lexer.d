module autoparsed.lexer;



struct Lexer(alias Mod){

  import autoparsed.syntax;
  import autoparsed.autoparsed;
  
  import std.range.primitives;

  import sumtype;
  import std.traits;
  import std.meta;
  import std.typecons : Nullable;

  pragma(msg, "lexer symbols from module");
  pragma(msg, "module is");
  //  pragma(msg, Mod);
  pragma(msg, fullyQualifiedName!Mod);
  pragma(msg, __traits(allMembers, Mod));

  pragma(msg, staticMap!(fullyQualifiedName, symbolsFromModule!Mod));
  pragma(msg, "tokens from module");
  pragma(msg, staticMap!(fullyQualifiedName, tokensFromModule!Mod));
  pragma(msg, "lexer token types: ");
  pragma(msg, tokenTypes!Mod);
  alias parseRule = OneOf!(tokenTypes!Mod);
  pragma(msg, "lexer parse rule");
  pragma(msg, parseRule);
  pragma(msg, "node type: ");
  pragma(msg, typeof(front_));
  
  this(const(char)[] bytes){
	bytes_ = bytes;
	popFront();
  }

  bool empty(){
	return front_.isNull;
  }

  auto front(){
	import std.traits : TemplateArgsOf;

	return front_.get();
  }

  void popFront(){
	import autoparsed.recursivedescent;
	//should never be called when empty bc of rules of ranges
	pragma(msg, "begin lexer popfront");
	front_ = parse!parseRule(bytes_);
	pragma(msg, "end lexer popfront");
  }
  
private:
  const(char)[] bytes_;
  Nullable!(parseRule.NodeType) front_;
  
}
