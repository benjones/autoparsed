module autoparsed.lexer;



struct Lexer(alias Mod){

  import autoparsed.syntax;
  import autoparsed.autoparsed;
  
  import std.range.primitives;

  import std.traits : fullyQualifiedName;

  import sumtype;
  import std.typecons : Nullable;

  pragma(msg, "lexer for module: ", fullyQualifiedName!Mod);
  pragma(msg, "lexer token types: ");
  pragma(msg, tokenTypes!Mod);
  alias parseRule = OneOf!(tokenTypes!Mod);
  pragma(msg, "lexer parse rule");
  pragma(msg, parseRule);
  
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
	front_ = parse!parseRule(bytes_);
  }
  
private:
  const(char)[] bytes_;
  Nullable!(parseRule.NodeType) front_;
  
}
