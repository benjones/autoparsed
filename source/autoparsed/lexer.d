module autoparsed.lexer;


/**
   Produce a lexer for a module which acts as a forward range.
   The lexer will take a `const(char)[]` stream and its [front] method will return
   SumType objects of which can be any of the types annotated as [autoparsed.syntax.Token]s in the module.
   For tokens of struct type, they can be annotated with [autoparsed.syntax.Lex] using the normal autoparsed syntax rules
   See the [sexp example](sexpGrammar.html) for reference.

 **/
struct Lexer(alias Mod){

  import autoparsed.syntax;
  import autoparsed.autoparsed;
  import autoparsed.log;
  
  import std.range.primitives;

  import std.traits : fullyQualifiedName;

  import sumtype;
  import std.typecons : Nullable;

  mixin CTLog!("lexer for module: ", fullyQualifiedName!Mod);
  alias parseRule = OneOf!(tokenTypes!Mod);
  mixin CTLog!("lexer parse rule types: ", parseRule);  

  ///
  this(const(char)[] bytes){
	bytes_ = bytes;
	popFront();
  }
  ///
  bool empty(){
	return front_.isNull;
  }

  ///return the next token in the stream
  auto front(){
	import std.traits : TemplateArgsOf;

	return front_.get();
  }
  ///
  void popFront(){
	import autoparsed.recursivedescent;
	//should never be called when empty bc of rules of ranges
	front_ = parse!parseRule(bytes_);
  }
  
private:
  const(char)[] bytes_;
  Nullable!(parseRule.NodeType) front_;
  
}
