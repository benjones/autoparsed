module autoparsed.syntax;


///UDA type for annotating constructors as grammar rules
struct Syntax(T...){
  alias Elements = T;
}

///PEG rule for picking between options
struct OneOf(T...){
  import std.traits: fullyQualifiedName;
  static if(T.length < 2){
	pragma(msg, "Warning: using OneOf to pick between "
		   ~ T.length~ " options.  You should probably provide at least 2 options.");

	static if(T.length ==1){
	  pragma(msg, "type provided: " ~ fullyQualifiedName!T[0]);
	}
  }
}

///PEG rule for "at least one"
struct RegexPlus(T){

}

///PEG rule for "0 or more"
struct RegexStar(T){

}

///PEG rule for 0 or 1 of this
struct Optional(T){

}

