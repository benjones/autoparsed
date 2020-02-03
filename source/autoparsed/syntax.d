module autoparsed.syntax;


///UDA type for annotating constructors as grammar rules
struct Syntax(T...){
  alias Elements = T;
}

///PEG rule for picking between options
struct None {}
struct OneOf(Ts...){
  import std.traits: fullyQualifiedName;
  
  static if(Ts.length < 2){
	pragma(msg, "Warning: using OneOf to pick between "
		   ~ Ts.length~ " options.  You should probably provide at least 2 options.");

	static if(Ts.length ==1){
	  pragma(msg, "type provided: " ~ fullyQualifiedName!Ts[0]);
	}
  }
  import sumtype;
  alias NodeType = SumType!(None, Ts);
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

