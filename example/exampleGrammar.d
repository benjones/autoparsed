
import autoparsed.autoparsed;
public import tokens;

class Number {
public:
  @Syntax!(IntLiteral)
  this(IntLiteral il){
	
  }
  
}


class Addition {
public:
  @Syntax!(IntLiteral, plus, IntLiteral)
  this(IntLiteral a, IntLiteral b){

	
  }

  
}
