/// A grammer for a simple C-like language
module clike;

import autoparsed.autoparsed;
import autoparsed.lexer;
import autoparsed.syntax;
import autoparsed.log;

import std.typecons : Tuple;
import std.stdio;
import std.meta;
import std.algorithm;
import std.range;
import std.array;

import std.sumtype;


@Token{
  enum lcurly = '{';
  enum rcurly = '}';
  enum lparen = '(';
  enum rparen = ')';
  enum comma = ',';
  enum semi = ';';
  enum eq = '=';

  @Syntax!(RegexPlus!(OneOf!(' ', '\t', '\n', '\r')))
    struct Whitespace{
      const(char)[] val;
    }


  //syntax with sequence explicit
  @Syntax!('i', 'f')
    enum if_token;

  //syntax where the only element is a type of sequence
  @Syntax!("while")
    enum while_token;

  @Syntax!(RegexPlus!(OneOf!('-', InRange!('a','z'), InRange!('A', 'Z'))))
    struct Identifier{
      const(char)[] val;
      alias val this;
    }

}

@Syntax!(Identifier)
struct Expression{
  Identifier id;
}

@Syntax!(Identifier, eq, Expression, semi)
struct AssignmentStatement {
  this(Identifier id, Expression exp){
    writeln("assign statment with it: ", id, " and expression ", exp);
  }
}

@Syntax!(Expression, semi)
struct ExpressionStatement {
  Expression exp;
}

alias StatementSyntax = OneOf!(AssignmentStatement, ExpressionStatement, IfStatement, WhileStatement);
alias StatementValue = StatementSyntax.NodeType;

alias ParameterList = AliasSeq!(lparen, RegexStar!(Identifier, Identifier, comma), Optional!(Identifier, Identifier), rparen);

@Syntax!(Identifier, Identifier, ParameterList, lcurly, RegexStar!StatementSyntax, rcurly)
struct FunctionDeclaration{

  this(Identifier retType, Identifier name, Tuple!(Identifier, Identifier)[] parameters, StatementValue[] body_){
    writeln("making a function decl with return type ", retType, " named ", name, " with params: ", parameters, " and body: ", body_);
  }

}

@Syntax!(Identifier, Identifier, semi)
struct VariableDeclaration{
  Identifier type, name;
}

alias DeclarationSyntax = OneOf!(FunctionDeclaration, VariableDeclaration);
alias DeclarationValue = DeclarationSyntax.NodeType;

@Syntax!(RegexStar!DeclarationSyntax)
struct CompilationUnit
{
  DeclarationValue[] decls;

}

@Syntax!(if_token, lparen, Expression, rparen, lcurly, StatementSyntax, rcurly)
class IfStatement {
  this(Expression cond, StatementValue bod){
    writefln("making an if statment with condition: %s and body: %s", cond, bod);
    condition = cond;
    body = bod;
  }
  Expression condition;
  StatementValue body;
}

@Syntax!(while_token, lparen, Expression, rparen, lcurly, StatementSyntax, rcurly)
class WhileStatement {
  this(Expression cond, StatementValue bod){
    writefln("made a while statement with condition: %s and body: %s", cond, bod);
    condition = cond;
    body = bod;
  }
  Expression condition;
  StatementValue body;
}

unittest{
  import autoparsed.recursivedescent;

  string program = `type var;
ret func(argA nameA, argB nameB){}
ret gunc(argC nameC){ z = qwerty;}
ret hunc(argD nameD){if (nameD) { y = asdf;}}
`;

  auto lexer = Lexer!clike(program);
  auto tokens = lexer.filter!( x => x.match!(
                                 (Whitespace w) => false,
                                 _ => true)
                               ).array;
  writeln("\n\nTOKENS:\n", tokens, "\n\n");

  auto cu = parse!CompilationUnit(tokens);

  writeln("parsed cu is: ", cu);

}

unittest {
  import autoparsed.recursivedescent;
  string program = "ret func(){while(true){ x = y; }}";

  auto lexer = Lexer!clike(program);
  auto tokens = lexer.filter!( x => x.match!(
                                 (Whitespace w) => false,
                                 _ => true)
                               ).array;
  writeln("\n\nTOKENS:\n", tokens, "\n\n");

  auto cu = parse!CompilationUnit(tokens);

  writeln("parsed cu is: ", cu);

  assert(cu.getPayload.contents.decls.length == 1);

}
