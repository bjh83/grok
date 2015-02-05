grammar Grok;

@header {
  package com.grok;
}

@parser::members {
  // We need to be able to check whether the next token parsed is a
  // LineTerminator so that we can determine the end of statements.
  private boolean lineTerminatorAhead() {
    int possibleEosTokenIndex = getCurrentToken().getTokenIndex() - 1;
    Token possibleEosToken = _input.get(possibleEosTokenIndex);
    int type = possibleEosToken.getType();
    return type == LineTerminator;
  }
}

compilationUnit
  : topLevelStatement* EOF
  ;

topLevelStatement 
  : (functionDefinition
    | methodDefintion 
    | structDefinition
    | unionDefintion
    | interfaceDefinition
    | instance
    | statement
    ) eos
  ;

functionDefinition
  : 'func' typeParameters? Identifier funcParameters ':' type '=' expression
  ;

methodDefintion
  : 'meth' typeParameters? '(' type ')' Identifier funcParameters ':' type '=' expression
  ;

structDefinition
  : 'struct' typeParameters? Identifier '{' field* '}'
  ;

unionDefintion
  : 'union' typeParameters? Identifier '{' type* '}'
  ;

interfaceDefinition
  : 'interface' typeParameters? Identifier ('extends' type)? '{' methodStub* '}'
  ;

instance
  : 'instance' typeParameters? type 'of' type '{' instanceMethod* '}'
  ;

typeParameters
  : '<' type+ '>'
  ;

type
  : Identifier typeParameters?
  ;

funcParameters
  : '(' ((funcParameter ',')* funcParameter)? ')'
  ;

funcParameter
  : Identifier ':' type
  ;

field
  : ('var' | 'val') Identifier ':' type
  ;

methodStub
  : 'meth' typeParameters? Identifier funcParameters ':' type
  ;

instanceMethod
  : 'meth' typeParameters? Identifier funcParameters ':' type '=' expression
  ;

innerStatement
  : variableDeclaration
  | variableAssignment
  | structAssignment
  | ifExpression
  | whileExpression
  | matchExpression
  | functionCall
  | methodCall
  ;

statement
  : innerStatement eos
  ;

variableDeclaration
  : ('var' | 'val') Identifier (':' type) '=' expression
  ;

variableAssignment
  : Identifier '=' expression
  ;

structAssignment
  : Identifier '.' Identifier '=' expression
  ;

expression
  : ifExpression
  | whileExpression
  | matchExpression
  | functionCall
  | methodCall
  | block
  | lambda
  | booleanExpression
  | arithmeticExpression
  | variable
  | accessor
  | 'this'
  ;

ifExpression
  : 'if' booleanExpression block ('else' (block | ifExpression))?
  ;

whileExpression
  : 'while' booleanExpression block
  ;

matchExpression
  : 'match' expression '{' case+ '}'
  ;

case
  : 'case' type funcParameters? '=>' expression
  ;

functionCall
  : Identifier arguments
  ;

methodCall
  : expression '.' Identifier arguments
  ;

block
  : '{' statement* expression? '}'
  ;

lambda
  : lambdaParameters '=>' expression
  ;

lambdaParameters
  : lambdaParameter
  | '(' ((lambdaParameter ',')* lambdaParameter)? ')'
  ;

lambdaParameter
  : Identifier (':' type)?
  ;

booleanExpression
  : booleanProduct '||' booleanExpression
  | booleanProduct
  ;

booleanProduct
  : booleanInverse '&&' booleanProduct
  | booleanInverse
  ;

booleanInverse
  : '!'? booleanTerm
  ;

booleanTerm
  : '(' booleanExpression ')'
  | comparison
  | functionCall
  | methodCall
  | variable
  | BooleanConstant
  ;

comparison
  : arithmeticExpression 
    ( '==' 
    | '!=' 
    | '<=' 
    | '>=' 
    | '<' 
    | '>'
  ) arithmeticExpression
  ;

BooleanConstant
  : 'true'
  | 'false'
  ;

arithmeticExpression
  : arithmeticProduct ('+' | '-') arithmeticExpression
  | arithmeticProduct
  ;

arithmeticProduct
  : arithmeticTerm ('*' | '/' | '%') arithmeticProduct
  | arithmeticTerm
  ;

arithmeticTerm
  : '(' arithmeticExpression ')'
  | functionCall
  | methodCall
  | variable
  | ArithmeticConstant
  ;

ArithmeticConstant
  : [0-9]+
  | [0-9]+ '.' [0-9]*
  | [0-9]* '.' [0-9]+
  ;

variable
  : Identifier
  ;

accessor
  : expression '.' Identifier
  ;

eos
  : ';'
  | { lineTerminatorAhead() }?
  | EOF
  ;

arguments
  : '(' ((argument ',')* argument)? ')'
  ;

argument
  : Identifier
  ;

Identifier
  : [a-zA-Z0-9_]+
  ;

LineTerminator
  : [\r\n]+ -> channel(HIDDEN)
  ;

WS : [ \t]+ -> skip;
