grammar GROK;

topLevelStatement : functionDefinition
                        | methodDefintion 
                        | structDefinition
                        | unionDefintion
                        | interfaceDefinition
                        | instance
                        | statement;

functionDefinition : 'func' '<' typeParameterList '>' identifier '(' parameterDefinitionList ')' ':' type '=' expression
                        | 'func' identifier '(' parameterDefinitionList ')' ':' type '=' expression;

methodDefintion : 'meth' '<' typeParameterList '>' '(' identifier ')' identifier '(' parameterDefinitionList ')' ':' type '=' expression
                     | 'meth' '(' identifier ')' identifier '(' parameterDefinitionList ')' ':' type '=' expression;

structDefinition : 'struct' identifier '<' typeParameterList '>' '{' fieldDeclarationList '}';

unionDefintion : 'union' type '{' unionTypeList '}';

interfaceDefinition : 'interface' type '{' extendsList stubList '}';

instance : 'instance' type 'of' type '{' functionOrMethodInstanceList '}';

statement : variableDeclaration
              | variableAssignment
              | ifExpression
              | whileExpression
              | matchExpression
              | functionCall
              | methodCall;

// Expressions.
expression : ifExpression
               | whileExpression
               | matchExpression
               | functionCall
               | methodCall
               | block
               | lambda
               | booleanExpression
               | arithmeticExpression
               | variable;

// Might want to consider leaving out if only and make that a if_statement.
ifExpression : 'if' booleanExpression block //if_statement only.
                  | 'if' booleanExpression block 'else' ifExpression
                  | 'if' booleanExpression block 'else' block;

// Might want to have a while_block instead of just block.
whileExpression : 'while' booleanExpression block;

matchExpression : 'match' '(' expression ')' '{' caseList '}';

functionCall : identifier '(' parameterCallList ')';

methodCall : expression '.' identifier '(' parameterCallList ')';

// Might want to consider differentiating between block_expression and
// block_statement since it would not make sense for an if_statement to have
// a block_expression body, or a if_expression to have an block_statement
// body.
block : '{' statementList '}' //block_statement
          | '{' statementList expression '}'; //block_expression

lambda : '(' parameterDefinitionTypeOptionalList ')' '=>' expression
           | parameterDefinitionTypeOptional '=>' expression;

booleanExpression : booleanExpression '||' booleanProduct
                       | booleanProduct;

booleanProduct : booleanProduct '&&' booleanTerm
                    | booleanTerm;

booleanTerm : '(' booleanExpression ')'
                 | comparison
                 | functionCall // Type-check needed.
                 | methodCall // Type-check needed.
                 | variable // Type-check needed.
                 | BOOLEAN_CONSTANT;

BOOLEAN_CONSTANT : 'true' | 'false';

arithmeticExpression : arithmeticExpression '+' arithmeticProduct
                          | arithmeticExpression '-' arithmeticProduct
                          | arithmeticProduct;

arithmeticProduct : arithmeticProduct '*' arithmeticTerm
                       | arithmeticProduct '/' arithmeticTerm
                       | arithmeticTerm;

arithmeticTerm : '(' arithmeticExpression ')'
| functionCall // Type-check needed.
| methodCall // Type-check needed.
| variable // Type-check needed.
| arithmeticConstant;

// Here integer_constant and float_constant are left intentionally
// undefined as they are easier to define using regular expressions.
ARITHMETIC_CONSTANT : INTEGER_CONSTANT | FLOAT_CONSTANT;

fragment INTEGER_CONSTANT : [0-9]+;

fragment FLOAT_CONSTANT : [0-9]+ '.' [0-9]* | [0-9]* '.' [0-9]+;

// Other stuff.
typeParameterList : typeParameterList ',' type
                        | type;

type : identifier
         | identifier '<' typeParameterList '>';

parameterDefinitionList : parameterDefinitionList ',' parameterDefinition
                              | parameterDefinition;

parameterDefinition : identifier ':' type;

parameterDefinitionTypeOptionalList : parameterDefinitionTypeOptionalList ',' parameterDefinitionTypeOptional
                                            | parameterDefinitionTypeOptional;

parameterDefinitionTypeOptional : identifier
                                       | identifier ':' type;

variableDeclaration : 'val' identifier '=' expression
                         | 'val' identifier ':' type '=' expression
                         | 'var' identifier '=' expression
                         | 'var' identifier ':' type '=' expression;

variable_assignment : identifier '=' expression;

variable : identifier;

caseList : caseList caseExpression;

caseExpression : 'case' identifier '(' parameterDefinitionList ')' '=>' expression
                    | 'case' parameterDefinition '=>' expression;
