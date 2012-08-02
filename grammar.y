class Parser

# Declare tokens produced by the lexer
token IF ELSE
token DEF
token VARIABLE
token VARIABLES
token SELECT
token CLASS
token PARENT
token NEWLINE
token NUMBER
token STRING
token TRUE FALSE NIL EMPTY
token IDENTIFIER
token CONSTANT
token INDENT DEDENT

# Precedence table
# Based on http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence
prechigh
  left  '.'
  right '!'
  left  '*' '/'
  left  '+' '-'
  left  '>' '>=' '<' '<='
  left  '==' '!='
  left  '&&'
  left  '||'
  right '='
  left  ','
preclow

rule
  # All rules are declared in this format:
  #
  #   RuleName:
  #     OtherRule TOKEN AnotherRule    { code to run when this matches }
  #   | OtherRule                      { ... }
  #   ;
  #
  # In the code section (inside the {...} on the right):
  # - Assign to "result" the value returned by the rule.
  # - Use val[index of expression] to reference expressions on the left.
  
  
  # All parsing will end in this rule, being the trunk of the AST.
  Root:
    /* nothing */                      { result = Nodes.new([]) }
  | Expressions                        { result = val[0] }
  ;
  
  # Any list of expressions, class or method body, seperated by line breaks.
  Expressions:
    Expression                         { result = Nodes.new(val) }
  | Expressions Terminator Expression  { result = val[0] << val[2] }
    # To ignore trailing line breaks
  | Expressions Terminator             { result = val[0] }
  | Terminator                         { result = Nodes.new([]) }
  ;

  # All types of expressions in our language
  Expression:
    Literal
  | Call
  | Operator
  | Constant
  | Assign
  | Def
  | Class
  | If
  | '(' Expression ')'    { result = val[1] }
  ;
  
  # All tokens that can terminate an expression
  Terminator:
    NEWLINE
  | ";"
  ;
  
  # All hard-coded values after empty code is taken from jscore
  Literal:
    NUMBER                        { result = NumberNode.new(val[0]) }
  | STRING                        { result = StringNode.new(val[0]) }
  | TRUE                          { result = TrueNode.new }
  | FALSE                         { result = FalseNode.new }
  | NIL                           { result = NilNode.new }
  | EMPTY                         { result = EmptyNode.new}
  | NULLTOKEN
  | TRUETOKEN
  | '/'
  | DIVEQUAL
  ;
  

  Property:
    IDENT ':' AssignmentExpr
  | STRING ':' AssignmentExpr
  | NUMBER ':' AssignmentExpr
  | IDENT IDENT '(' ')' OPENBRACE FunctionBody CLOSEBRACE
  | IDENT IDENT '(' FormalParameterList ')' OPENBRACE FunctionBody CLOSEBRACE
  | IDENTIFIER ':' AssignmentExpr
  | IDENTIFIER IDENTIFIER '(' ')' INDENT FunctionBody DEDENT 
  | IDENTIFIER IDENTIFIER '(' FormalParameterList ')' INDENT FunctionBody DEDENT 
  ;
  
  PropertyList
    Property
  | PropertyList ',' Property
  ;
  
  PrimaryExpr
    PrimaryExprNoBrace
  | OPENBRACE CLOSEBRACE
  | OPENBRACE PropertyList CLOSEBRACE
  | OPENBRACE PropertyList ',' CLOSEBRACE
  | INDENT DEDENT
  | INDENT PropertyList DEDENT
  | INDENT PropertyList ',' DEDENT
  | 
  ;

  # Primary Expression with no indents
  PrimaryExprNoBrace
    THISTOKEN
  | Literal
  | ArrayLiteral
  | IDENT
  | '(' Expr ')'
  | IDENTIFIER
  | '(' Expression ')'
  ; 
  
  # Arrays 
  ArrayLiteral
    '[' ElisionOpt ']'
  | '[' ElementList ']'
  | '[' ElementList ',' ElisionOpt ']'
  ;

  ElementList
    ElisionOpt AssignmentExpr
  | ElementList ',' ElisionOpt AssignmentExpr
  ;

  ElisionOpt
  
  | Elision
  ;

  Elision
    ','
  | Elision ','
  ;

  MemberExpr
    PrimaryExpr
  | FunctionExpr
  | MemberExpr '[' Expr ']'
  | MemberExpr '.' IDENT
  | NEW MemberExpr Arguments
  | MemberExpr '[' Expression ']'
  | MemberExpr '.' IDENTIFIER
  ;

  # What is no BF

  MemberExprNoBF
    PrimaryExprNoBrace
  | MemberExprNoBF '[' Expr ']'
  | MemberExprNoBF '.' IDENT
  | NEW MemberExpr Arguments
  | MemberExprNoBF '[' Expression ']'
  | MemberExprNoBF '.' IDENTIFER
  ;

  NewExpr
    MemberExpr
  | NEW NewExpr
  ;

  NewExprNoBF
    MemberExprNoBF
  | NEW NewExpr
    ;

  CallExpr
    MemberExpr Arguments
  | CallExpr Arguments
  | CallExpr '[' Expr ']'
  | CallExpr '.' IDENT
  | CallExpr '[' Expression ']'
  | CallExpr '.' IDENTIFIER
  ;

  CallExprNoBF
    MemberExprNoBF Arguments
  | CallExprNoBF Arguments
  | CallExprNoBF '[' Expr ']'
  | CallExprNoBF '.' IDENT
  | CallExprNoBF '[' IDENTIFIER ']'
  ;

  Arguments
    '(' ')'
  | '(' ArgumentList ')'
  ;

  ArgumentList
    AssignmentExpr
  | ArgumentList ',' AssignmentExpr
  ;

  LeftHandSideExpr
    NewExpr
  | CallExpr
  ;

  LeftHandSideExprNoBF
    NewExprNoBF
  | CallExprNoBF
  ;

  PostfixExpr
    LeftHandSideExpr
  | LeftHandSideExpr PLUSPLUS
  | LeftHandSideExpr MINUSMINUS
  | LeftHandSideExpr '++'
  | LeftHandSideExpr '--'
  ;

  PostfixExprNoBF
    LeftHandSideExprNoBF
  | LeftHandSideExprNoBF PLUSPLUS
  | LeftHandSideExprNoBF MINUSMINUS
  | LeftHandSideExprNoBF '++'
  | LeftHandSideExprNoBF '--'
  ;

  UnaryExprCommon
    DELETETOKEN UnaryExpr
  | VOIDTOKEN UnaryExpr
  | TYPEOF UnaryExpr
  | PLUSPLUS UnaryExpr
  | AUTOPLUSPLUS UnaryExpr
  | MINUSMINUS UnaryExpr
  | AUTOMINUSMINUS UnaryExpr
  | '+' UnaryExpr
  | '-' UnaryExpr
  | '~' UnaryExpr
  | '!' UnaryExpr
  | '++' Unary Expr
  | '--' Unary Expr
  ;

  UnaryExpr
    PostfixExpr
  | UnaryExprCommon
  ;

  UnaryExprNoBF
    PostfixExprNoBF
  | UnaryExprCommon
  ;

  MultiplicativeExpr
    UnaryExpr
  | MultiplicativeExpr '*' UnaryExpr
  | MultiplicativeExpr '/' UnaryExpr
  | MultiplicativeExpr '%' UnaryExpr
  ;

  MultiplicativeExprNoBF
    UnaryExprNoBF
  | MultiplicativeExprNoBF '*' UnaryExpr
  | MultiplicativeExprNoBF '/' UnaryExpr
  | MultiplicativeExprNoBF '%' UnaryExpr
  ;

  AdditiveExpr
    MultiplicativeExpr
  | AdditiveExpr '+' MultiplicativeExpr
  | AdditiveExpr '-' MultiplicativeExpr
  ;

  AdditiveExprNoBF
    MultiplicativeExprNoBF
  | AdditiveExprNoBF '+' MultiplicativeExpr
  | AdditiveExprNoBF '-' MultiplicativeExpr
  ;
  
  ShiftExpr
    AdditiveExpr
  | ShiftExpr LSHIFT AdditiveExpr
  | ShiftExpr RSHIFT AdditiveExpr
  | ShiftExpr URSHIFT AdditiveExpr
  ;

  ShiftExprNoBF
    AdditiveExprNoBF
  | ShiftExprNoBF LSHIFT AdditiveExpr
  | ShiftExprNoBF RSHIFT AdditiveExpr
  | ShiftExprNoBF URSHIFT AdditiveExpr
  ;

  RelationalExpr
    ShiftExpr
  | RelationalExpr '<' ShiftExpr
  | RelationalExpr '>' ShiftExpr
  | RelationalExpr LE ShiftExpr
  | RelationalExpr GE ShiftExpr
  | RelationalExpr INSTANCEOF ShiftExpr
  | RelationalExpr INTOKEN ShiftExpr
  ;
  
  RelationalExprNoIn
    ShiftExpr
  | RelationalExprNoIn '<' ShiftExpr
  | RelationalExprNoIn '>' ShiftExpr
  | RelationalExprNoIn LE ShiftExpr
  | RelationalExprNoIn GE ShiftExpr
  | RelationalExprNoIn INSTANCEOF ShiftExpr
  ;

  RelationalExprNoBF
    ShiftExprNoBF
  | RelationalExprNoBF '<' ShiftExpr
  | RelationalExprNoBF '>' ShiftExpr
  | RelationalExprNoBF LE ShiftExpr
  | RelationalExprNoBF GE ShiftExpr
  | RelationalExprNoBF INSTANCEOF ShiftExpr
  | RelationalExprNoBF INTOKEN ShiftExpr
  ;

  EqualityExpr
    RelationalExpr
  | EqualityExpr EQEQ RelationalExpr
  | EqualityExpr NE RelationalExpr
  | EqualityExpr STREQ RelationalExpr
  | EqualityExpr STRNEQ RelationalExpr
  ;
  
  EqualityExprNoIn
    RelationalExprNoIn
  | EqualityExprNoIn EQEQ RelationalExprNoIn
  | EqualityExprNoIn NE RelationalExprNoIn
  | EqualityExprNoIn STREQ RelationalExprNoIn
  | EqualityExprNoIn STRNEQ RelationalExprNoIn
  ;

  EqualityExprNoBF
    RelationalExprNoBF
  | EqualityExprNoBF EQEQ RelationalExpr
  | EqualityExprNoBF NE RelationalExpr
  | EqualityExprNoBF STREQ RelationalExpr
  | EqualityExprNoBF STRNEQ RelationalExpr
  ;

  BitwiseANDExpr
    EqualityExpr
  | BitwiseANDExpr '&' EqualityExpr
  ;

  BitwiseANDExprNoIn
    EqualityExprNoIn
  | BitwiseANDExprNoIn '&' EqualityExprNoIn
  ;

  BitwiseANDExprNoBF
    EqualityExprNoBF
  | BitwiseANDExprNoBF '&' EqualityExpr
  ;

  BitwiseXORExpr
    BitwiseANDExpr
  | BitwiseXORExpr '^' BitwiseANDExpr
  ;

  BitwiseXORExprNoIn
    BitwiseANDExprNoIn
  | BitwiseXORExprNoIn '^' BitwiseANDExprNoIn
  ;

  BitwiseXORExprNoBF
    BitwiseANDExprNoBF
  | BitwiseXORExprNoBF '^' BitwiseANDExpr
  ;

  BitwiseORExpr
    BitwiseXORExpr
  | BitwiseORExpr '|' BitwiseXORExpr
  ;

  BitwiseORExprNoIn
    BitwiseXORExprNoIn
  | BitwiseORExprNoIn '|' BitwiseXORExprNoIn
  ;

  BitwiseORExprNoBF
    BitwiseXORExprNoBF
  | BitwiseORExprNoBF '|' BitwiseXORExpr
  ;

  LogicalANDExpr
    BitwiseORExpr
  | LogicalANDExpr AND BitwiseORExpr
  ;

  LogicalANDExprNoIn
    BitwiseORExprNoIn
  | LogicalANDExprNoIn AND BitwiseORExprNoIn
  ;

  LogicalANDExprNoBF
    BitwiseORExprNoBF
  | LogicalANDExprNoBF AND BitwiseORExpr
  ;

  LogicalORExpr
    LogicalANDExpr
  | LogicalORExpr OR LogicalANDExpr
  ;

  LogicalORExprNoIn
    LogicalANDExprNoIn
  | LogicalORExprNoIn OR LogicalANDExprNoIn
  ;

  LogicalORExprNoBF
    LogicalANDExprNoBF
  | LogicalORExprNoBF OR LogicalANDExpr
  ;

  ConditionalExpr
    LogicalORExpr
  | LogicalORExpr '?' AssignmentExpr ':' AssignmentExpr
  ;

  ConditionalExprNoIn
    LogicalORExprNoIn
  | LogicalORExprNoIn '?' AssignmentExprNoIn ':' AssignmentExprNoIn
  ;


  ConditionalExprNoBF 
    LogicalORExprNoBF
  | LogicalORExprNoBF '?' AssignmentExpr ':' AssignmentExpr
  ;

  AssignmentExpr
    ConditionalExpr
  | LeftHandSideExpr AssignmentOperator AssignmentExpr
  ;

  AssignmentExprNoIn
    ConditionalExprNoIn
  | LeftHandSideExpr AssignmentOperator AssignmentExprNoIn
  ;

  AssignmentExprNoBF
    ConditionalExprNoBF
  | LeftHandSideExprNoBF AssignmentOperator AssignmentExpr
  ;
  
  AssignmentOperator
   '='
  | PLUSEQUAL
  | MINUSEQUAL
  | MULTEQUAL
  | DIVEQUAL
  | LSHIFTEQUAL
  | RSHIFTEQUAL
  | URSHIFTEQUAL
  | ANDEQUAL
  | XOREQUAL
  | OREQUAL
  | MODEQUAL
  | '+='
  | '-='
  | '*='
  | '/='
  ;

  Expr
    AssignmentExpr
  | Expr ',' AssignmentExpr
  ;

  ExprNoIn
    AssignmentExprNoIn
  | ExprNoIn ',' AssignmentExprNoIn
  ;

  ExprNoBF
    AssignmentExprNoBF
  | ExprNoBF ',' AssignmentExpr
  ;

  Statement
    Block
  | VariableStatement
  | ConstStatement
  | FunctionDeclaration
  | EmptyStatement
  | ExprStatement
  | IfStatement
  | IterationStatement
  | ContinueStatement
  | BreakStatement
  | ReturnStatement
  | WithStatement
  | SwitchStatement
  | LabelledStatement
  | ThrowStatement
  | TryStatement
  | DebuggerStatement
  ;


  Block
    OPENBRACE CLOSEBRACE
  | OPENBRACE SourceElements CLOSEBRACE
  | INDENT DEDENT
  | INDENT SourceElements DEDENT
  ;


  VariableStatement
    VAR VariableDeclarationList ';'
  | VAR VariableDeclarationList error
  | VARIABLE IDENTIFIER ':'
  | VARIABLES VariableDeclarationList ':'
  | VARIABLE IDENTIFIER error
  | VARIABLES VariableDeclaration List error
  ;

  VariableDeclarationList
  : IDENT
  | IDENT Initializer
  | VariableDeclarationList ',' IDENT
  | VariableDeclarationList ',' IDENT Initializer
  | IDENTIFIER
  | IDENTIFIER Initializer
  | VariableDeclarationList ',' IDENTIFIER
  | VariableDeclarationList ',' IDENTIFIER Initializer
  ;

  VariableDeclarationListNoIn
  : IDENT
  | IDENT InitializerNoIn
  | VariableDeclarationListNoIn ',' IDENT
  | VariableDeclarationListNoIn ',' IDENT InitializerNoIn
  | IDENTIFIER
  | IDENTIFIER InitializerNoIn
  | VariableDeclarationListNoIn ',' IDENTIFIER
  | VariableDelarationListNoIn ',' IDENTIFIER InitializerNoIn
  ;

  ConstStatement
    CONSTTOKEN ConstDeclarationList ';'
  | CONSTTOKEN ConstDeclarationList error
  | CONSTANT ConstDeclarationList ':'
  | CONSTANT ConstDeclarationList error
  ;

  ConstDeclarationList
    ConstDeclaration
  | ConstDeclarationList ',' ConstDeclaration
  ;

  ConstDeclaration
    IDENT
  | IDENT Initializer
  | IDENTIFIER 
  | IDENTIFIER Initializer
  ;

  Initializer
    '=' AssignmentExpr
  ;

  InitializerNoIn
    '=' AssignmentExprNoIn
  ;

  EmptyStatement
    ';'
  ;

  ExprStatement
    ExprNoBF ';'
  | ExprNoBF error
  ;

  IfStatement
    IF '(' Expr ')' Statement %prec IF_WITHOUT_ELSE
  | IF '(' Expr ')' Statement ELSE Statement
  ;


  # A method call
  Call:
    # method
    IDENTIFIER                    { result = CallNode.new(nil, val[0], []) }
    # method(arguments)
  | IDENTIFIER "(" ArgList ")"    { result = CallNode.new(nil, val[0], val[2]) }
    # receiver.method
  | Expression "." IDENTIFIER     { result = CallNode.new(val[0], val[2], []) }
    # receiver.method(arguments)
  | Expression "."
      IDENTIFIER "(" ArgList ")"  { result = CallNode.new(val[0], val[2], val[4]) }
  ;
  
  ArgList:
    /* nothing */                 { result = [] }
  | Expression                    { result = val }
  | ArgList "," Expression        { result = val[0] << val[2] }
  ;
  
  Operator:
  # Binary operators
    Expression '||' Expression    { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '&&' Expression    { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '==' Expression    { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '!=' Expression    { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '>' Expression     { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '>=' Expression    { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '<' Expression     { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '<=' Expression    { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '+' Expression     { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '-' Expression     { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '*' Expression     { result = CallNode.new(val[0], val[1], [val[2]]) }
  | Expression '/' Expression     { result = CallNode.new(val[0], val[1], [val[2]]) }
  ;
  
  Constant:
    CONSTANT                      { result = GetConstantNode.new(val[0]) }
  ;
  
  # Assignment to a variable or constant
  Assign:
    IDENTIFIER "=" Expression     { result = SetLocalNode.new(val[0], val[2]) }
  | CONSTANT "=" Expression       { result = SetConstantNode.new(val[0], val[2]) }
  
  ;
  
  # Method definition
  Def:
    DEF IDENTIFIER Block          { result = DefNode.new(val[1], [], val[2]) }
  | DEF IDENTIFIER
      "(" ParamList ")" Block     { result = DefNode.new(val[1], val[3], val[5]) }
  ;

  ParamList:
    /* nothing */                 { result = [] }
  | IDENTIFIER                    { result = val }
  | ParamList "," IDENTIFIER      { result = val[0] << val[2] }
  ;
  
  # Class definition
  Class:
    CLASS CONSTANT Block          { result = ClassNode.new(val[1], val[2]) }
  ;
  
  # if block
  If:
    IF Expression Block           { result = IfNode.new(val[1], val[2]) }
  ;
  
  # A block of indented code. You see here that all the hard work was done by the
  # lexer.
  Block:
    INDENT Expressions DEDENT     { result = val[1] }
  # If you don't like indentation you could replace the previous rule with the 
  # following one to separate blocks w/ curly brackets. You'll also need to remove the
  # indentation magic section in the lexer.
  # "{" Expressions "}"           { replace = val[1] }
  ;
end

---- header
  require "lexer"
  require "nodes"

---- inner
  # This code will be put as-is in the Parser class.
  def parse(code, show_tokens=false)
    @tokens = Lexer.new.tokenize(code) # Tokenize the code using our lexer
    puts @tokens.inspect if show_tokens
    do_parse # Kickoff the parsing process
  end
  
  def next_token
    @tokens.shift
  end