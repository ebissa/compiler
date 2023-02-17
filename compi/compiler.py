import ply.lex as lex
import ply.yacc as yacc

# Define the tokens
tokens = (
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET',
    'IF', 'ELSE', 'WHILE', 'FOR', 'DO', 'SWITCH',
    'CASE', 'BREAK', 'CONTINUE', 'RETURN', 'FUNCTION',
    'INPUT', 'OUTPUT',
    'IDENTIFIER', 'NUMBER',
)

# Define regular expressions for simple tokens
t_PLUS = r'\+'
t_MINUS = r'\-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'

# Define regular expressions for complex tokens
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

# Define keywords
keywords = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'do': 'DO',
    'switch': 'SWITCH',
    'case': 'CASE',
    'break': 'BREAK',
    'continue': 'CONTINUE',
    'return': 'RETURN',
    'function': 'FUNCTION',
    'input': 'INPUT',
    'output': 'OUTPUT',
}

# Add the keywords to the tokens list
tokens += tuple(keywords.values())

# Define regular expression for ignoring whitespace
t_ignore = ' \t'

# Define regular expression for ignoring comments
def t_COMMENT(t):
    r'//.*'
    pass

# Define error handling
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Create the lexer
lexer = lex.lex()

# Define the grammar rules
def p_expression_plus(p):
    'expression : expression PLUS term'
    p[0] = ('+', p[1], p[3])

def p_expression_minus(p):
    'expression : expression MINUS term'
    p[0] = ('-', p[1], p[3])

def p_expression_term(p):
    'expression : term'
    p[0] = p[1]

def p_term_times(p):
    'term : term TIMES factor'
    p[0] = ('*', p[1], p[3])

def p_term_divide(p):
    'term : term DIVIDE factor'
    p[0] = ('/', p[1], p[3])

def p_term_factor(p):
    'term : factor'
    p[0] = p[1]

def p_factor_number(p):
    '''
    factor : NUMBER
    '''
    p[0] = ('number', p[1])

def p_factor_expr(p):
    '''
    factor : LPAREN expr RPAREN
    '''
    p[0] = p[2]

def p_factor_var(p):
    '''
    factor : VAR
    '''
    p[0] = ('var', p[1])

def p_bool_expression(p):
    '''
    bool_expression : expr LESS expr
                    | expr GREATER expr
                    | expr LESS_EQUAL expr
                    | expr GREATER_EQUAL expr
                    | expr EQUALS expr
                    | expr NOT_EQUAL expr
    '''
    p[0] = (p[2], p[1], p[3])

def p_logic_expression(p):
    '''
    logic_expression : bool_expression AND bool_expression
                     | bool_expression OR bool_expression
    '''
    p[0] = (p[2], p[1], p[3])

def p_if_statement(p):
    '''
    if_statement : IF LPAREN bool_expression RPAREN LBRACE statements RBRACE
                  | IF LPAREN bool_expression RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    '''
    if p[3]:
        p[0] = p[6]
    else:
        p[0] = p[10]

def p_while_loop(p):
    '''
    while_loop : WHILE LPAREN bool_expression RPAREN LBRACE statements RBRACE
    '''
    while p[3]:
        p[0] = p[6]

def p_for_loop(p):
    '''
    for_loop : FOR LPAREN VAR ASSIGN expr SEMICOLON bool_expression SEMICOLON VAR ASSIGN expr RPAREN LBRACE statements RBRACE
    '''
    for i in range(p[5], p[8], p[12]):
        p[0] = p[15]

def p_array_access(p):
    '''
    array_access : VAR LBRACKET expr RBRACKET
    '''
    p[0] = ('array_access', p[1], p[3])

def p_procedure_call(p):
    '''
    procedure_call : VAR LPAREN RPAREN
                    | VAR LPAREN expr RPAREN
                    | VAR LPAREN expr COMMA expr RPAREN
    '''
    if len(p) == 4:
        p[0] = ('procedure_call', p[1], [])
    elif len(p) == 5:
        p[0] = ('procedure_call', p[1], [p[3]])
    else:
        p[0] = ('procedure_call', p[1], [p[3], p[5]])

def p_statement(p):
    '''
    statement : expr SEMICOLON
              | bool_expression SEMICOLON
              | logic_expression SEMICOLON
              | if_statement
              | while_loop
              | for_loop
              | array_access ASSIGN expr SEMICOLON
              | procedure_call SEMICOLON
    '''
    p[0] = p[1]

def p_statements(p):
    '''
    statements : statement
               | statements statement
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

parser = yacc.yacc()

   
