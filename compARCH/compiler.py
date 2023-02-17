# lexical analysis
import re
from collections import deque

# The symbol table maps variable names to stack offsets
symbol_table = {}
# A counter used to generate unique labels for branches and loops
label_count = 0

# Define a regular expression pattern for each token type
patterns = [
    (r"\b(int|float|double|char|void)\b", "TYPE"),
    (r"\b(if|else|while|for|do|switch|case|default|break|continue|return)\b", "KEYWORD"),
    (r"\b(true|false)\b", "BOOLEAN"),
    (r"[a-zA-Z_]\w*", "IDENTIFIER"),
    (r"\d+(\.\d+)?", "NUMBER"),
    (r"'.'", "CHARACTER"),
    (r'"(\\"|[^"])*"', "STRING"),
    (r"\+|-|\*|/|%|==|!=|<=|>=|<|>|&&|\|\||!|&|\||~|\^", "OPERATOR"),
    (r"\(|\)|\[|\]|\{|\}|;|,|\.", "PUNCTUATION"),
    (r"//.*?$|/\*.*?\*/", "COMMENT"),
    (r"\s+", "WHITESPACE")
]

# Combine the regular expressions into a single pattern
pattern = "|".join(f"(?P<{name}>{pattern})" for pattern, name in patterns)

def tokenize(source):
    # Apply the pattern to the source code and iterate over the resulting match objects
    for match in re.finditer(pattern, source, re.MULTILINE | re.DOTALL):
        # Extract the token type and value from the match object
        token_type = match.lastgroup
        value = match.group(token_type)

        # Skip whitespace and comments
        if token_type in ["WHITESPACE", "COMMENT"]:
            continue

        # Yield the token type and value as a tuple
        yield token_type, value

# Step 2: Syntax Analysis


# Define the grammar for the C language using a context-free grammar (CFG)
grammar = {
    "<program>": ["<declaration-list>"],
    "<declaration-list>": ["<declaration>", "<declaration-list>", None],
    "<declaration>": ["<type-specifier>", "IDENTIFIER", "<declarator-suffix>", ";"],
    "<type-specifier>": ["int", "float", "double", "char", "void"],
    "<declarator-suffix>": ["<array-suffix>", "<function-suffix>", None],
    "<array-suffix>": ["[", "NUMBER", "]", "<array-suffix>", None],
    "<function-suffix>": ["(", "<parameter-list>", ")", "<function-suffix>", None],
    "<parameter-list>": ["<parameter>", ",", "<parameter-list>", None],
    "<parameter>": ["<type-specifier>", "IDENTIFIER", "<array-suffix>", None],
    "<statement>": ["<expression-statement>", "<compound-statement>", "<selection-statement>", "<iteration-statement>", None],
    "<expression-statement>": ["<expression>", ";"],
    "<expression>": ["<assignment-expression>", "<expression-suffix>", None],
    "<expression-suffix>": [",", "<expression>", None],
    "<assignment-expression>": ["<conditional-expression>", "=", "<assignment-expression>", None],
    "<conditional-expression>": ["<logical-or-expression>", "?", "<expression>", ":", "<conditional-expression>", None],
    "<logical-or-expression>": ["<logical-and-expression>", "||", "<logical-or-expression>", None],
    "<logical-and-expression>": ["<bitwise-or-expression>", "&&", "<logical-and-expression>", None],
    "<bitwise-or-expression>": ["<bitwise-xor-expression>", "|", "<bitwise-or-expression>", None],
    "<bitwise-xor-expression>": ["<bitwise-and-expression>", "^", "<bitwise-xor-expression>", None],
    "<bitwise-and-expression>": ["<equality-expression>", "&", "<bitwise-and-expression>", None],
    "<equality-expression>": ["<relational-expression>", "==", "<equality-expression>", "!=", "<equality-expression>", None],
    "<relational-expression>": ["<shift-expression>", "<", "<relational-expression>", ">", "<relational-expression>", "<=", "<relational-expression>", ">=", "<relational-expression>", None],
    "<shift-expression>": ["<additive-expression>", "<<", "<shift-expression>", ">>", "<shift-expression>", None],
    "<additive-expression>": ["<multiplicative-expression>", "+", "<additive-expression>", "-", "<additive-expression>", None],
    "<multiplicative-expression>": ["<cast-expression>", "*", "<multiplicative-expression>", "/", "<multiplicative-expression>", "%", "<multiplicative-expression>", None],
    "<cast-expression>": ["(<type-specifier>)", "<unary-expression>", None],
    "<unary-expression>": ["<postfix-expression>", "++", "<unary-expression>", "--", "<unary-expression>", "<unary-operator>", "<cast-expression>", None],
    "<postfix-expression>": ["IDENTIFIER", "<array-suffix>", "<function-suffix>", "(", "<argument-list>", ")", "++", "--", ".", "IDENTIFIER", "->", "IDENTIFIER", None],
    "<argument-list>": ["<assignment-expression>", ",", "<argument-list>", None],
    "<compound-statement>": ["{", "<declaration-list>", "<statement-list>", "}", None],
    "<statement-list>": ["<statement>", "<statement-list>", None],
    "<selection-statement>": ["if", "(", "<expression>", ")", "<statement>", "else", "<statement>", "if", "(", "<expression>", ")", "<statement>", None],
    "<iteration-statement>": ["while", "(", "<expression>", ")", "<statement>", "for", "(", "<for-expression>", ")", "<statement>", None],
    "<for-expression>": ["<for-declaration>", "<for-expression-suffix>", "<assignment-expression>", None],
    "<for-declaration>": ["<type-specifier>", "IDENTIFIER", "=", "<expression>", ";", None],
    "<for-expression-suffix>": ["<assignment-expression>", ";", "<for-expression-suffix>", None],
    "<input-statement>": ["scanf", "(", "<input-format-string>", ",", "<input-list>", ")", ";", None],
    "<output-statement>": ["printf", "(", "<output-format-string>", ",", "<output-list>", ")", ";", None],
    "<input-format-string>": ['"%"', "'d'", None],
    "<output-format-string>": ['"%"', "'d'", None],
    "<input-list>": ["&", "IDENTIFIER", ",", "<input-list>", None],
    "<output-list>": ["<expression>", ",", "<output-list>", None],
}
 #define token types
token_map = {
"IDENTIFIER": "[a-zA-Z_][a-zA-Z0-9_]",
"NUMBER": "[0-9]+",
"STRING_LITERAL": '".?"',
"CHARACTER_CONSTANT": "'.'",
"PLUS": "\+",
"MINUS": "-",
"MULTIPLY": "\*",
"DIVIDE": "/",
"MODULO": "%",
"LEFT_PAREN": "\(",
"RIGHT_PAREN": "\)",
"LEFT_BRACKET": "\[",
"RIGHT_BRACKET": "\]",
"LEFT_BRACE": "\{",
"RIGHT_BRACE": "\}",
"SEMICOLON": ";",
"COMMA": ",",
"EQUALS": "==",
"NOT_EQUALS": "!=",
"LESS_THAN": "<",
"LESS_THAN_OR_EQUAL": "<=",
"GREATER_THAN": ">",
"GREATER_THAN_OR_EQUAL": ">=",
"AND": "&&",
"OR": "\|\|",
"NOT": "!",
"BITWISE_AND": "&",
"BITWISE_OR": "\|",
"BITWISE_XOR": "\^",
"BITWISE_NOT": "~",
"LEFT_SHIFT": "<<",
"RIGHT_SHIFT": ">>",
"INCREMENT": "\+\+",
"DECREMENT": "--",
"STRUCTURE_POINTER": "->",
"STRUCTURE_MEMBER": "\.",
"ASSIGNMENT": "=",
"IF": "if",
"ELSE": "else",
"WHILE": "while",
"FOR": "for",
"INT": "int",
"FLOAT": "float",
"DOUBLE": "double",
"CHAR": "char",
"VOID": "void",
"SCANF": "scanf",
"PRINTF": "printf"
}


#Create a regular expression to match any token
regex = "|".join(f"(?P<{k}>{v})" for k, v in token_map.items())

def tokenize(program):
#Takes a string containing a C program and returns a list of tokens.

    tokens = []
    for match in re.finditer(regex, program):
        for name, value in match.groupdict().items():
            if value and name != "WHITESPACE":
                tokens.append((name, value))
        return tokens
def parse_expression(tokens):

    #Parses an expression and returns the AST.

    # Parse the first term
    ast = parse_term(tokens)
    # Keep parsing until we run out of tokens or hit the end of the expression
    while tokens and tokens[0][0] in ("PLUS", "MINUS"):
        # Get the operator
        operator = tokens.pop(0)

        # Parse the next term
        term = parse_term(tokens)

        # Combine the two terms into a new AST node
        ast = ("BINOP", operator[0], ast, term)

    return ast
def parse_term(tokens):

#Parses a term and returns the AST.

    # Parse the first factor
    ast = parse_factor(tokens)
    # Keep parsing until we run out of tokens or hit the end of the term
    while tokens and tokens[0][0] in ("MULTIPLY", "DIVIDE", "MODULO"):
        # Get the operator
        operator = tokens.pop(0)

        # Parse the next factor
        factor = parse_factor(tokens)

        # Combine the two factors into a new AST node
        ast = ("BINOP", operator[0], ast, factor)

    return ast
def parse_factor(tokens):

#Parses a factor and returns the AST.

    # Check if the factor is a number or an identifier
    if tokens[0][0] == "NUMBER":
        return ("NUMBER", int(tokens.pop(0)[1]))
    elif tokens[0][0] == "IDENTIFIER":
        return ("IDENTIFIER", tokens.pop(0)[1])
    # Check for parentheses
    if tokens[0][0] == "LEFT_PAREN":
        # Pop the left paren and parse the expression inside
        tokens.pop(0)
        ast = parse_expression(tokens)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)
        return ast

    # If we get here, we don't recognize the factor
    raise Exception(f"Unexpected token: {tokens[0][1]}")
def parse_statement(tokens):

#Parses a statement and returns the AST.

    # Check for an if statement
    if tokens[0][0] == "IF":
        tokens.pop(0)
    # Check for the left paren
    if tokens[0][0] != "LEFT_PAREN":
        raise Exception("Expected '('")

    tokens.pop(0)

    # Parse the expression
    condition = parse_expression(tokens)

    # Check for the right paren
    if tokens[0][0] != "RIGHT_PAREN":
        raise Exception("Expected ')'")

    tokens.pop(0)

    # Parse the if statement
    if_statement = parse_statement(tokens)

    # Check for an else statement
    if tokens and tokens[0][0] == "ELSE":
        tokens.pop(0)
        else_statement = parse_statement(tokens)
    else:
        else_statement = None

    return ("IF", condition, if_statement, else_statement)

    # Check for a while loop
    if tokens[0][0] == "WHILE":
        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the expression
        condition = parse_expression(tokens)

        
        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Parse the loop body
        body = parse_statement(tokens)

        return ("WHILE", condition, body)

    # Check for a do-while loop
    if tokens[0][0] == "DO":
        tokens.pop(0)

        # Parse the loop body
        body = parse_statement(tokens)

        # Check for the while keyword
        if tokens[0][0] != "WHILE":
            raise Exception("Expected 'while'")

        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the expression
        condition = parse_expression(tokens)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("DO_WHILE", condition, body)

    # Check for a for loop
    if tokens[0][0] == "FOR":
        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the initialization statement
        if tokens[0][0] == "SEMICOLON":
            init = None
        else:
            init = parse_statement(tokens)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        # Parse the condition
        if tokens[0][0] == "SEMICOLON":
            condition = None
        else:
            condition = parse_expression(tokens)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        # Parse the update statement
        if tokens[0][0] == "RIGHT_PAREN":
            update = None
        else:
            update = parse_statement(tokens)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Parse the loop body
        body = parse_statement(tokens)

        return ("FOR", init, condition, update, body)

    # Check for a variable declaration
    if tokens[0][0] == "INT":
        tokens.pop(0)

        # Get the variable name
        if tokens[0][0] != "IDENTIFIER":
            raise Exception("Expected identifier")

        name = tokens.pop(0)[1]

        # Check for an assignment
        if tokens[0][0] == "ASSIGN":
            tokens.pop(0)
            value = parse_expression(tokens)
        else:
            value = None

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("VAR_DECL", name, value)

    # Check for a function declaration
    if tokens[0][0] == "INT":
        tokens.pop(0)

        # Get the function name
        if tokens[0][0] != "IDENTIFIER":
            raise Exception("Expected identifier")

        name = tokens.pop(0)[1]
            # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the parameter list
        params = []
        if tokens[0][0] != "RIGHT_PAREN":
            while True:
                # Get the parameter name
                if tokens[0][0] != "INT":
                    raise Exception("Expected 'int'")

                tokens.pop(0)[1]

                if tokens[0][0] != "IDENTIFIER":
                    raise Exception("Expected identifier")

                param_name = tokens.pop(0)[1]

                params.append(param_name)

                if tokens[0][0] == "RIGHT_PAREN":
                    break

                if tokens[0][0] != "COMMA":
                    raise Exception("Expected ','")

                tokens.pop(0)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Parse the function body
        body = parse_statement(tokens)

        return ("FUNC_DECL", name, params, body)

    # Check for a return statement
    if tokens[0][0] == "RETURN":
        tokens.pop(0)

        # Parse the return value
        value = parse_expression(tokens)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("RETURN", value)

    # Check for a function call
    if tokens[0][0] == "IDENTIFIER":
        name = tokens.pop(0)[1]

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the argument list
        args = []
        if tokens[0][0] != "RIGHT_PAREN":
            while True:
                arg = parse_expression(tokens)
                args.append(arg)

                if tokens[0][0] == "RIGHT_PAREN":
                    break

                if tokens[0][0] != "COMMA":
                    raise Exception("Expected ','")

                tokens.pop(0)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("FUNC_CALL", name, args)

    # Check for a print statement
    if tokens[0][0] == "PRINT":
        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the expression
        value = parse_expression(tokens)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("PRINT", value)

    # Check for a read statement
    if tokens[0][0] == "READ":
        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

    # Get the variable name
        if tokens[0][0] != "IDENTIFIER":
            raise Exception("Expected identifier")

        var_name = tokens.pop(0)[1]

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("READ", var_name)

    # Check for an if statement
    if tokens[0][0] == "IF":
        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the condition
        condition = parse_expression(tokens)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Parse the if block
        if_block = parse_statement(tokens)

        # Check for an else block
        if tokens and tokens[0][0] == "ELSE":
            tokens.pop(0)

            else_block = parse_statement(tokens)
        else:
            else_block = None

        return ("IF", condition, if_block, else_block)

    # Check for a while loop
    if tokens[0][0] == "WHILE":
        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the condition
        condition = parse_expression(tokens)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Parse the loop body
        body = parse_statement(tokens)

        return ("WHILE", condition, body)

    # Check for a for loop
    if tokens[0][0] == "FOR":
        tokens.pop(0)

        # Check for the left paren
        if tokens[0][0] != "LEFT_PAREN":
            raise Exception("Expected '('")

        tokens.pop(0)

        # Parse the initialization
        init = parse_expression(tokens)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        # Parse the condition
        condition = parse_expression(tokens)

        # Check for the semicolon
        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        # Parse the increment
        increment = parse_expression(tokens)

        # Check for the right paren
        if tokens[0][0] != "RIGHT_PAREN":
            raise Exception("Expected ')'")

        tokens.pop(0)

        # Parse the loop body
        body = parse_statement(tokens)

        return ("FOR", init, condition, increment, body)

    # Check for an array assignment
    if tokens[0][0] == "IDENTIFIER":
        var_name = tokens[0][1]
        if tokens[1][0] == "LEFT_BRACKET":
            tokens.pop(0)
            tokens.pop(0)
            index = parse_expression(tokens)

            if tokens[0][0] != "RIGHT_BRACKET":
                raise Exception("Expected ']'")

            tokens.pop(0)

            if tokens[0][0] != "EQUALS":
                    tokens.pop(0)
                    expr = parse_expression(tokens)

            if tokens[0][0] != "SEMICOLON":
                raise Exception("Expected ';'")

            tokens.pop(0)

            return ("ARRAY_ASSIGN", var_name, index, expr)

    # Check for a variable assignment
    if tokens[0][0] == "IDENTIFIER" and tokens[1][0] == "EQUALS":
        var_name = tokens[0][1]
        tokens.pop(0)
        tokens.pop(0)
        expr = parse_expression(tokens)

        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("ASSIGN", var_name, expr)

    # Check for a function call
    if tokens[0][0] == "IDENTIFIER" and tokens[1][0] == "LEFT_PAREN":
        func_name = tokens[0][1]
        tokens.pop(0)
        tokens.pop(0)

        args = []
        while tokens[0][0] != "RIGHT_PAREN":
            args.append(parse_expression(tokens))

            if tokens[0][0] == "COMMA":
                tokens.pop(0)
            elif tokens[0][0] != "RIGHT_PAREN":
                raise Exception("Expected ',' or ')'")

        tokens.pop(0)

        if tokens[0][0] != "SEMICOLON":
            raise Exception("Expected ';'")

        tokens.pop(0)

        return ("CALL", func_name, args)

    raise Exception(f"Invalid statement: {tokens[0][1]}")
def parse_statement_list(tokens):
    statement_list = []
    while tokens and tokens[0][0] != "RIGHT_BRACE":
        statement = parse_statement(tokens)
        statement_list.append(statement)
    return statement_list
def parse_function(tokens):
    if tokens[0][0] != "FUNCTION":
        raise Exception("Expected 'function'")
    tokens.pop(0)

    if tokens[0][0] != "IDENTIFIER":
        raise Exception("Expected function name")

    func_name = tokens[0][1]
    tokens.pop(0)

    if tokens[0][0] != "LEFT_PAREN":
        raise Exception("Expected '('")

    tokens.pop(0)

    args = []
    while tokens[0][0] != "RIGHT_PAREN":
        if tokens[0][0] != "IDENTIFIER":
            raise Exception("Expected argument name")

        arg_name = tokens[0][1]
        args.append(arg_name)
        tokens.pop(0)

        if tokens[0][0] == "COMMA":
            tokens.pop(0)

    tokens.pop(0)

    if tokens[0][0] != "LEFT_BRACE":
        raise Exception("Expected '{'")

    tokens.pop(0)

    statements = parse_statement_list(tokens)

    if tokens[0][0] != "RIGHT_BRACE":
        raise Exception("Expected '}'")

    tokens.pop(0)

    return ("FUNCTION", func_name, args, statements)
def parse_block(tokens):
    
#Parses a block of code (i.e. a sequence of statements) and returns the abstract syntax tree.

    statements = []
    while tokens:
        statement = parse_statement(tokens)
        statements.append(statement)

        # Skip any semicolons between statements
        while tokens and tokens[0][0] == "SEMICOLON":
            tokens.pop(0)

    return ("PROGRAM", statements)
# MIPS registers
t0 = "$t0"
t1 = "$t1"
t2 = "$t2"
t3 = "$t3"
t4 = "$t4"
t5 = "$t5"
t6 = "$t6"
t7 = "$t7"
t8 = "$t8"
t9 = "$t9"
s0 = "$s0"
s1 = "$s1"
s2 = "$s2"
s3 = "$s3"
s4 = "$s4"
s5 = "$s5"
s6 = "$s6"
s7 = "$s7"
fp = "$fp"
ra = "$ra"

# MIPS assembly commands
load_word = "lw"
store_word = "sw"
add = "add"
sub = "sub"
multiply = "mul"
divide = "div"
and_op = "and"
or_op = "or"
not_op = "not"
move = "move"
li = "li"
syscall = "syscall"
j = "j"
jal = "jal"
jr = "jr"
beq = "beq"
bne = "bne"
slt = "slt"
slt_eq = "sle"
sgt = "sgt"
sgt_eq = "sge"
nop = "nop"
# Global variables
current_function = None
stack_frame_size = 0
def generate_code(program):
    # Set up the data segment
    data_segment = ".data\n"

    # Generate code for global variables
    for statement in program[1]:
        if statement[0] == "VAR":
            var_name = statement[1]
            var_value = statement[2]
            data_segment += f"{var_name}: .word {var_value}\n"

    # Set up the text segment
    text_segment = ".text\n"
def generate_expression(expr):
    if type(expr) == int:
        return f"li $t0, {expr}"
    elif type(expr) == str:
        return f"lw $t0, {expr}"
    elif len(expr) == 3:
        op = expr[1]
        left = generate_expression(expr[0])
        right = generate_expression(expr[2])
        if op == "PLUS":
            return f"{left}\nadd $t0, $t0, {right}"
        elif op == "MINUS":
            return f"{left}\nsub $t0, $t0, {right}"
        elif op == "MULT":
            return f"{left}\nmult $t0, {right}\nmflo $t0"
        elif op == "DIV":
            return f"{left}\ndiv $t0, {right}\nmflo $t0"
        elif op == "MOD":
            return f"{left}\ndiv $t0, {right}\nmfhi $t0"
        elif op == "LT":
            return f"{left}\nslt $t0, $t0, {right}"
        elif op == "LTE":
            return f"{left}\nslt $t1, {right}, $t0\nxori $t0, $t1, 1"
        elif op == "GT":
            return f"{left}\nslt $t1, $t0, {right}\nxori $t0, $t1, 1"
        elif op == "GTE":
            return f"{left}\nslt $t0, {right}, $t0"
        elif op == "EQ":
            return f"{left}\nseq $t0, {right}"
        elif op == "NEQ":
            return f"{left}\nsne $t0, {right}"
    else:
        raise Exception(f"Invalid expression: {expr}")

def get_unique_label():
    global label_count
    label = f"LABEL{label_count}"
    label_count += 1
    return label

def generate_assignment(name, expr):
    if name in symbol_table:
        offset = symbol_table[name]
        expr_code = generate_expression(expr)
        return f"{expr_code}\nsw $t0, {offset}($fp)"
    else:
        raise Exception(f"Undefined variable: {name}")

def generate_statement(statement):
    global current_function
    global stack_frame_size

    if statement[0] == "ASSIGN":
        var_name = statement[1]
        expr = statement[2]

        # Generate code for the expression
        expr_code = generate_expression(expr)

        # Store the result in the variable
        var_offset = current_function["locals"][var_name]
        code = f"{expr_code}\n{store_word} {t0}, {var_offset}({fp})"

        return code

    if statement[0] == "ARRAY_ASSIGN":
        var_name = statement[1]
        index = statement[2]
        expr = statement[3]

        # Generate code for the index expression
        index_code = generate_expression(index)

        # Generate code for the value expression
        expr_code = generate_expression(expr)

        # Compute the address of the element
        array_offset = current_function["locals"][var_name]
        index_size = current_function["arrays"][var_name]["size"]
        code = f"{index_code}\n{multiply} {t0}, {t0}, {index_size}\n{add} {t0}, {t0}, {array_offset}\n{add} {t0}, {t0}, {fp}\n{expr_code}\n{store_word} {t0}, 0({t1})"

        return code

    if statement[0] == "IF":
        cond = statement[1]
        then_clause = statement[2]
        else_clause = statement[3] if len(statement) == 4 else None

        # Generate code for the condition
        cond_code = generate_expression(cond)

        # Generate code for the then clause
        then_code = generate_statement_list(then_clause)

        # Generate code for the else clause, if it exists
        else_code = ""
        if else_clause:
            else_code = generate_statement_list(else_clause)

        # Generate the full if statement code
        label = get_unique_label()
        code = f"{cond_code}\nbeq {t0}, $zero, {label}\n{then_code}\n{label}:"
        if else_clause:
            else_label = get_unique_label()
            code += f"\n{else_code}\n{else_label}:"

        return code

    if statement[0] == "WHILE":
        cond = statement[1]
        body = statement[2]

        # Generate labels for the loop
        start_label = get_unique_label()
        end_label = get_unique_label()

        # Generate code for the condition
        cond_code = generate_expression(cond)

        # Generate code for the body of the loop
        body_code = generate_statement_list(body)

        # Generate the full loop code
        code = f"{start_label}:\n{cond_code}\nbeq {t0}, $zero, {end_label}\n{body_code}\nj {start_label}\n{end_label}:"

        return code

    if statement[0] == "RETURN":
        expr = statement[1]

        # Generate code for the expression
        expr_code = generate_expression(expr)

        # Store the result in the return value register and return
        code = f"{expr_code}\n{move} $v0, {t0}\njr {ra}"

        return code

    raise Exception(f"Invalid statement: {statement}")
def generate_statement_list(statement_list):
    code = ""
    for statement in statement_list:
        code += generate_statement(statement) + "\n"
    return code
def compile_c_to_mips(c_code):
    # Parse the C code and generate an abstract syntax tree
    tokens = tokenize(c_code)
    ast = parse_block(tokens)

    # Generate MIPS assembly code from the abstract syntax tree
    mips_code = generate_code(ast)

    return mips_code

           




