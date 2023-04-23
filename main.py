import ply.lex as lex
import ply.yacc as yacc
import sys

reserved = {
    'program' : 'PROGRAM',
    'main' :    'MAIN',
    'if' :      'IF',
    'then' :    'THEN',
    'else' :    'ELSE',
    'do' :      'DO',
    'while' :   'WHILE',
    'end' :     'END',
    'print' :   'PRINT',
    'and' :     'AND',
    'or' :      'OR',
    'for' :     'FOR',
    'int' :     'INT',
    'real' :    'REAL',
    'string' :  'STRING',
    'bool' :    'BOOL',
    'true' :    'TRUE',
    'false' :   'FALSE'
}

tokens = [
    'LPARENT', # (
    'RPARENT', # )
    'LCURLY', # {
    'RCURLY', # }
    'EQUALS', # ==
    'MORETHAN', # >
    'LESSTHAN', # <
    'MOREEQUAL', # >=
    'LESSEQUAL', # <=
    'DIFFERENT', # !=
    'PLUS', # +
    'MINUS', # -
    'DIVIDE', # /
    'MULTIPLY', # *
    'ASSIGN', # =
    'DOT', # .
    'COMMA', # ,
    'DOUBLEPOINT', # :
    'SEMICOLON', # ;
    'COMMENT',
    'RINT',
    'RREAL',
    'RBOOL',
    'RSTRING',
    'ID'
]

## =================================== Lexico ===================================

tokens = tokens + list(reserved.values())

# LLAVES
t_LPARENT = r'\('
t_RPARENT = r'\)'
t_LCURLY = r'\{'
t_RCURLY = r'\}'

# LOGICOS
t_EQUALS = r'\=\='
t_MORETHAN = r"\>"
t_LESSTHAN = r"\<"
t_MOREEQUAL = r"\>\="
t_LESSEQUAL = r"\<\="
t_DIFFERENT = r"\!\="



# ARITMETICOS
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_ASSIGN = r'\='

# SIMBOLOS
t_DOT = r"\."
t_COMMA = r"\,"
t_DOUBLEPOINT = r"\:\:"
t_SEMICOLON = r"\;"

t_ignore_COMMENT = r"\#.*"
t_ignore = r' '

def t_RREAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_RINT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_RSTRING(t):
    r'\"[^\"]*\"'
    t.value = str(t.value)
    return t

def t_RBOOL(t):
    r'true|false'
    t.value = True if t.value == 'true' else False
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_newline(t):
    r'\n'
    t.lexer.lineno += 1

def t_error(t):
    print("Illegal characters!")
    t.lexer.skip(1)

lexer = lex.lex()

## =================================== Lexico ===================================

# Jerarquia de las operaciones
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE')
)

def p_init_program(p):
    '''
    program : PROGRAM MAIN calcs END PROGRAM MAIN
    '''
    for item in p[3]:
        execution = run(item)
        if execution != None:
            print(execution)

def p_operations(p):
    '''
    calcs : calc
          | calcs calc
    '''
    if len(p) > 2:
        p[1].append(p[2])
    p[0] = [p[1]] if len(p) == 2 else p[1]


def p_calc(p):
    # expression - for line to line execution
    '''
    calc : if_condition
         | var
         | print
         | empty
    '''
    # print(run(p[1]))
    p[0] = p[1]

def p_type_expression(p):
    '''
    type : INT
         | REAL
         | BOOL
         | STRING
    '''
    p[0] = p[1]

def p_var(p):
    '''
    var : type DOUBLEPOINT ID
    '''
    p[0] = ('var_declare', p[1], p[3])

def p_var_assign(p):
    '''
    var : ID ASSIGN expression 
    '''
    p[0] = ('=', p[1], p[3])

def p_expression_var(p):
    '''
    expression : ID
    '''
    p[0] = ('var', p[1])

def p_expression_parent(p):
    '''
    expression : LPARENT expression RPARENT
    '''
    p[0] = p[2]

def p_expression(p):
    '''
    expression : expression MULTIPLY expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
    '''
    # Build our tree.
    p[0] = (p[2], p[1], p[3])

def p_expression_value(p):
    '''
    expression : RINT
               | RREAL
               | RSTRING
               | RBOOL
    '''
    p[0] = p[1]

def p_condition(p):
    '''
    condition : expression EQUALS expression
              | expression MORETHAN expression
              | expression LESSTHAN expression
              | expression MOREEQUAL expression
              | expression LESSEQUAL expressionQUALS 
              | expression DIFFERENT expressionQUALS 
    '''
    p[0] = (p[2], p[1], p[3])

def p_if_condition(p):
    '''
    if_condition : IF LPARENT condition RPARENT THEN calc END IF
                 | IF LPARENT condition RPARENT THEN calc ELSE calc END IF
    '''
    print(p[2])

def p_print(p):
    '''
    print : PRINT LPARENT expression RPARENT
    '''
    p[0] = p[3]

def p_error(p):
    print("Syntax error found!")

def p_empty(p):
    '''
    empty :
    '''
    p[0] = ''

parser = yacc.yacc()

env = {}
def run(p):
    global env
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])
        elif p[0] == '-':
            return run(p[1]) - run(p[2])
        elif p[0] == '*':
            return run(p[1]) * run(p[2])
        elif p[0] == '/':
            return run(p[1]) / run(p[2])
        elif p[0] == '=':
            try:
                type_value = type(run(p[2]))
                type_data = env[p[1]]["type_data"]
                value = run(p[2])
            except:
                raise NameError(f"Variable \'{p[1]}\' is not declared")

            if type_value == int and type_data == "int":
                env[p[1]]["value"] = value
            elif type_value == float and type_data == "real":
                env[p[1]]["value"] = value
            elif type_value == str and type_data == "string":
                env[p[1]]["value"] = value
            elif type_value == bool and type_data == "bool":
                env[p[1]]["value"] = value
            else:
                if type_value == int: 
                    raise TypeError(f"Trying to assign a value of type 'int' in a variable of type '{type_data}'")
                elif type_value == float: 
                    raise TypeError(f"Trying to assign a value of type 'real' in a variable of type '{type_data}'")
                elif type_value == str: 
                    raise TypeError(f"Trying to assign a value of type 'string' in a variable of type '{type_data}'")
                elif type_value == bool: 
                    raise TypeError(f"Trying to assign a value of type 'bool' in a variable of type '{type_data}'")
            return None
        # ========== OPERACIONES CON VARIABLES ===========
        elif p[0] == 'var':
            if p[1] not in env:
                raise NameError(f"Variable \'{p[1]}\' is not declared")
            return env[p[1]]["value"]
        elif p[0] == 'var_declare':
            if p[2] not in env:
                env[p[2]] = {"type_data": p[1], "value": None}
                return None
            else:
                raise AttributeError(f"Redefinition of the variable '{p[2]}'")
        # ======== OPERACIONES LOGICAS ===============
    elif type(p) == str:
        return p.strip('\"')
    else:
        return p


if __name__ == "__main__":
    s = '''program main
        int :: x
        x = 12+2.2
        print(x)
        print(12)
    end program main
    '''
    parser.parse(s)
    # while True:
    #     try:
    #         s = input('>> ')
    #     except EOFError:
    #         break
    #     parser.parse(s)