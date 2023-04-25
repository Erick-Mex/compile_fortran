import ply.lex as lex
import ply.yacc as yacc
import math
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
    'FACTORIAL', # !
#    'DOT', # .
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
# t_DOT = r"\."
t_COMMA = r"\,"
t_DOUBLEPOINT = r"\:\:"
t_SEMICOLON = r"\;"
t_FACTORIAL = r"\!"

t_ignore_COMMENT = r"\#.*"
t_ignore = r' '

def t_RREAL(t):
    r'-?\d+\.\d+'
    t.value = float(t.value)
    return t

def t_RINT(t):
    r'-?\d+'
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
    ('left', 'MULTIPLY', 'DIVIDE'),
    ('left', 'AND', 'OR'),
    ('left', 'FACTORIAL', 'PLUS'),
    ('left', 'FACTORIAL', 'MINUS'),
    ('left', 'FACTORIAL', 'MULTIPLY'),
    ('left', 'FACTORIAL', 'DIVIDE'),
)

def output_list(data):
    if type(data) != list:
        print(data)
    else:
        for item in data:
            output_list(item)
    
def evaluate(instructions: list, operations: list):
    # print("+++++ operations function", instructions)
    for operation in instructions:
        value = run(operation)
        if value != None:
            operations.append(value)
    # print("----- operations results", operations)
    return operations

def p_init_program(p):
    '''
    program : PROGRAM MAIN calcs END PROGRAM MAIN
    '''
    for item in p[3]:
        execution = run(item)
        if execution != None:
            if type(execution) == list:
                output_list(execution)
            else:
                print(execution)

def p_operations(p):
    '''
    calcs : calcs calc
    '''
    p[1].append(p[2])
    p[0] = p[1]

def p_calcs_cal(p):
    '''
    calcs : calc
    '''
    p[0] = [p[1]]

def p_calc(p):
    # expression - for line to line execution
    '''
    calc : if_condition
         | while_loop
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
    var : type DOUBLEPOINT multiple_var 
    '''
    p[0] = ('var_declare', p[1], p[3])

def p_multiple(p):
    '''
    multiple_var : ID COMMA multiple_var
    '''
    p[3].append(p[1])
    p[0] = p[3]

def p_multiple_variables(p):
    '''
    multiple_var : ID
    '''
    p[0] = [p[1]]

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

def p_expression_conditional(p):
    '''
    expression : condition
    '''
    p[0] = p[1]

def p_expression_parent(p):
    '''
    expression : LPARENT expression RPARENT
               | LPARENT condition RPARENT
    '''
    p[0] = p[2]

def p_expression(p):
    '''
    expression : expression MULTIPLY expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
               | expression FACTORIAL
    '''
    # Build our tree.
    if len(p) == 3:
        p[0] = (p[2], p[1])
    else:
        p[0] = (p[2], p[1], p[3])

def p_expression_value(p):
    '''
    expression : RINT
               | RREAL
               | RSTRING
               | RBOOL
               | TRUE
               | FALSE
    '''
    p[0] = p[1]

def p_condition(p):
    '''
    condition : expression EQUALS expression
              | expression MORETHAN expression
              | expression LESSTHAN expression
              | expression MOREEQUAL expression
              | expression LESSEQUAL expression
              | expression DIFFERENT expression
              | expression AND condition
              | expression OR condition
              | expression
    '''
    # (operador, l_expresion, r_expression)
    if len(p) > 3:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_if_condition(p):
    '''
    if_condition : IF LPARENT condition RPARENT THEN LCURLY calcs RCURLY
                 | IF LPARENT condition RPARENT THEN LCURLY calcs RCURLY ELSE LCURLY calcs RCURLY
    '''
    if type(p[3]) == bool or len(p[3]) < 3:
        if len(p) == 9:
            p[0] = ('if_unique', p[3], p[7])
        else:
            p[0] = ('if_unique', p[3], p[7], p[11])
    elif type(p[3]) == tuple:
        if len(p) == 9:
            p[0] = ('if', p[3], p[7])
        else:
            p[0] = ('if', p[3], p[7], p[11])

def p_while_loop(p):
    '''
    while_loop : WHILE LPARENT condition RPARENT DO LCURLY calcs RCURLY
    '''
    p[0] = ("while", p[3], p[7])

# def p_for_loop(p):
#     '''
#     for_loop : FOR 
#     '''

def p_print(p):
    '''
    print : PRINT LPARENT expression RPARENT
    '''
    p[0] = p[3]


def p_error(p):
    if p:
        print("Syntax error at token", p.type, "(", p.value, ") at line", p.lineno, "position", p.lexpos)
    else:
        print("Syntax error at EOF")

def p_empty(p):
    '''
    empty :
    '''
    p[0] = ''

parser = yacc.yacc()

env = {}
# operations = []
def run(p):
    global env
    # global operations
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
        elif p[0] == "!":
            value = run(p[1])
            if type(value) != int:
                raise TypeError(f"{type(value)} used in this method. An intenger must be used")
            
            return math.factorial(run(value))
        # ========== OPERACIONES CON VARIABLES ===========
        elif p[0] == 'var':
            if p[1] not in env:
                raise NameError(f"Variable \'{p[1]}\' is not declared")

            value = env[p[1]]["value"]
            if value == None:
                raise NameError(f"name '{p[1]}' is not defined")
            return value
        elif p[0] == 'var_declare':
            for i in p[2]:
                if i not in env:
                    env[i] = {"type_data": p[1], "value": None}
                else:
                    raise AttributeError(f"Redefinition of the variable '{p[2]}'")
            return None
        # ======== OPERACIONES LOGICAS ===============
        elif p[0] == "and":
            return run(p[1]) and run(p[2])
        elif p[0] == "or":
            return run(p[1]) or run(p[2])
        elif p[0] == ">":
            return run(p[1]) > run(p[2])
        elif p[0] == "<":
            return run(p[1]) < run(p[2])
        elif p[0] == ">=":
            return run(p[1]) >= run(p[2])
        elif p[0] == "<=":
            return run(p[1]) <= run(p[2])
        elif p[0] == "!=":
            return run(p[1]) != run(p[2])
        elif p[0] == "==":
            return run(p[1]) == run(p[2])
        elif p[0] == "if":
            operator = p[1][0]
            left = p[1][1]
            right = p[1][2]
            logic = run((operator, run(left), run(right)))
            operations = []
            # cuando el if no tiene else
            if len(p) == 3:
                if logic: 
                    return evaluate(p[2], operations)
            # cuando el if tiene else
            else:
                if logic: 
                    return evaluate(p[2], operations)
                else: 
                    return evaluate(p[3], operations)
        elif p[0] == "if_unique":
            operations = []
            logic = run(p[1])
            # operacion si no tiene ELSE 
            if len(p) == 3:
                if logic: 
                    return evaluate(p[2], operations)
            # operacion si tiene  ELSE
            else:
                return evaluate(p[2], operations) if logic else evaluate(p[3], operations)
        elif p[0] == "while":
            operations = []
            # Si la condicional es un booleano o una variable
            if type(p[1]) == bool or len(p[1]) < 3:
                while run(p[1]):
                    operations = evaluate(p[2], operations)
                return operations
            else:
                operator = p[1][0]
                while run((operator, run(p[1][1]), run(p[1][2]))):
                    operations = evaluate(p[2], operations)
                
                return operations
    elif type(p) == str:
        return p.strip('\"')
    else:
        return p

## ================= Test 1 ==================
# program main
# int :: i,n,f,x
# print("texto dump")
# f=5
# x=1
# for (x;x<f;x=x+1){
# 	n = x + f
# }
# print(n)
# end program main
## ================= Test 2 ==================
# program main
# int :: i,n,f,x
# print("texto dump")
# f=1
# x=5
# if( x > f) then 
# {
# 	n = x + f
# }else{
# 	n = x - 4
# }
# print(n)
# end program main
# ================== Test 3 ================
# program main
# int :: i,n,f,x
# print("texto dump")
# f=1
# x=5
# while (x<f) do
# {
# 	x = f + 2
# }
# print(n)
# end program main
# ================== Test 4 ================
# int :: i, n, f, x
# bool :: z
# print("text dump")
# f = 3
# x = 1
# z = true
# while(x < f) do
# {
#     x = x + 1
#     if (x == 2) then {
#         print("estoy dentro")
#         if (true) then {
#             print("dentro de un true")
#             while (z) do {
#                 print("dentro del while")
#                 z = false
#             }
#         }
#     }
# }
# print(f)
if __name__ == "__main__":
    s = '''program main
        int :: i, n, f
        bool :: z, x
        f = 3
        x = (2+4) > 5
        print(((2+4) > 5) == x)
    end program main
    '''
    try:
        parser.parse(s)
    except SyntaxError as e:
        print(f"SyntaxError: {e.msg} at line {e.lineno}, col {e.offset}: {e.text}")
