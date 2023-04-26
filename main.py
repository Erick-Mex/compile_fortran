import ply.lex as lex
import ply.yacc as yacc
import math
import sys
import os

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
    for operation in instructions:
        value = run(operation)
        if value != None:
            operations.append(value)
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
    '''
    calc : if_condition
         | while_loop
         | for_loop
         | var
         | print
         | empty
    '''
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
    '''
    p[0] = p[1]

def p_condition(p):
    '''
    condition : condition EQUALS condition 
              | condition MORETHAN condition
              | condition LESSTHAN condition
              | condition MOREEQUAL condition
              | condition LESSEQUAL condition
              | condition DIFFERENT condition
              | condition AND condition
              | condition OR condition
              | expression
              | TRUE
              | FALSE
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

def p_condition_for(p):
    '''
    for_condition : var SEMICOLON condition SEMICOLON var
                  | expression SEMICOLON condition SEMICOLON var
    '''
    p[0] = (p[1], p[3], p[5])


def p_for_loop(p):
    '''
    for_loop : FOR LPARENT for_condition RPARENT LCURLY calcs RCURLY
    '''
    p[0] = ("for_loop", p[3], p[6])

def p_print(p):
    '''
    print : PRINT LPARENT expression RPARENT
          | PRINT LPARENT condition RPARENT
    '''
    p[0] = ('print', p[3])

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
        elif p[0] == "!":
            value = run(p[1])
            if type(value) != int:
                raise TypeError(f"{type(value)} used in this method. An intenger must be used")
            
            return math.factorial(value)
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
                    raise AttributeError(f"Redefinition of the variable '{i} from '{env[i]['type_data']}' to '{p[1]}'")
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
        # ============ LOOPS ============ 
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
            if type(p[1]) != str and len(p[1]) < 3:
                while run(p[1]):
                    operations = evaluate(p[2], operations)
                return operations
            else:
                operator = p[1][0]
                while run((operator, run(p[1][1]), run(p[1][2]))):
                    operations = evaluate(p[2], operations)
                
                return operations
        elif p[0] == "for_loop":
            init = run(p[1][0])
            logic = run(p[1][1])
            operations = []
            while logic:
                operations = evaluate(p[2], operations)
                run(p[1][2])
                logic = run(p[1][1])
            return operations
        elif p[0] == "print":
            print(run(p[1]))
            return None
    elif type(p) == str:
        return p.strip('\"')
    else:
        return p

if __name__ == "__main__":
    list_file = [x for x in os.listdir("./test") if x.endswith(".txt")]
    list_file.sort()
    for idx, file in enumerate(list_file):
        with open(f"./test/{file}", "r") as f:
            s = f.read()
            print("\n"*3)
            print("="*15, f"Test {idx + 1}: '{str(file)}'", "="*15)
            print(s)
            print("\n")
            try:
                header_result ="="*15 + f" Resultado {idx + 1}: '{str(file)}' " + "="*15
                print(header_result)
                parser.parse(s)
                print("="*len(header_result))
                env = {}
            except SyntaxError as e:
                print(f"SyntaxError: {e.msg} at line {e.lineno}, col {e.offset}: {e.text}")