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
    'false' :   'FALSE',
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
    'QUOTE', # "
    'DOUBLEPOINT', # :
    'SEMICOLON', # ;
    'COMMENT', # "#"
    'ID'
]

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
t_MOREEQUAL = r"\<"
t_LESSEQUAL = r"\<"
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
t_QUOTE = r"\"[^\"]*\""
t_DOUBLEPOINT = r"\:\:"
t_SEMICOLON = r"\;"

t_ignore_COMMENT = r"\#.*"
t_ignore = r' '

def t_REAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
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

data = """program main
    # comentario de lo que se me hinche el huevo
    int :: i,n,f,__x
    bool z = true
    print("text dump")
    f = 5
    x = 1
    for (x;x<f;x=x+1) {
        n = x + f
    }
    print(n)
end program main
"""
lex.input(data)
while True:
    tok = lex.token()
    if not tok:
        break
    print(tok)
