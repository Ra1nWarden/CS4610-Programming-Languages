import lex as lex
import sys
import re

tokens = (
    "NUMBER",
    "PLUSSSS",
    "MINUS",
    "TIMES",
    "DIVIDE",
    "LPAREN",
    "RPAREN",
    "LBRACE",
    "RBRACE",
    "COLON",
    "SEMI",
    "COMMA",
    "ARROW",
    "DOT",
)

t_DOT = r'\.'
t_ARROW = r'<'
t_COMMA = r','
t_SEMI = r';'
t_COLON = r':'
t_LBRACE = r'{'
t_RBRACE = r'}'
t_PLUSSSS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\('

def t_COMMENT(t):
    r'--.*[\r|\n]|\(\*.*\*\)'
    t.lexer.lineno += len(re.findall(r'\r|\n', t.value))

def t_NUMBER(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_newline(t):
    r'(\n|\r)+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

lexer = lex.lex()

data = open(sys.argv[1], 'r').read()

lexer.input(data)

while True:
    tok = lexer.token()
    if not tok: break
    print tok.lineno
    print tok.type
    print tok.value
