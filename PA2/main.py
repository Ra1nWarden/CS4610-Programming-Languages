# Zihao Wang
# zw2rf@virginia.edu
# Feb 2, 2014

import lex as lex
import re
import sys

# Tokens for the lexer, all symbols are separate types
tokens = (
    'keyword',
    'identifier',
    'integer',
    'type',
    'lbrace',
    'rbrace',
    'lparen',
    'rparen',
    'lt',
    'le',
    'equals',
    'plus',
    'minus',
    'times',
    'divide',
    'tilde',
    'semi',
    'string',
    'colon',
    'comma',
    'larrow',
    'rarrow',
    'dot',
    'at',
)

# A keyword list for cool. True and False constants are separated for special handling.
keyword_list = ["class", "else", "fi", "if", "in", "inherits", "isvoid", "let", "loop", "pool", "then", "while", "case", "esac", "new", "of", "not"]
true_false = ["true", "false"]

# Definition of all the symbols
t_at = r'@'
t_tilde = r'~'
t_plus = r'\+'
t_minus = r'-'
t_times = r'\*'
t_divide = r'/'
t_lbrace = r'{'
t_rbrace = r'}'
t_lparen = r'\('
t_rparen = r'\)'
t_semi = r';'
t_colon = r':'
t_comma = r','
t_larrow = r'<-'
t_rarrow = r'=>'
t_dot = r'\.'
t_lt = r'<'
t_le = r'<='
t_equals = r'='

# States of the lexer, two different states other than initial state
states = (
    # Comment state to handle nested comments
    ('multicomment', 'exclusive'),
    # String state to handle escaped character
    ('stringprocess', 'exclusive'),
)

# Two global variables to indicate errors found in Comment or String state
EOF_comment = False
string_mode = False



'''
Multicomment state to handle comments
'''

# Start of comment mode: r'"' This makes the lexer into Comment state.
def t_multicomment(t):
    r'\(\*'
    # level is another property of the lexer to indicate the level of comment
    t.lexer.level = 1
    # Change the global variable to indicate that the lexer is in Comment state now
    global EOF_comment
    EOF_comment = True
    t.lexer.begin('multicomment')

# Another open parenthesis inside the Comment state. This increases the level of the comment
def t_multicomment_lparen(t):
    r'\(\*'
    t.lexer.level += 1

# A close parenthesis inside the Comment state to decrease the level of the comment.
def t_multicomment_rparen(t):
    r'\*\)'
    t.lexer.level -= 1
    # When the level reaches 0, the lexer falls back into the normal mode
    if t.lexer.level == 0:
        # Change the global variable to indicate that the lexer is not in Comment state any more.
        global EOF_comment
        EOF_comment = False
        t.lexer.begin('INITIAL')

# A type inside the Comment state to cound newline characters and handle line numbers
def t_multicomment_newline(t):
    r'\n+'
    t.lexer.lineno += len(re.findall(r'\n', t.value))

# A type inside the Comment state to match content of the comments
def t_multicomment_content(t):
    r'[^\Z\n]+?'
    pass

# An error type inside Comment state
def t_multicomment_error(t):
    print "ERROR: %d: Lexer: EOF inside comment!"
    sys.exit(0)

# Ignored characters inside Comment state
t_multicomment_ignore = ' \t\r\f\v'



# A comment type that handles one-line comments
def t_COMMENT(t):
    r'--.*'
    t.lexer.lineno += len(re.findall(r'\n', t.value))

# Newline characters are counted to handle lexer line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(re.findall(r'\n', t.value))



'''
String state
'''

# Integer type inside normal state
def t_integer(t):
    r'[0-9]+'
    number = int(t.value)
    if number > 2147483647:
        print "ERROR: %d: Lexer: integer out of range" % t.lexer.lineno
        sys.exit(0)
    # Remove leading 0s and convert into strings
    t.value = str(number)
    return t

# A type that makes the lexer into String state.
def t_string(t):
    r'"'
    t.lexer.string_start = t.lexer.lexpos
    # Change the global variable to indicate that the lexer is in String state now
    global string_mode
    string_mode = True
    t.lexer.begin('stringprocess')

# Escaped character \ in String state to ignore the next character after \
def t_stringprocess_escape(t):
    r'\\[^\x00\n]'
    pass

# A single " character in String state that exits the state
def t_stringprocess_end(t):
    r'"'
    t.value = t.lexer.lexdata[t.lexer.string_start:t.lexer.lexpos-1]
    # Count the length of the string matched, if it is longer than 1024 characters, reject it
    if len(t.value) > 1024:
        print "ERROR: %d: Lexer: String is too long!" % t.lexer.lineno
        sys.exit(0)
    t.type = "string"
    # Change the global variable to indicate that the lexer is not in String state any more
    global string_mode
    string_mode = False
    t.lexer.begin('INITIAL')
    return t

# Process the content of the string which matches any character except newline and null character
def t_stringprocess_content(t):
    r'[^\x00\n]+?'
    pass

# Error type in String state
def t_stringprocess_error(t):
    print "ERROR: %d: Lexer: Invalid string." % t.lexer.lineno
    sys.exit(0)

# Nothing to be ignored in String state
t_stringprocess_ignore = ''



# Identifier type, matches any string that starts with upper or lower case alphabets
def t_identifier(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    matched = t.value
    # Test whether it is one of the keywords
    if matched.lower() in keyword_list:
        t.type = 'keyword'
        t.value = matched.lower()
        return t
    # Check whether it is true or false
    elif matched.lower() in true_false:
        # If it starts with lower case letters, it is a true or false character
        if matched[0] == 't':
            t.type = 'keyword'
            t.value = 'true'
            return t
        elif matched[0] == 'f':
            t.type = 'keyword'
            t.value = 'false'
            return t
        # If it starts with an upper case letter, it is a type
        elif matched[0] == 'T' or matched[0] == 'F':
            t.type = 'type'
            return t
    elif matched[0] >= 'A' and matched[0] <= 'Z':
        t.type = 'type'
        return t
    # Otherwise, it is an identifier type
    else:
        t.type = 'identifier'
        return t

# Erros for the normal state
def t_error(t):
    print "ERROR: %d: Lexer: invalid character: %s" % (t.lexer.lineno, t.value[0])
    sys.exit(0)

# Characters to be ignored in the normal state. These are the whitespaces in Cool's manual
t_ignore = ' \t\r\f\v'



'''
Build the lexer and run!
'''

# Building the lexer
lexer = lex.lex()

# List of tokens read
read_token = [] 

# Read the file name from command line argument
file_name = sys.argv[1]

# Form the output file name
output_file = file_name + '-lex'

# Reading from .cl file
data = open(file_name, 'r').read()

lexer.input(data)

# Processing tokens
while True:
    tok = lexer.token()
    if not tok: break
    read_token.append(tok)

# If the global variable indicates that it is still in Comment state, reject it
if EOF_comment:
    print "ERROR: %d: Lexer: EOF inside comment!" % lexer.lineno
    sys.exit(0)

# If the global variable indicates that it is still in String state, reject it
if string_mode:
    print "ERROR: %d: Lexer: Invalid string!" % lexer.lineno
    sys.exit(0)


# If things are good, start creating the file
output_stream = open(output_file, 'w')

# Printing the tokens
for tok in read_token:
    if tok.type == 'keyword':
        output_stream.write(str(tok.lineno))
        output_stream.write('\n')
        output_stream.write(tok.value)
        output_stream.write('\n')
    elif tok.type in ['string', 'type', 'identifier', 'integer']:
        output_stream.write(str(tok.lineno))
        output_stream.write('\n')
        output_stream.write(tok.type)
        output_stream.write('\n')
        output_stream.write(tok.value)
        output_stream.write('\n')
    else:
        output_stream.write(str(tok.lineno))
        output_stream.write('\n')
        output_stream.write(tok.type)
        output_stream.write('\n')

# Close the output stream
output_stream.close()
