#######################################################
#
# Earley Parser (documented classroom version)
# Westley Weimer
#
# See: http://en.wikipedia.org/wiki/Earley_parser
#
#######################################################

# Key Terminology: 
#
#       terminal   
#               Also called a "token", a terminal is an atomic lexical
#               unit. "int" and "lparen" are examples. We use the strings
#               to represent terminals. 
#
#       non-terminal
#               An abstract part of our grammar such as "expression" or
#               "statement". A non- terminal typically derives (i.e., can
#               be rewritten to, corresponds to, etc.) zero or more
#               terminals. We use strings to represent non-terminals. (You
#               are responsible for ensuring that the terminals and
#               non-terminals are distinct.)
#
#       production
#               Also called a "rewrite rule", a grammar production has the
#               form "X -> abcd" where X is a non-terminal and abcd is a
#               possibly- empty sequence of terminals and/or non-
#               terminals. We use pairs to represent rewrite rules: the
#               first element is the non-terminal and the second element is
#               the list of terminals and/or non-terminals.  For example,
#               "E -> E + E" is ( "E", [ "E", "+", "E" ] ) while "E ->
#               \epsilon" is ( "E", [ ] ). 
#
#       grammar
#               Our grammar is a list of rewrite rules. We use a list to
#               represent the grammar. The 0th rule in grammar is the
#               starting rule. The non-terminal on the left of that rule is
#               the "start symbol". 
#
#       state   
#               A parse state "X -> ab . cd, from j" means: We are in
#               the middle of parsing non-terminal X. Intuitively, the dot
#               (period) marks where we are now in our incremental parse.
#               We have already seen tokens derivable from ab. We expect to
#               see tokens derivable from cd. We started parsing this X
#               after having seen j input tokens. ab and cd are both
#               possibly- empty sequences of terminals and/or non-
#               terminals. "X -> abcd" is a production in the grammar. We
#               use four-tuples to represent parse states. For example, "X
#               -> ab . cd, from j" is (X,ab,cd,j). (Recall that X is a
#               string, ab and cd are lists, and j is a natural number.) 
#               
#       chart
#               The parsing table. Earley's Algorithm is a chart-parsing
#               algorithm. The chart is a one-dimensional array indexed by
#               "number of input tokens seen so far". Each chart array
#               element is a _set_ of parse states. chart[i] holds possible
#               parse states that could correspond to valid parsing
#               configurations after we have seen the first i tokens. 
#               For example, chart[0] holds the set of parsing states
#               possible when we have seen no input (= 0 tokens). We use an
#               array to implement the chart. We use a list to implement
#               each chart[i] (we are careful to avoid inserting duplicate
#               elements when using a list to implement a set.) 

# Earley's Algorithm uses dynamic programming to fill in the chart. 
#
# Three key operations are used: shift, closure, and reduction.
# 
# Each operation starts with chart[i] containing "X -> ab.cd, from j". 
# Each operation returns a list of chart states, as well as where
# to put those new chart states. 

### SHIFT (also called SCAN)
# If chart[i] contains "X -> ab.cd , from j":
#       and cd is not empty
#       and c is token[i]
# Then add:
#       "X -> abc.d , from j" 
#       to chart[i+1] 
# Informally: We're looking for to parse token c next and the current token
# is exactly c! Aren't we lucky! So we can parse over it and move to i+1.
def shift(tokens, i, x, ab, cd, j): 
  if cd <> [] and tokens[i] == cd[0]:
    c = cd[0]
    d = cd[1:] 
    abc = ab + [c] 
    new_chart_state = (x, abc, d, j) 
    new_chart_index = i + 1
    return [ (new_chart_index, new_chart_state) ] 
  else:
    return []

### CLOSURE (also called PREDICT)  
# If chart[i] contains "X -> ab.cd , from j":
#       and cd is not empty
#       and c is a non-terminal
#       and there is a grammar rule "c -> pqr" 
# Then add:
#       "c -> . pqr , from i" 
#       to chart[i] 
# Informally: 
#       We're about to start parsing a "c", but "c" may be something like
#       "exp" with its own production rules. We'll bring those production
#       rules in.  
def closure(grammar,i,x,ab,cd,j): 
  return [ (i , (rule[0],[],rule[1],i)) \
        for rule in grammar if cd <> [] and cd[0] == rule[0] ] 

### REDUCTION (also called COMPLETION)
# If chart[i] contains "X -> ab. , from j"
#       (that is: cd is empty)
#       and chart[j] contains "Y -> pq.Xr , from k" 
# Then add
#       "Y -> pqX.r , from k" 
#       to chart[i]
# Informally: We just finished parsing an "X" with this token, but that may
# have been a sub-step (like matching "exp -> 2" in "2+3"). We should
# update the higher-level rules as well.  
def reduction(chart,i,x,ab,cd,j): 
  return [ (i, (jstate[0], jstate[1] + [x], (jstate[2])[1:], jstate[3] )) \
    for jstate in chart[j] 
    if cd == [] and jstate[2] <> [] and (jstate[2])[0] == x ]

###
# We compute shift, closure and reduction. This produces new chart states.
# We repeat this until new no states are added to the chart. The
# add_to_chart function returns 'True' if any updates were actually made
# to the chart and returns 'False' otherwise. (Recall that chart[i]
# is a set, and sets do not consider duplicate entries.) 
def add_to_chart(chart, list_of_index_state_pairs):
  if list_of_index_state_pairs == []: 
    return False
  else:
    new_chart_index, new_chart_state = list_of_index_state_pairs[0] 
    if not (new_chart_state in chart[new_chart_index]):
      chart[new_chart_index] = chart[new_chart_index] + [new_chart_state] 
      return add_to_chart(chart, list_of_index_state_pairs[1:]) or True
    else:
      return add_to_chart(chart, list_of_index_state_pairs[1:]) 

### EARLEY_PARSE
# This is our main parsing function. Given a sequence of tokens and
# a grammar, return True if that sequence of tokens is in the language
# of the grammar.
def earley_parse(tokens, grammar): 

  # Step 1: Initialization

  # We append an EOF marker to the tokens to prevent us from walking
  # off the end of the token array.
  tokens = tokens + [ "end_of_input_marker" ] 

  # By convention, the starting rule is the first rule in the grammar.
  start_rule = grammar[0] 

  # The starting parse state is "S -> . abcd , from 0" 
  start_state = (start_rule[0], [], start_rule[1], 0) 

  # The parsing chart is a one-dimensional array, initially empty.
  chart = {}  
  for i in range(len(tokens)+1):
    chart[i] = [ ] 

  # We start parsing by placing the starting state in chart[0]. 
  chart[0] = [ start_state ]

  # Step 2: Dynamic Programming
  for i in range(len(tokens)):
    # Dynamic Programming: fill in chart[i] using the values of chart[0]
    # ... chart[i]. 

    # We repeatedly apply shift, closure and reduction until no new parsing
    # states are added to the chart. 
    def apply_shift_closure_reduction():
      if any([ 
          add_to_chart(chart, 
            shift(tokens,i,x,ab,cd,j) + 
            closure(grammar,i,x,ab,cd,j) + 
            reduction(chart,i,x,ab,cd,j)) 
            for x, ab, cd, j in chart[i] ]):
        apply_shift_closure_reduction() # do it again if any changes

    apply_shift_closure_reduction() 

  # We're done building the chart. 

  # Debugging: We print out the resulting chart.
  for i in range(len(tokens)):
    print "chart[%d]" % i
    for x,ab,cd,j in chart[i]:
      print "  %s -> %-16s , from %d" % (x, \
        ( (' '.join(ab)) + " . " + (' '.join(cd)) ), j) 

  # Step 3: Determine actual answer. We accept the string if the completed
  # starting rule is in the final accepting state. 
  accepting_state = (start_rule[0], start_rule[1], [], 0) 
  return accepting_state in chart[len(tokens)-1]

# Simple testing. This grammar is the one from the "Massive Earley Example"
# in the slides and on the handout.

grammar = [
  ("S", ["F"]), 
  ("F", ["id", "(", "A", ")"]), 
  ("A", [ ] ), 
  ("A", ["N"] ), 
  ("N", ["id", ]),
  ("N", ["id", ",", "N" ]),
]
tokens = [ "id" , "(" , "id", "," , "id", ")" ] 

print "\nString Accepted: " + str(earley_parse(tokens, grammar))

# Other example grammars and strings for testing ...

grammar1 = [
  ("P", ["S"]),
  ("S", ["S","+","M"]),
  ("S", ["M"]),
  ("M", ["M","*","T"]),
  ("M", ["T"]),
  ("T", ["2"]),
  ("T", ["3"]),
  ("T", ["4"]),
] 
tokens1 = [ "2", "+", "3", "*", "4" ] 

grammar2 = [
  ("S", ["P" ]) , 
  ("P", ["(" , "P", ")" ]),
  ("P", [ ]) , 
] 
tokens2 = [ "(", "(", ")", ")" ] 


grammar3 = [
  ("S", ["E"]), 
  ("E", ["E", "-", "E" ]),
  ("E", ["E", "+", "E" ]),
  ("E", ["(", "E", ")" ]),
  ("E", ["int"]),
]
tokens3 = [ "int", "-", "int", "+", "int" ] 

grammar4 = [
  ("S", ["A"]), 
  ("A", ["int", "+", "string" ]),
  ("A", ["int"]),
]
tokens4 = [ "int", "+", "int" ] 

grammar5 = [
  ("S", ["T"]),
  ("T", ["a","B","c"]),
  ("B", ["b","b"]),
]
tokens5 = ["a","b","b","c"] 

grammar6 = [
  ("S", ["T"]),
  ("T", ["p","Q","r"]),
  ("T", ["p","Q","s"]),
  ("Q", ["q"]),
]
tokens6 = ["p","q","r"] 

grammar7 = [
  ("S", ["id", "(", "OPTARGS", ")"]),
  ("OPTARGS", []),
  ("OPTARGS", ["ARGS"]), 
  ("ARGS", ["exp", ",", "ARGS"]), 
  ("ARGS", ["exp"]), 
]
tokens7 = ["id","(","exp", ",", "exp", ")"] 

